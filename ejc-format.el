;;; ejc-format.el -- SQL formatting library (the part of ejc-sql).

;;; Copyright © 2012-2018 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Code:

(require 'cua-base)
(require 'ejc-lib)
(require 'subr-x)

(defvar ejc-sql-separator "/"
  "The char with purpose to separate the SQL statement both other.")

(defun ejc-sql-separator-re ()
  (format "^\\s-*%s\\s-*" ejc-sql-separator))

(defun ejc-get-border-top ()
  "Get top position of batch statement(s) seperator `ejc-sql-separator'.
Upper position of this batch statement(s)."
  (save-excursion
    (if (re-search-backward (ejc-sql-separator-re) nil t nil)
        (forward-char)
      (beginning-of-buffer))
    (point)))

(defun ejc-get-border-bottom ()
  "Get bottom position of batch statements seperator `ejc-sql-separator'
Bottom position of this batch statement(s)."
  (save-excursion
    (if (re-search-forward (ejc-sql-separator-re) nil t nil)
        (backward-char)
      (end-of-buffer))
    (if (equal (string (preceding-char)) "\n")
        (backward-char 1))
    (let ((sep-len (length ejc-sql-separator)))
      (if (equal (buffer-substring (- (point) sep-len) (point))
                 ejc-sql-separator)
          (backward-char sep-len)))
    (point)))

(defun ejc-get-org-begin ()
  "Obtain position for begin of code snippet in `org-mode' buffers."
  (save-excursion
    (if (and (derived-mode-p 'org-mode)
             (re-search-backward "^\\s-*#\\+begin_[(src)(example)].*"
                                 nil t nil))
        (line-end-position)
      (point-min))))

(defun ejc-get-org-end ()
  "Obtain position for end of code snippet in `org-mode' buffers."
  (save-excursion
    (if (and (derived-mode-p 'org-mode)
             (re-search-forward "^\\s-*#\\+end_[(src)(example)]"
                                nil t nil))
        (line-beginning-position)
      (point-max))))

(cl-defun ejc-get-sql-boundaries-at-point (&optional beg end)
  "Returns list of the boundaries of the current SQL expression.
The current SQL expression is the expression under the point.
The boundaries are marked by `ejc-sql-separator's. If the top or
bottom boundary is absent - it returns beginning or end of the
buffer. Set BEG and END parameters to add manual boundaries restrictions."
  (let* ((beg (max (or beg (point-min))
                   (ejc-get-border-top)
                   (ejc-get-org-begin)))
         (end (min (or end (point-max))
                   (ejc-get-border-bottom)
                   (ejc-get-org-end))))
    (list beg end)))

(defmacro ejc--in-sql-boundaries (beg end &rest body)
  "Inject `beg' and `end' local variables to the `body' scope.
`beg' and `end' are the boundaries of the current sql expression."
  `(let* ((boundaries (ejc-get-sql-boundaries-at-point ,beg ,end))
          (,beg (car boundaries))
          (,end (car (cdr boundaries))))
     ,@body))

(defun ejc-mark-this-sql ()
  "Select (mark) SQL around the point."
  (interactive)
  (ejc--in-sql-boundaries beg end
   (when mark-active
     (setq mark-active nil))
   (goto-char beg)
   (cua-set-mark)
   (goto-char end)))

(defun ejc-next-sql (&optional mark)
  "Goto next SQL statement."
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (if (and mark (not mark-active))
       (cua-set-mark))
   (goto-char end)
   (right-char 1)))

(defun ejc-previous-sql (&optional mark)
  "Goto previous SQL statement."
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (if (and mark (not mark-active))
       (cua-set-mark))
   (goto-char beg)
   (left-char 1)))

(defun ejc-apply-in-sql-boundaries (func)
  (ejc--in-sql-boundaries beg end
   (apply func (list beg end))))

(cl-defun ejc-get-sql-at-point (&key beg end)
  "Return SQL around the point."
  (ejc--in-sql-boundaries
   beg end
   (let ((sql (ejc-strip-text-properties
               (buffer-substring beg end))))
     sql)))

(defmacro ejc-ensure-sql-mode (&rest body)
  `(if (not (equal major-mode 'sql-mode))
       (error "SQL formatting is suitable in sql-mode only.")
     (progn ,@body)))

(defun ejc-format-sql (beg end)
  (interactive "r")
  (ejc-ensure-sql-mode
   (save-excursion
     (mapc (lambda (from-to)
             (ejc-apply-in-sql-boundaries
              (lambda (beg end)
                (replace-regexp (car from-to) (cadr from-to) nil beg end))))
           '(("\n"           " ")
             (","            ", ")
             (" +"           " ")
             (","            ",\n    ")
             ("select"       "select \n    ")
             (" from "       "\nfrom \n     ")
             (" where "      "\nwhere \n     ")
             (" and "        "\n and ")
             (" or "         "\n  or ")
             (" order by "   "\norder by \n")
             (" inner join " "\ninner join ")
             (" left join "  "\nleft join ")
             (" on "         "\n  on ")
             (" group by "   "\ngroup by "))))))

(defun ejc-format-sql-at-point ()
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (ejc-format-sql beg end)))

(defun ejc-pretty-print-sql-at-point ()
  "Pretty-print SQL bounded by the `ejc-sql-separator' or/and buffer
boundaries."
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (let ((result (ejc-pretty-print (buffer-substring beg end) :hibernate)))
     (delete-region beg end)
     (insert result))))

(defun ejc-pretty-print-sql-region (beg end)
  "Pretty-print SQL bounded by the selection area."
  (interactive "r")
  (let ((result (ejc-pretty-print (buffer-substring beg end) :hibernate)))
    (delete-region beg end)
    (insert result)))

(defun ejc-insert-file-header ()
  (interactive)
  (insert (concat "-- -*- mode: sql; -*-\n"
                  "-- Local Variables:\n"
                  "-- eval: (ejc-sql-mode)\n"
                  "-- End:\n")))

(defvar ejc-clear-sql-regexp
  (concat "^\\s-*\\t*\""
          "\\|\\\\n\"\s-*\\+$"
          "\\|\\\\n\";$"))

(defun ejc-strinp-sql-at-point ()
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (save-excursion
     (replace-regexp ejc-clear-sql-regexp "" nil beg end)
     (whitespace-cleanup-region beg end))))

(defun ejc-longest-line-length (beg-line end-line)
  (interactive)
  (save-excursion
    (let ((curr-line beg-line)
          (max-length 0)
          (new-length 0))
      (while (<= curr-line end-line)
        (goto-line curr-line)
        (setq new-length (save-excursion
                           (end-of-line)
                           (current-column)))
        (if (> new-length max-length)
            (setq max-length new-length))
        (setq curr-line (1+ curr-line)))
      max-length)))

(defun ejc-is-separator-string (pos)
  (equal (string-trim (buffer-substring
                       pos
                       (save-excursion
                         (end-of-line)
                         (point))))
         ejc-sql-separator))

(defun ejc-dress-sql-at-point ()
  (interactive)
  (ejc-strinp-sql-at-point)
  (save-excursion
    (ejc--in-sql-boundaries
     beg end
     (let* ((beg-line (progn (goto-char beg)
                             (right-char)
                             (line-number-at-pos)))
            (end-line (progn (goto-char end)
                             (if (ejc-is-separator-string end)
                                 (left-char 1))
                             (line-number-at-pos)))
            (length-line (ejc-longest-line-length beg-line end-line))
            (curr-line beg-line))
       (while (<= curr-line end-line)
         (goto-line curr-line)
         (beginning-of-line)
         (insert "\"")
         (end-of-line)
         (dotimes (counter (+ 2 (- length-line (current-column))))
           (insert " "))
         (if (equal curr-line end-line)
             (insert "\\n\";")
           (insert "\\n\" +"))
         (setq curr-line (1+ curr-line)))))))

(defun ejc-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(cl-defun ejc-flash-this-sql (&key beg end)
  "Select (mark) SQL around the point."
  (interactive)
  (ejc--in-sql-boundaries
   beg end
   (ejc-flash-region beg end)))

(provide 'ejc-format)

;;; ejc-format.el ends here
