;;; ejc-format.el -- SQL formatting library (the part of ejc-sql).

;;; Copyright © 2012, 2013 - Kostafey <kostafey@gmail.com>

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

(require 'cua-base)
(require 'clomacs-lib)

(defvar ejc-sql-separator "/"
  "The char with purpose to separate the SQL statement both other.")

(font-lock-add-keywords
 'sql-mode '(("/" 0 'font-lock-function-name-face t)))

(defun ejc-get-sql-boundaries-at-point ()
  "Returns list of the boundaries of the current SQL expression.
The current SQL expression is the expression under the point.
The boundaries are marked by `ejc-sql-separator's. If the top or
bottom boundary is absent - it returns beginning or end of the
buffer."
  (save-excursion
    (let* ((beg (progn
                  (if (search-backward ejc-sql-separator nil t nil)
                      (forward-char)
                    (beginning-of-buffer))
                  (point)))
           (end (progn
                  (if (search-forward ejc-sql-separator nil t nil)
                      (backward-char)
                    (end-of-buffer))
                  (point))))
      (list beg end))))

(defmacro ejc--in-sql-boundaries (beg end &rest body)
  "Inject `beg' and `end' local variables to the `body' scope.
`beg' and `end' are the boundaries of the current sql expression."
  `(let* ((boundaries (ejc-get-sql-boundaries-at-point))
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

(defun ejc-apply-in-sql-boundaries (func)
  (ejc--in-sql-boundaries beg end
   (apply func (list beg end))))

(defun ejc-get-sql-at-point ()
  "Return SQL around the point."
  (ejc--in-sql-boundaries beg end
   (let ((sql (clomacs-strip-text-properties (buffer-substring beg end))))
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
                (replace-string (car from-to) (cadr from-to) nil beg end))))
           '((","            ",\n    ")
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
  (ejc--in-sql-boundaries beg end
   (ejc-format-sql beg end)))

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
  (equal (trim-string (buffer-substring
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
         (insert-string "\"")
         (end-of-line)
         (dotimes (counter (+ 2 (- length-line (current-column))))
           (insert-string " "))
         (if (equal curr-line end-line)
             (insert-string "\\n\";")
           (insert-string "\\n\" +"))
         (setq curr-line (1+ curr-line)))))))

(provide 'ejc-format)
