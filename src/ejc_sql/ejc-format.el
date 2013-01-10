;;; ejc-format.el -- SQL formatting library (the part of ejc-sql).

;;; Copyright © 2012 - Kostafey <kostafey@gmail.com>

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

(defvar ejc-sql-separator "/"
  "The char with purpose to separate the SQL statement both other.")

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

(defmacro ejc--in-sql-boundaries (&rest body)
  "Inject `beg' and `end' local variables to the `body' scope.
`beg' and `end' are the boundaries of the current sql expression."
  `(let* ((boundaries (ejc-get-sql-boundaries-at-point))
          (beg (car boundaries))
          (end (car (cdr boundaries))))
     ,@body))

(defun ejc-mark-this-sql ()
  "Select (mark) SQL around the point."
  (interactive)
  (ejc--in-sql-boundaries
   (when mark-active
     (setq mark-active nil))
   (goto-char beg)
   (cua-set-mark)
   (goto-char end)))

(defun ejc-apply-in-sql-boundaries (func)
  (ejc--in-sql-boundaries
   (apply func (list beg end))))

(defun ejc-get-sql-at-point ()
  "Return SQL around the point."
  (ejc--in-sql-boundaries
   (let ((sql (buffer-substring beg end)))
     sql)))

(defun ejc-format-sql (beg end)
  (interactive "r")
  (save-excursion
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end) 
        (replace-string "," ",\n    " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string "select" "select \n    " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " from " "\nfrom \n     " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " where " "\nwhere \n     " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " and " "\n and " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " or " "\n  or " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " order by " "\norder by \n" nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " inner join " "\ninner join " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " left join " "\nleft join " nil beg end)))
    (ejc-apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " on " "\n  on " nil beg end)))
    ))

(defun ejc-format-sql-at-point ()
  (interactive)  
  (ejc--in-sql-boundaries
   (ejc-format-sql beg end)))

(provide 'ejc-format)

