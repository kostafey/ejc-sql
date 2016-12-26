;;; ejc-autocomplete.el -- SQL completitions at point (the part of ejc-sql).

;;; Copyright © 2013-2016 - Kostafey <kostafey@gmail.com>

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

(require 'dash)
(require 'auto-complete)
(require 'ejc-lib)

(defun ejc-get-prefix-word ()
  "Return the word preceding dot before the typing."
  (save-excursion
    (let ((space-dist (or (save-excursion
                            (re-search-backward "[ \n\t\r]+" nil t))
                          0))
          (dot (search-backward "." nil t))
          (space (re-search-backward "[ \n\t\r.]+" nil t)))
      (if (and dot
               space
               (> dot space)
               (<= space-dist space)) ; is a dot completition
          (buffer-substring (1+ space) dot)
        nil))))

(defvar ejc-ansi-sql-words
  '("select" "where" "and" "or" "from" "insert" "update" "delete" "join"
    "order" "by" "distinct" "create" "alter" "drop"
    "grant" "revoke" "deny" "commit" "rollback" "savepoint"))

(defvar ejc-auxulary-sql-words
  '("show" "errors" "desc" "count" "type" "table" "function" "procedure"
    "begin" "end" "for" "return"))

(defun ejc-get-ansi-sql-words ()
  (append ejc-ansi-sql-words
          ejc-auxulary-sql-words
          (mapcar 'upcase ejc-ansi-sql-words)
          (mapcar 'upcase ejc-auxulary-sql-words)))

(defun ejc-string-to-boolean (s)
  (not (equal s "nil")))

;;;###autoload
(defun ejc-candidates ()
  "Possible completions list according to already typed prefixes."
  (let* ((prefix-1 (ejc-get-prefix-word))
         (prefix-2 (save-excursion
                     (search-backward "." nil t)
                     (ejc-get-prefix-word)))
         (result (ejc-get-stucture ejc-db prefix-1 prefix-2))
         (pending (car result))
         (candidates-cache (cdr result)))
    (if (ejc-string-to-boolean pending)
        (message "Receiving database structure (%s)..." pending))
    (if (and (not prefix-1) (not prefix-2))
        (append candidates-cache (ejc-get-ansi-sql-words))
      candidates-cache)))

(defun ejc-return-point ()
  "Return point position if point (cursor) is located next to dot char (.#)"
  (let ((curr-char (buffer-substring
                    (save-excursion
                      (left-char 1)
                      (point))
                    (point))))
    (if (equal curr-char ".")
        (point)
      nil)))

(defvar ac-source-ejc-sql
  '((candidates . ejc-candidates)
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-sql-point
  '((candidates . ejc-candidates)
    (prefix . ejc-return-point)
    (requires . 0)
    (cache . t)))

;;;###autoload
(defun ejc-ac-setup ()
  "Add the completion sources to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ejc-sql)
  (add-to-list 'ac-sources 'ac-source-ejc-sql-point ))

(provide 'ejc-autocomplete)

;;; ejc-autocomplete.el ends here
