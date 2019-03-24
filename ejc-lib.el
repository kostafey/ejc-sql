;;; ejc-lib.el -- ejc-sql shared objects (the part of ejc-sql).

;;; Copyright © 2013-2019 - Kostafey <kostafey@gmail.com>

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

(defvar-local ejc-connection-name nil
  "Buffer-local connection name created with `ejc-create-connection'.")

(defvar-local ejc-connection-struct nil
  "Buffer-local connection structure.")

(defun ejc-string-endswith-p (s ending)
  "Return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun ejc-find-file-in-load-path (search-file-name &optional fail-on-error)
  "Return the full path to `file-name'.
`file-name' is searching in the emacs `load-path'."
  (let ((result nil))
    (dolist (path load-path)
      (let ((search-file-path (expand-file-name search-file-name path)))
        (if (file-exists-p search-file-path)
            (setq result search-file-path))))
    (if (and fail-on-error (not result))
        (error (concat "Can't find file " search-file-name))
      result)))

(defun ejc-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defvar ejc-product-assoc
  '((sqlserver . ms)
    (oracle:sid . oracle)))

(defun ejc-get-product-name (db)
  "Get database type from db connection info."
  (let ((product-name (or (alist-get :subprotocol db)
                          (alist-get :dbtype db))))
    (or (cdr (assoc-string product-name ejc-product-assoc))
        (car (assoc-string product-name sql-product-alist))
        'ansi)))

(defun ejc-ensure-file-directory-exists (file-path)
  (let ((dir (file-name-directory file-path)))
    (if (not (file-accessible-directory-p dir))
        (make-directory dir))))

(defun ejc-save-to-file (file-path data)
  "Save Elisp variable DATA to FILE-PATH."
  (ejc-ensure-file-directory-exists file-path)
  (with-temp-file file-path
    (prin1 data (current-buffer))))

(cl-defun ejc-load-from-file (file-path &key default check)
  "Read Elisp variable from FILE-PATH."
  (ejc-ensure-file-directory-exists file-path)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (let ((value (read (current-buffer))))
          (if (and check (funcall check value))
              value
            (error "File contents don't match function check."))))
    (error (ejc-save-to-file file-path default)
           default)))

(defun ejc-plist-p (lst)
  "Check if LST is a plist."
  (condition-case nil
      (progn
        (lax-plist-get lst nil)
        t)
    (error nil)))

(provide 'ejc-lib)

;;; ejc-lib.el ends here
