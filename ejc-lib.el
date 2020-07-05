;;; ejc-lib.el -- ejc-sql shared objects (the part of ejc-sql).

;;; Copyright © 2013-2020 - Kostafey <kostafey@gmail.com>

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

(require 's)
(require 'dash)

(defvar-local ejc-connection-name nil
  "Buffer-local connection name created with `ejc-create-connection'.")

(defvar-local ejc-connection-struct nil
  "Buffer-local connection structure.")

(defun ejc-lein-artifact-to-path (artifact)
  "Get ~/.m2 jar file path of artifact."
  (if (not (vectorp artifact))
      (error
       "Expect leiningen artifact, e.g. [com.h2database/h2 \"1.4.199\"]."))
  (let* ((group (car (split-string
                      (ejc-strip-text-properties (symbol-name (elt artifact 0)))
                      "/")))
         (name (or (cadr (split-string
                          (ejc-strip-text-properties
                           (symbol-name (elt artifact 0))) "/"))
                   group))
         (version (elt artifact 1)))
    (format "~/.m2/repository/%s/%s/%s/%s-%s.jar"
            (mapconcat 'identity (split-string group "\\.") "/")
            name
            version
            name
            version)))

(defun ejc-path-to-lein-artifact (path)
  "Get leiningen artifact from ~/.m2 jar file path."
  (if (not (stringp path))
      (error
       "Expect jar file path."))
  (let* ((path (s-replace "\\" "/" path))
         (path (nth 1 (s-split ".m2/repository/" path)))
         (path-elements (s-split "/" path))
         (version (nth (- (length path-elements) 2) path-elements))
         (name (nth (- (length path-elements) 3) path-elements))
         (group (s-join "." (-> path-elements -butlast -butlast -butlast))))
    (read
     (format "[[%s/%s \"%s\"]]"
             group
             name
             version))))

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

(defun ejc-flatten-index (imenu-index)
  "Flatten imenu index into a plain list.
IMENU-INDEX - imenu index tree."
  (-mapcat
   (lambda (x)
     (if (imenu--subalist-p x)
         (mapcar (lambda (y) (cons (car y) (cdr y)))
                 (ejc-flatten-index (cdr x)))
       (list x)))
   imenu-index))

(defun ejc-split-and-join (fn split &optional join)
  "Split string by SPLIT, apply FN to each substring, than join by JOIN."
  (let ((join (or join split)))
    (string-join
     (-map fn (split-string (car p) split))
     join)))

(defun ejc-not-nil-str (s)
  (not (equal s "nil")))

(defun ejc-append-without-duplicates (list-left list-right comparator lead)
  "Unite LIST-LEFT and LIST-RIGHT without duplicates items.
Check items equality by COMPARATOR function.
In case if some item presents in both lists (in terms of COMPARATOR
equality) get the item from LIST-LEFT if LEAD param is `:left'or
get it from LIST-RIGHT if LEAD param is `:right'."
  (let* ((a (if (eq lead :left) list-left list-right))
         (b (if (eq lead :left) list-right list-left))
         (a-results (-map comparator a)))
    (append a
            (-filter
             (lambda (b-item)
               (not (-contains-p a-results
                                 (funcall comparator b-item))))
             b))))

(provide 'ejc-lib)

;;; ejc-lib.el ends here
