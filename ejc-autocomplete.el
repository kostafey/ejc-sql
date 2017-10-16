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
(require 'ejc-interaction)
(require 'ejc-doc)
(require 'ejc-format)

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

(defun ejc-not-nil-str (s)
  (not (equal s "nil")))

(defmacro ejc-candidates (cand-fn)
  `(if (ejc-buffer-connected-p)
       (let* ((prefix-1 (ejc-get-prefix-word))
              (prefix-2 (save-excursion
                           (search-backward "." nil t)
                           (ejc-get-prefix-word)))
              (result (funcall ,cand-fn
                               ejc-db
                               (apply
                                'buffer-substring
                                (ejc-get-sql-boundaries-at-point))
                               prefix-1
                               prefix-2))
              (pending (car result))
              (candidates-cache (cdr result)))
         (if (ejc-not-nil-str pending)
             (message "Receiving database structure...")
           candidates-cache))))

;;;###autoload
(defun ejc-owners-candidates ()
  (ejc-candidates 'ejc-get-owners-candidates))

;;;###autoload
(defun ejc-tables-candidates ()
  (ejc-candidates 'ejc-get-tables-candidates))

;;;###autoload
(defun ejc-colomns-candidates ()
  (ejc-candidates 'ejc-get-colomns-candidates))

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

(defun ac-ejc-documentation (symbol-name)
  "Return a documentation string for SYMBOL-NAME."
  (if (not ejc-doc-created-p)
      (ejc-create-doc))
  (gethash (intern (downcase symbol-name)) ejc-sql-doc))

(defvar ac-source-ejc-owners
  '((candidates . ejc-owners-candidates)
    (symbol . "o")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-tables
  '((candidates . ejc-tables-candidates)
    (symbol . "t")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-tables-point
  '((candidates . ejc-tables-candidates)
    (symbol . "t")
    (prefix . ejc-return-point)
    (requires . 0)
    (cache . t)))

(defvar ac-source-ejc-colomns
  '((candidates . ejc-colomns-candidates)
    (symbol . "c")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-colomns-point
  '((candidates . ejc-colomns-candidates)
    (symbol . "c")
    (prefix . ejc-return-point)
    (requires . 0)
    (cache . t)))

(defvar ac-source-ejc-ansi-sql
  '((candidates . ejc-get-ansi-sql-words)
    (symbol . "s")
    (document . ac-ejc-documentation)
    (requires . 1)
    (cache . t)))

;;;###autoload
(defun ejc-ac-setup ()
  "Add the completion sources to the front of `ac-sources'.
This affects only the current buffer.

Check against following cases:
prefix-2.prefix-1.#
prefix-1.#
something#"
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ejc-ansi-sql)
  (add-to-list 'ac-sources 'ac-source-ejc-owners)
  (add-to-list 'ac-sources 'ac-source-ejc-tables)
  (add-to-list 'ac-sources 'ac-source-ejc-tables-point)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns-point))

(provide 'ejc-autocomplete)

;;; ejc-autocomplete.el ends here
