;;; ejc-completion-common.el -- SQL completitions common functionality (the part of ejc-sql).

;;; Copyright © 2020 - Kostafey <kostafey@gmail.com>

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

(require 'ejc-lib)
(require 'ejc-format)
(require 'ejc-interaction)
(require 'ejc-doc)

(defcustom ejc-candidates-uppercase t
  "Use uppercase candidates or downcase.
Uppercase by default, set to nil to use downcase candidates."
  :type 'boolean
  :safe #'booleanp
  :group 'ejc-sql)

(defvar ejc-ansi-sql-words
  '("select" "where" "and" "or" "from" "insert" "update" "delete" "join"
    "order by" "distinct" "create" "alter" "drop" "like"
    "grant" "revoke" "deny" "commit" "rollback" "savepoint"))

(defvar ejc-auxulary-sql-words
  '("show" "errors" "desc" "count" "type" "table" "function" "procedure"
    "begin" "end" "for" "return"))

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

(defun ejc-get-prefix-word ()
  "Return the word preceding dot before the typing."
  (save-excursion
    (let ((space-dist (or (save-excursion
                            (re-search-backward "[ \n\t\r(]+" nil t))
                          0))
          (dot (search-backward "." nil t))
          (space (re-search-backward "[ \n\t\r(.]+" nil t)))
      (if (and dot
               space
               (> dot space)
               (<= space-dist space)) ; is a dot completition
          (buffer-substring (1+ space) dot)
        nil))))

(defun ejc-get-ansi-sql-words ()
  "Get ANSI SQL keywords."
  (unless (or (ejc-return-point) (ejc-get-prefix-word))
    (if ejc-candidates-uppercase
        (append (mapcar 'upcase ejc-ansi-sql-words)
                (mapcar 'upcase ejc-auxulary-sql-words))
      (append ejc-ansi-sql-words
              ejc-auxulary-sql-words))))

(defun ejc-get-keywords ()
  "Get DB-specific keywords."
  (if (ejc-buffer-connected-p)
      (unless (or (ejc-return-point) (ejc-get-prefix-word))
        (if ejc-candidates-uppercase
            (mapcar 'upcase (ejc-get-keywords-inner ejc-db nil))
          (mapcar 'lowercase (ejc-get-keywords-inner ejc-db nil))))))

(cl-defun ejc-complete-auto-complete (buffer-name point)
  "Called by Clojure side when db structure cache creation process completes.
When the user typed some chars, the request for autocompletion is passed to
Clojure side. If Clojure side has the database structure cache, autocompletion
variants returned immediately. If not, the database structure cache creation
process starts. It's async, so the process of Emacs is not blocked and the
user can move point (cursor), edit SQL and so on. After Clojure side cache
creation process finishes, it calls this `ejc-complete-auto-complete'
function. If the user waits for autocompletion and doesn't move point
(cursor) during this process, he will get autocompletion variants."
  (switch-to-buffer buffer-name)
  (if (equal point (point))
      (cond ((bound-and-true-p auto-complete-mode) (auto-complete))
            ((bound-and-true-p company-mode) (company-complete))))
  nil)

(defmacro ejc-candidates (cand-fn)
  `(if (ejc-buffer-connected-p)
       (let* ((prefix-1 (ejc-get-prefix-word))
              (prefix-2 (save-excursion
                          (search-backward "." nil t)
                          (ejc-get-prefix-word)))
              (result (funcall ,cand-fn
                               :db ejc-db
                               :sql (apply
                                     'buffer-substring
                                     (ejc-get-sql-boundaries-at-point))
                               :prefix-1 prefix-1
                               :prefix-2 prefix-2
                               :buffer-name (buffer-name)
                               :point (point)))
              (pending (car result))
              (candidates-cache (cdr result)))
         (if (ejc-not-nil-str pending)
             (progn
               (message "Receiving database structure...")
               (list))
           candidates-cache))))

;;;###autoload
(defun ejc-owners-candidates ()
  (ejc-candidates 'ejc-get-owners-candidates))

;;;###autoload
(defun ejc-tables-candidates ()
  (ejc-candidates 'ejc-get-tables-candidates))

;;;###autoload
(defun ejc-views-candidates ()
  (ejc-candidates 'ejc-get-views-candidates))

;;;###autoload
(defun ejc-packages-candidates ()
  (ejc-candidates 'ejc-get-packages-candidates))

;;;###autoload
(defun ejc-colomns-candidates ()
  (ejc-candidates 'ejc-get-colomns-candidates))

(defun ac-ejc-documentation (symbol-name)
  "Return a documentation string for SYMBOL-NAME."
  (if (not ejc-doc-created-p)
      (ejc-create-doc))
  (gethash (intern (downcase symbol-name)) ejc-sql-doc))

(provide 'ejc-completion-common)

;;; ejc-completion-common.el ends here
