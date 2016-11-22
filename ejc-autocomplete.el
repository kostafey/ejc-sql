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

(defvar ejc-connections-cache (make-hash-table :test #'equal)
  "Tables and owners cache shared for all buffers of the same connection.
It has the following example structure:
  ('h2-payments' (:owners ('root')
                  :tables ('root' ('users' 'payments')))
   'mysql-sales' (:owners ('su' 'admin')
                  :tables ('su' ('product' 'sales')
                           'admin' ('log'))))")

(defun ejc-get-owners-cache ()
  (let ((connection-cache (gethash ejc-connection-name ejc-connections-cache)))
    (if connection-cache
        (gethash :owners connection-cache))))

(defun ejc-set-owners-cache (owners)
  (let* ((connection-cache (gethash ejc-connection-name ejc-connections-cache))
         (connection-cache (or connection-cache
                               (let ((cc (make-hash-table :test #'equal)))
                                 (puthash ejc-connection-name
                                          cc
                                          ejc-connections-cache)
                                 cc))))
    (puthash :owners owners connection-cache)))

(defun ejc-get-tables-cache (owner)
  (let* ((connection-cache (gethash ejc-connection-name ejc-connections-cache))
         (owner-tables-cache (if connection-cache
                                 (gethash :tables connection-cache))))
    (if owner-tables-cache
        (gethash owner owner-tables-cache))))

(defun ejc-set-tables-cache (owner tables)
  (let* ((connection-cache (gethash ejc-connection-name ejc-connections-cache))
         (connection-cache (or connection-cache
                               (let ((cc (make-hash-table :test #'equal)))
                                 (puthash ejc-connection-name
                                          cc
                                          ejc-connections-cache)
                                 cc)))
         (owner-tables-cache (gethash :tables connection-cache))
         (owner-tables-cache (or owner-tables-cache
                                 (let ((cc (make-hash-table :test #'equal)))
                                   (puthash :tables
                                            cc
                                            connection-cache)
                                   cc))))
    (puthash owner tables owner-tables-cache)))

(defun ejc-invalidate-cache ()
  "Clean your current connection cache (database owners and tables list)."
  (interactive)
  (remhash ejc-connection-name ejc-connections-cache))

;;;###autoload
(defun ejc--select-db-meta-script (meta-type &optional owner table)
  (let ((owner (if owner
                   (ejc-add-squotes owner)
                 (ejc-add-squotes (ejc-db-conn-user ejc-connection-struct))))
        (db-type (ejc-db-conn-subprotocol ejc-connection-struct))
        (db-name (or (ejc-db-conn-database ejc-connection-struct)
                     (ejc-get-db-name
                      (ejc-db-conn-subname ejc-connection-struct)))))
    (cond
     ;;----------
     ;; informix
     ;;----------
     ((string-match "informix" db-type)
      (cond
       ((eq :owners meta-type) nil)
       ((eq :tables meta-type)
        (concat " SELECT TRIM(t.tabname) as tablesList "
                " FROM systables AS t   "
                " WHERE t.tabtype = 'T' "
                "   AND t.tabid >= 100  "
                " ORDER BY t.tabname;   "))
       ((eq :columns meta-type)
        (concat " SELECT TRIM(c.colname) AS column_name \n"
                "  FROM systables AS t, syscolumns AS c \n"
                " WHERE t.tabid = c.tabid               \n"
                "   AND t.tabtype = 'T'                 \n"
                "   AND t.tabid >= 100                  \n"
                "   AND TRIM(t.tabname) = '" table "'   \n"
                " ORDER BY c.colno;                     \n"))))
     ;;-------
     ;; mysql
     ;;-------
     ((string-match "mysql" db-type)
      (cond
       ((eq :owners meta-type) nil)
       ((eq :tables meta-type)
        (concat
         " SELECT table_name FROM INFORMATION_SCHEMA.TABLES "
         " WHERE table_schema = '" db-name "'"))
       ((eq :columns meta-type)
        (concat "SELECT column_name              \n"
                "FROM INFORMATION_SCHEMA.COLUMNS \n"
                "WHERE table_name = '" table "'  \n"))))
     ;;--------
     ;; oracle
     ;;--------
     ((string-match "oracle" db-type)
      (let ((owner (upcase owner)))
        (cond
         ((eq :entity meta-type)
          (concat "SELECT text             "
                  "FROM all_source         "
                  "WHERE name = '" (upcase table) "'"))
         ((eq :types meta-type)
          "SELECT * FROM USER_TYPES")
         ((eq :owners meta-type)
          (concat "select DISTINCT(owner) "
                  " from ALL_OBJECTS"))
         ((eq :tables meta-type)
          (concat " SELECT table_name, owner \n"
                  " FROM all_tables          \n"
                  (if owner
                      (concat " WHERE owner = " owner) "")))
         ((eq :columns meta-type)
          (concat " SELECT column_name             \n"
                  " FROM ALL_TAB_COLUMNS           \n"
                  " WHERE table_name = '" table "' \n"))
         ((eq :constraints meta-type)
          (if table
              (concat " SELECT * FROM all_constraints    \n"
                      " WHERE owner = "owner"            \n"
                      "       AND table_name = '"table"' \n")
            "SELECT * FROM user_constraints"))
         ((eq :procedures meta-type)
          (concat " SELECT object_name, procedure_name \n"
                  " FROM all_procedures                \n"
                  " WHERE owner = "owner"              \n"))
         ((eq :objects meta-type)
          (concat "SELECT * FROM all_objects WHERE object_type IN "
                  "('FUNCTION','PROCEDURE','PACKAGE')")))))
     ;;--------
     ;; h2
     ;;--------
     ((string-match "h2" db-type)
      (cond
       ((eq :tables meta-type)
        (concat "SELECT table_name              \n"
                "FROM INFORMATION_SCHEMA.TABLES \n"
                "WHERE TABLE_SCHEMA='PUBLIC'"))
       ((eq :columns meta-type)
        (concat "SELECT column_name              \n"
                "FROM INFORMATION_SCHEMA.COLUMNS \n"
                "WHERE table_name = '" table "'  \n"))))
     ;;-------
     ;; ms sql server
     ;;-------
     ((string-match "sqlserver" db-type)
      (cond
       ((eq :tables meta-type)
        "SELECT * FROM information_schema.tables")
       ((eq :columns meta-type)
        (concat "SELECT COLUMN_NAME              \n"
                "FROM INFORMATION_SCHEMA.COLUMNS \n"
                "WHERE TABLE_NAME='" table "'      ")))))))

(defun ejc-get-owners-list ()
  (-distinct (ejc--eval-get-list
              ejc-db
              (ejc--select-db-meta-script :owners))))

(defun ejc-get-tables-list (&optional owner)
  (-distinct (ejc--eval-get-list
              ejc-db
              (ejc--select-db-meta-script :tables owner))))

(defun ejc-get-columns-list (owner table)
  (ejc--eval-get-list
   ejc-db
   (ejc--select-db-meta-script :columns owner table)))

(defun ejc-get-procedures-list (&optional owner)
  (-distinct (ejc--eval-get-list
              ejc-db
              (ejc--select-db-meta-script :procedures))))

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
        (message "Receiving database structure... %s" pending))
    (if (and (not prefix-1) (not prefix-2))
        (append candidates-cache (ejc-get-ansi-sql-words))
      candidates-cache)))

(defun ejc-return-point ()
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
