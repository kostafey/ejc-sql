;;; ejc-interaction.el -- ejc-sql interact with clojure.

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

(require 'ejc-lib)
(require 'ejc-format)
(require 'clomacs)
(require 'clomacs-lib)

(defun ejc-ensure-nrepl-runnig ()
  "Ensures nrepl is runnig.
If not, launch it, return nil. Return t otherwise."
  (interactive)
  (clomacs-ensure-nrepl-runnig))

(clomacs-defun ejc-sql-set-db 
               ejc-sql.connect/set-db
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Define ejc-sql.connect/db var.")

(clomacs-defun ejc-jpa-connect
               ejc-sql.jpa/connect-plain
               :lib-name "ejc-sql"
               :namespace ejc-sql.jpa
               :doc "Define ejc-sql.jpa/em var.")

(defun ejc-connect-to-db (conn-struct)
  (case (ejc-get-connection-type conn-struct)
    ;;----------------------------------------------------------------------
    ;; is JDBC/SQL
    (:sql
     (clomacs-add-to-cp (ejc-db-conn-classpath conn-struct))
     (clomacs-import (read (ejc-db-conn-classname conn-struct)))
     (ejc-sql-set-db (ejc-db-conn-classname   conn-struct)
                     (ejc-db-conn-subprotocol conn-struct)
                     (ejc-db-conn-subname     conn-struct)
                     (ejc-db-conn-user        conn-struct)
                     (ejc-db-conn-password    conn-struct))
     (setq ejc-db-type (ejc-db-conn-subprotocol conn-struct))
     (setq ejc-db-owner (ejc-db-conn-owner conn-struct))
     (setq ejc-db-name (ejc-get-db-name (ejc-db-conn-subname conn-struct))))
    ;;----------------------------------------------------------------------
    ;; is JPA/JPQL
    (:jpa
     (ejc-jpa-connect (ejc-jpa-connection-name    conn-struct)
                      (ejc-jpa-persistent-xml-url conn-struct)
                      (ejc-jpa-domain-objects-url conn-struct)
                      (ejc-jpa-jdbc-driver-url    conn-struct)))
    (setq-local ejc-connection-struct conn-struct)))

(defun ejc-get-sql-from-string (sql)
  (let* ((sql (replace-regexp-in-string ejc-clear-sql-regexp "" sql))
         (sql (replace-regexp-in-string "\"" "'" sql)))
    sql))

(clomacs-defun ejc--eval-sql-and-log
               ejc-sql.connect/eval-sql-and-log
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Evaluate user's SQL scripts and write them to log file.")

(clomacs-defun ejc--eval-sql-and-log-print
               ejc-sql.connect/eval-sql-and-log-print
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :return-type :string
               :return-value :stdout)

(clomacs-defun ejc--eval-jpql
               ejc-sql.jpa/eval-jpql
               :lib-name "ejc-sql"
               :namespace ejc-sql.jpa
               :return-type :string
               :return-value :stdout)

(defun ejc-eval-sql-and-log (sql)
  "Core function to evaluate SQL queries.
Prepare SQL string, evaluate SQL script and write them to log file"
  (if sql
      (let* ((prepared-sql (ejc-get-sql-from-string sql))
             (result (case (ejc-get-connection-type ejc-connection-struct)
                       (:sql (ejc--eval-sql-and-log-print prepared-sql))
                       (:jpa (ejc--eval-jpql prepared-sql))
                       (nil "No database connection."))))
        result)))

(clomacs-defun ejc--eval-sql-get-column
               ejc-sql.connect/eval-sql-internal-get-column
               :lib-name "ejc-sql"
               :return-type :list
               :namespace ejc-sql.connect
               :doc "Get `sql', return list.")

(defalias 'ejc--eval-get-list 'ejc--eval-sql-get-column)

(clomacs-defun ejc--get-table-meta
               ejc-sql.connect/get-table-meta
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect)

(defun ejc-get-table-meta (table-name)
  (clomacs-print (ejc--get-table-meta table-name)))

(provide 'ejc-interaction)
