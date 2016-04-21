;;; ejc-interaction.el -- ejc-sql interact with clojure.

;;; Copyright © 2013-2016 - Kostafey <kostafey@gmail.com>

(require 'ejc-lib)
(require 'ejc-format)
(require 'clomacs)

(clomacs-defun ejc-sql-set-db
               set-db
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Define ejc-sql.connect/db var.")

(clomacs-defun ejc-jpa-connect
               ejc-sql.jpa/connect-plain
               :lib-name "ejc-sql"
               :namespace ejc-sql.jpa
               :doc "Define ejc-sql.jpa/em var.")

(clomacs-defun ejc-add-classpath
               add-classpath
               :lib-name "ejc-sql"
               :namespace cemerick.pomegranate)

(clomacs-defun ejc-require
               clojure.core/require
               :lib-name "ejc-sql")

(clomacs-defun ejc-import
               clojure.core/import
               :lib-name "ejc-sql")

(defun ejc-connect-to-db (conn-struct)
  (ejc-require `'cemerick.pomegranate)
  (case (ejc-get-connection-type conn-struct)
    ;;----------------------------------------------------------------------
    ;; is JDBC/SQL
    (:sql
     (ejc-add-classpath (ejc-db-conn-classpath conn-struct))
     (ejc-import (read (ejc-db-conn-classname conn-struct)))
     (ejc-sql-set-db (ejc-connection-struct-to-plist conn-struct)))
    ;;----------------------------------------------------------------------
    ;; is JPA/JPQL
    (:jpa
     (ejc-jpa-connect (ejc-jpa-connection-name    conn-struct)
                      (ejc-jpa-persistent-xml-url conn-struct)
                      (ejc-jpa-domain-objects-url conn-struct)
                      (ejc-jpa-jdbc-driver-url    conn-struct))))
  (setq-local ejc-connection-struct conn-struct))

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
               ejc-sql.jpa/eval-jpql-print
               :lib-name "ejc-sql"
               :namespace ejc-sql.jpa
               :return-type :string
               :return-value :stdout)

(defun ejc-eval-sql-and-log (db sql)
  "Core function to evaluate SQL queries.
Prepare SQL string, evaluate SQL script and write them to log file"
  (if sql
      (let* ((prepared-sql (ejc-get-sql-from-string sql))
             (result (case (ejc-get-connection-type ejc-connection-struct)
                       (:sql (ejc--eval-sql-and-log-print
                              db
                              prepared-sql))
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

(clomacs-defun ejc-print
               clojure.core/print
               :lib-name "ejc-sql"
               :return-type :string
               :return-value :stdout)

(defun ejc-get-table-meta (db table-name)
  (ejc-print (ejc--get-table-meta db table-name)))

(clomacs-defun ejc-get-log-file-path
               print-log-file-path
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :return-value :stdout)

(defun ejc-quit-connection ()
  (interactive)
  (when (y-or-n-p  "Are you sure you want to close all jdbc connections?")
    (cider--quit-connection (clomacs-get-connection "ejc-sql")))
  (unless (cider-connected-p)
    (cider-close-ancillary-buffers)))

(provide 'ejc-interaction)
