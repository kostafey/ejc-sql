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

(defun ejc-connect-to-db (conn-struct)
  (clomacs-add-to-cp (ejc-db-conn-classpath conn-struct))
  (clomacs-import (read (ejc-db-conn-classname conn-struct)))
  (ejc-sql-set-db (ejc-db-conn-classname   conn-struct)
                  (ejc-db-conn-subprotocol conn-struct)
                  (ejc-db-conn-subname     conn-struct)
                  (ejc-db-conn-user        conn-struct)
                  (ejc-db-conn-password    conn-struct))
  (clomacs-def ejc-db ejc-sql.connect/db)
  (setq ejc-db-type (ejc-db-conn-subprotocol conn-struct))
  (setq ejc-db-owner (ejc-db-conn-owner conn-struct))
  (setq ejc-db-name (ejc-get-db-name (ejc-db-conn-subname conn-struct))))

(defun ejc-get-sql-from-string (sql)
  (let* ((sql (replace-regexp-in-string ejc-clear-sql-regexp "" sql))
         (sql (replace-regexp-in-string "\"" "'" sql)))
    sql))

(clomacs-defun ejc--eval-sql-and-log
               ejc-sql.connect/eval-sql-and-log
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Evaluate user's SQL scripts and write them to log file.")

(defun ejc-eval-sql-and-log (sql)
  "Core function to evaluate SQL queries.
Prepare SQL string, evaluate SQL script and write them to log file"
  (if sql
      (let* ((prepared-sql (ejc-get-sql-from-string sql))
             (result (clomacs-print (ejc--eval-sql-and-log prepared-sql))))
        result)
    ""))

(defun ejc--eval-get-column (sql)
  (ejc-get-nrepl-stdout
   (concat "(eval-sql-internal-get-column "
           (ejc-add-quotes sql) " )")))

(defun ejc--eval-get-list (sql)
  (let ((sql-result-string (ejc--eval-get-column sql)))
    (split-string
     (substring sql-result-string
                1 (- (length sql-result-string) 1)))))

(defun ejc-get-table-meta (table-name)
  (ejc-get-nrepl-stdout
   (concat "(get-table-meta " (ejc-add-quotes table-name) ")")))

(provide 'ejc-interaction)
