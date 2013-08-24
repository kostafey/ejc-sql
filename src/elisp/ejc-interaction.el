;;; ejc-interaction.el -- ejc-sql interact with clojure.

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

(require 'ejc-lib)
(require 'ejc-format)
(require 'clomacs)
(require 'clomacs-lib)

(defvar ejc-nrepl-connrection-buffer-name (nrepl-connection-buffer-name))

(defvar ejc-clojure-src-file "connect.clj"
  "Main clojure src file name.")

(defvar ejc-clojure-offline-file "clojure_offline.clj"
  "Clojure-offline src helper file name.")

(defun ejc-ensure-nrepl-runnig ()
  "Ensures nrepl is runnig.
If not, launch it, return nil. Return t otherwise."
  (interactive)
  (let ((is-running t))
    (when (not (clomacs-is-nrepl-runnig))
      (setq is-running nil)
      (clomacs-launch-nrepl (ejc-find-clojure-main-file)))
    is-running))

(defun ejc-find-clojure-main-file ()
  "Return the full path to `ejc-clojure-src-file'."
  (find-file-in-load-path ejc-clojure-src-file))

(defun ejc-find-clojure-offline-file ()
  "Return the full path to `ejc-clojure-offline-file'."
  (ejc-find-file-in-load-path ejc-clojure-offline-file))

(defun ejc-get-nrepl-stdout (expr)
  "Evaluate `expr', print it and return printed text as function's result."
  ;; nrepl-eval-async
  (plist-get (nrepl-eval
              (concat
               " (in-ns 'ejc-sql.core)"
               " (print " expr ")")) :stdout))

(defun ejc-get-nrepl-result (expr)
  "Evaluate `expr', and return expression's evaluation result."
  (plist-get (nrepl-eval
              (concat
               " (in-ns 'ejc-sql.core)"
               " " expr)) :value))

(defun ejc-load-clojure-side ()
  "Evaluate clojure side, run startup initialization functions."
  (if (not ejc-sql-log-file-path)
      (progn
        ;; load clojure-offline lib
        (nrepl-load-file (ejc-find-clojure-offline-file))
        ;; add clojure-side files to classpath
        (nrepl-eval
         (concat
          " (ejc-sql.clojure-offline/add-to-cp "
          (ejc-add-quotes
           (file-name-directory
            (expand-file-name ".." (ejc-find-clojure-main-file)))) ")"))
        ;; load ejc-sql main clojure-side file
        (nrepl-load-file (ejc-find-clojure-main-file))
        (setq ejc-sql-log-file-path
              (ejc-get-nrepl-stdout "sql-log-file-path")))))

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

(defun ejc-eval-sql (sql)
  "Core function to evaluate SQL queries."
  (if sql
      (let* ((prepared-sql (ejc-get-sql-from-string sql))
             (result (ejc-get-nrepl-stdout
                      (concat "(eval-user-sql "
                              (ejc-add-quotes prepared-sql) ")"))))
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
