;;; ejc-interaction.el -- ejc-sql interact with Clojure. -*- lexical-binding: t -*-

;;; Copyright © 2013-2018 - Kostafey <kostafey@gmail.com>

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
(require 'clomacs)

(clomacs-create-httpd-start ejc-httpd-start
                            :lib-prefix "ejc"
                            :lib-name "ejc-sql")

(clomacs-create-httpd-stop ejc-httpd-stop
                           :lib-prefix "ejc"
                           :lib-name "ejc-sql")

(clomacs-defun ejc-sql-set-db
               set-db
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Define ejc-sql.connect/db var."
               :httpd-starter 'ejc-httpd-start)

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
  (cl-case (ejc-get-connection-type conn-struct)
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

(clomacs-defun ejc--eval-sql-and-log-print
               eval-sql-and-log-print
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :return-type :string
               :doc "Core function to evaluate SQL queries.
  Prepare SQL string, evaluate SQL script and write them to log file")

(clomacs-defun ejc--is-query-running-p
               is-query-running?
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :return-type :boolean)

(clomacs-defun ejc--cancel-query
               cancel-query
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Cancel current query."
               :interactive t
               :return-type :list)

(clomacs-defun ejc--eval-sql-get-column
               eval-sql-internal-get-column
               :lib-name "ejc-sql"
               :return-type :list
               :namespace ejc-sql.connect
               :doc "Get `sql', return list.")

(defalias 'ejc--eval-get-list 'ejc--eval-sql-get-column)

(clomacs-defun ejc-get-table-meta
               ejc-sql.connect/get-table-meta
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect)

(clomacs-defun ejc-print
               clojure.core/print
               :lib-name "ejc-sql"
               :return-type :string
               :return-value :stdout)

(clomacs-defun ejc-write-result-file
               write-result-file
               :namespace ejc-sql.output
               :lib-name "ejc-sql"
               :return-type :string)

(clomacs-defun ejc-clear-result-file
               clear-result-file
               :namespace ejc-sql.output
               :lib-name "ejc-sql"
               :return-type :string)

(clomacs-defun ejc-get-log-file-path
               print-log-file-path
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :return-value :stdout)

(clomacs-defun ejc-pretty-print
               pretty-print
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :return-value :stdout)

(clomacs-defun ejc-set-rows-limit
               set-rows-limit
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :doc (concat "Set limit for number of records to output. "
                            "When nil no limit."))

(clomacs-defun ejc-get-stucture
               get-stucture
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-owners-candidates
               get-owners-candidates
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-tables-candidates
               get-tables-candidates
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-colomns-candidates
               get-colomns-candidates
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-cached-owners-list
               get-owners
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-cached-tables-list
               get-tables
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list
               :doc "Return cached tables list.
  Requires `ejc-db' buffer local variable as parameter.")

(clomacs-defun ejc-get-cached-colomns-list
               get-colomns
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-invalidate-cache-inner
               invalidate-cache
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :doc (concat "Clean your current connection cache "
                            "(database owners and tables list)."))

(defun ejc-invalidate-cache ()
  "Clean current connection cache (database owners and tables list)."
  (interactive)
  (ejc-invalidate-cache-inner ejc-connection-struct))

(clomacs-defun ejc-get-cache
               get-cache
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc-select-db-meta-script
               select-db-meta-script
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc-get-db-name
               get-db-name
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(defun ejc-quit-connection ()
  "Stop nREPL process, mark ejc-sql-mode buffers disconnected."
  (interactive)
  (when (y-or-n-p  "Are you sure you want to close all jdbc connections?")
    (ejc-httpd-stop)
    (cider--close-connection (clomacs-get-connection "ejc-sql"))
    ;; Update modeline of ejc-sql-mode buffers - mark as disconnected.
    (let ((buffers (buffer-list)))
      (while buffers
        (with-current-buffer (car buffers)
          (when (member 'ejc-sql-mode minor-mode-list)
            (sql-highlight-product)
            (spinner-stop)))
        (setq buffers (cdr buffers)))))
  (unless (cider-connected-p)
    (cider-close-ancillary-buffers)))

(defalias 'ejc-close-connection 'ejc-quit-connection)

(defun ejc-buffer-connected-p ()
  "Check if current buffer is connected to database."
  (and (clomacs-get-connection "ejc-sql") (boundp 'ejc-db) ejc-db))

(provide 'ejc-interaction)

;;; ejc-interaction.el ends here
