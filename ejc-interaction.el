;;; ejc-interaction.el -- ejc-sql interact with Clojure. -*- lexical-binding: t -*-

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

(require 'dash)
(require 'ejc-lib)
(require 'ejc-format)
(require 'clomacs)

(clomacs-create-httpd-start ejc-httpd-start
                            :lib-name "ejc-sql")

(clomacs-create-httpd-stop ejc-httpd-stop
                           :lib-name "ejc-sql")

(clomacs-defun ejc-sql-set-db
               set-db
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :doc "Define ejc-sql.connect/db var."
               :httpd-starter 'ejc-httpd-start)

(clomacs-defun ejc-add-classpath
               add-classpath
               :lib-name "ejc-sql"
               :namespace ejc-sql.classpath)

(clomacs-defun ejc-require
               clojure.core/require
               :lib-name "ejc-sql")

(clomacs-defun ejc-import
               clojure.core/import
               :lib-name "ejc-sql")

(clomacs-defun ejc-class-for-name
               Class/forName
               :lib-name "ejc-sql")

(clomacs-defun ejc-get-dependeces-files-list
               get-dependeces-files-list
               :lib-name "ejc-sql"
               :namespace ejc-sql.deps-resolver
               :return-type :list)

(defun ejc-connect-to-db (conn-struct)
  (let* ((jars-to-load
          (if-let (dependencies (alist-get :dependencies conn-struct))
              ;; Resolve dependencies and prepare jars paths list to load.
              (ejc-get-dependeces-files-list dependencies)
            (let* ((classpath (alist-get :classpath conn-struct))
                   (dependency-jars (-map
                                     (lambda (jar-path)
                                       (ejc-get-dependeces-files-list
                                        (ejc-path-to-lein-artifact jar-path)))
                                     classpath)))
              ;; Join jar files paths from `:classpath' vector of
              ;; `ejc-create-connection' and dependencies for this jars.
              (delq nil
                    (delete-dups
                     (append
                      (-flatten dependency-jars)
                      (append classpath nil))))))))
    (-map 'ejc-add-classpath jars-to-load))
  (when-let ((classname (alist-get :classname conn-struct)))
    (ejc-class-for-name classname)
    (ejc-import (read classname)))
  (ejc-sql-set-db conn-struct))

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

(clomacs-defun ejc--describe-table
               describe-table
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc-get-entity-type
               get-entity-type-fmt
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :eval)

(clomacs-defun ejc-get-parameters
               get-parameters
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-entity-description
               get-entity-description
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc--get-result-file-path
               get-result-file-path
               :lib-name "ejc-sql"
               :namespace ejc-sql.output)

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

(clomacs-defun ejc-format-by-hibernate
               format-sql-print
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :return-value :stdout)

(clomacs-defun ejc-set-fetch-size
               set-fetch-size
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :doc (concat "Set limit for number of records to output. "
                            "When nil no limit."))

(clomacs-defun ejc-set-max-rows
               set-max-rows
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :doc (concat "Set limit for number of records to contain "
                            "in ResultSet. When nil no limit."))

(clomacs-defun ejc-set-column-width-limit
               set-column-width-limit
               :lib-name "ejc-sql"
               :namespace ejc-sql.output
               :doc (concat "Set limit for number of chars per column to "
                            "output to output. When nil no limit."))

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

(clomacs-defun ejc-get-views-candidates
               get-views-candidates
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-get-packages-candidates
               get-packages-candidates
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

(clomacs-defun ejc-get-keywords-inner
               get-keywords
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure
               :return-type :list)

(clomacs-defun ejc-invalidate-cache-inner
               invalidate-cache
               :lib-name "ejc-sql"
               :namespace ejc-sql.cache
               :doc (concat "Clean your current connection cache "
                            "(database owners and tables list)."))

(clomacs-defun ejc-validate-connection
               validate-connection
               :lib-name "ejc-sql"
               :namespace ejc-sql.connect
               :return-type :eval)

(defun ejc-invalidate-cache ()
  "Clean current connection cache (database owners and tables list)."
  (interactive)
  (ejc-invalidate-cache-inner ejc-db))

(clomacs-defun ejc-output-cache
               output-cache
               :lib-name "ejc-sql"
               :namespace ejc-sql.cache
               :return-type :eval)

(defun ejc-print-cache ()
  "Print current database connection cache when run from connected SQL buffer.
Print all connections cache otherwise."
  (interactive)
  (pprint (ejc-output-cache ejc-db)))

(clomacs-defun ejc-select-db-meta-script
               select-db-meta-script
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc-get-db-name
               get-db-name
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(clomacs-defun ejc-get-this-owner
               get-this-owner
               :lib-name "ejc-sql"
               :namespace ejc-sql.structure)

(defun ejc-unset-mode-name ()
  (if (derived-mode-p 'org-mode)
      (setq mode-name "Org")
    (if (derived-mode-p 'sql-mode)
        (setq mode-name "SQL"))))

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
            (ejc-unset-mode-name)
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
