;;; ejc-lib.el -- ejc-sql shared objects (the part of ejc-sql).

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

(defvar-local ejc-connection-name nil
  "Buffer-local connection name created with `ejc-create-connection'.")

(defvar-local ejc-connection-struct nil
  "Buffer-local connection structure.")

(defun ejc-string-endswith-p (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun ejc-get-db-name (subname)
  (let* ((separator (if (equal (cl-first (split-string subname "/")) subname)
                        ":" "/"))
         (raw-db-name (cl-first (last (split-string subname separator))))
         (raw-db-name (cl-first (split-string raw-db-name "?")))
         (raw-db-name (cl-first (split-string raw-db-name ";"))))
    raw-db-name))

(defun ejc-get-connection-type (conn-struct)
  "JDBC/SQL, JPA/JPQL or Hibernate/HQL connection type."
  (cond
   ((ejc-db-conn-p conn-struct) :sql)
   ((ejc-jpa-p conn-struct) :jpa)
   (t nil)))

(cl-defstruct ejc-db-conn
  "DB connection information structure"
                                        ; path to jdbc jar file
  (classpath "<path>/<filename>.jar")
                                        ; the JDBC driver class
  (classname "<com.povider.jdbc.DataBaseDriver>")
                                        ; the kind of database, e.g:
                                        ; informix-sqli, mysql, postgresql,
                                        ; oracle, sqlserver, etc.
  (subprotocol "<sql-kind>")
                                        ; db connection path
                                        ; locale, like ru_RU.1251
  (database    "<my-databse>")
  (subname (concat
            "://<db-host>:<db-port>:"
            "<db-server>=<server-name>;"
            "database=<db-name>;"
            "DB_LOCALE=<locale>;"
            "CLIENT_LOCALE=<locale>;"
            ))
  (user "<user-name>")
  (password "<password>")
  (connection-uri "jdbc:<povider>://[serverName[\instanceName][:portNumber]][;property=value[;property=value]]")
  (separator ";"))

(cl-defstruct ejc-jpa
  "DB connection information structure for JPA"
  (type :jpa)
  (connection-name    "<persistence-unit name=")
  (persistent-xml-url "path to META-INF/persistence.xml")
  (domain-objects-url "path to classes/")
  (jdbc-driver-url    "<path>/<filename>.jar"))

(defun ejc-connection-struct-to-plist (conn-struct)
  (-filter
   (lambda (x) (cdr x))
   (list
    `(:classname      . ,(ejc-db-conn-classname      conn-struct))
    `(:subprotocol    . ,(ejc-db-conn-subprotocol    conn-struct))
    `(:subname        . ,(ejc-db-conn-subname        conn-struct))
    `(:user           . ,(ejc-db-conn-user           conn-struct))
    `(:password       . ,(ejc-db-conn-password       conn-struct))
    `(:database       . ,(ejc-db-conn-database       conn-struct))
    `(:separator      . ,(ejc-db-conn-separator      conn-struct))
    `(:connection-uri . ,(ejc-db-conn-connection-uri conn-struct)))))

(defun ejc-add-quotes (str)
  (concat "\"" str "\""))

(defun ejc-add-squotes (str)
  (concat "'" str "'"))

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

(provide 'ejc-lib)

;;; ejc-lib.el ends here
