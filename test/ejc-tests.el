(let ((current-directory (file-name-directory (or load-file-name ""))))
  (setq ejc-test-path (expand-file-name "." current-directory))
  (setq ejc-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path ejc-root-path)
(add-to-list 'load-path ejc-test-path)

(require 'cl)

(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"
              (:report-format 'lcov)
              (:merge-report nil)
              (:send-report nil)))
(require 'ejc-sql)
(require 'ejc-autocomplete)

(defun ejc-test:h2-jar ()
  "The H2 jar the tests connect with, in the local Maven repository."
  (expand-file-name "repository/com/h2database/h2/2.5.252/h2-2.5.252.jar"
                    (ejc-test:get-m2-path)))

(defun ejc-test:fetch-h2-jar ()
  "Fetch the H2 jar with Maven, unless it is in the local repository.
Tell why if it can't, the tests needing it fail then."
  (let ((jar (ejc-test:h2-jar)))
    (unless (file-exists-p jar)
      (print "Run maven-dependency-plugin")
      (if (not (executable-find "mvn"))
          (message "No mvn to fetch %s" jar)
        (with-temp-buffer
          (let ((status (call-process-shell-command
                         (concat
                          "mvn org.apache.maven.plugins:"
                          "maven-dependency-plugin:2.10:get "
                          "-Dartifact=com.h2database:h2:2.5.252")
                         nil t)))
            (print (buffer-string))
            (cond
             ((not (eql status 0))
              (message "mvn failed with %s to fetch %s" status jar))
             ((not (file-exists-p jar))
              (message "mvn succeeded, but there is no %s" jar)))))))))

(defun ejc-test:run-lein ()
  "Run `lein test', print its output, return its exit status."
  (print "Run lein test")
  (with-temp-buffer
    (let ((status (call-process-shell-command "lein test" nil t)))
      (print (buffer-string))
      status)))

(ert-deftest ejc-test:run-lein-test ()
  :tags '(cl)
  (should (eql 0 (ejc-test:run-lein))))

(ert-deftest ejc-test:ejc-lein-artifact-to-path ()
  :tags '(el)
  (should
   (equal "~/.m2/repository/org/xerial/sqlite-jdbc/3.23.1/sqlite-jdbc-3.23.1.jar"
          (ejc-lein-artifact-to-path [org.xerial/sqlite-jdbc "3.23.1"])))
  (should
   (equal "~/.m2/repository/com/h2database/h2/1.4.199/h2-1.4.199.jar"
          (ejc-lein-artifact-to-path [com.h2database/h2 "1.4.199"])))
  (should
   (equal "~/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"
          (ejc-lein-artifact-to-path [mysql/mysql-connector-java "5.1.44"])))
  (should
   (equal "~/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar"
          (ejc-lein-artifact-to-path [postgresql/postgresql "9.3-1102.jdbc41"])))
  (should
   (equal "~/.m2/repository/com/microsoft/sqlserver/mssql-jdbc/6.2.2.jre8/mssql-jdbc-6.2.2.jre8.jar"
          (ejc-lein-artifact-to-path [com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"])))
  (should
   (equal "~/.m2/repository/com/oracle/jdbc/ojdbc8/12.2.0.1/ojdbc8-12.2.0.1.jar"
          (ejc-lein-artifact-to-path [com.oracle.jdbc/ojdbc8 "12.2.0.1"]))))

(ert-deftest ejc-test:ejc-path-to-lein-artifact ()
  :tags '(el)
  (should
   (equal [[org.xerial/sqlite-jdbc "3.23.1"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/org/xerial/sqlite-jdbc/3.23.1/sqlite-jdbc-3.23.1.jar")))
  (should
   (equal [[com.h2database/h2 "1.4.199"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/com/h2database/h2/1.4.199/h2-1.4.199.jar")))
  (should
   (equal [[mysql/mysql-connector-java "5.1.44"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar")))
  (should
   (equal [[postgresql/postgresql "9.3-1102.jdbc41"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar")))
  (should
   (equal [[com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/com/microsoft/sqlserver/mssql-jdbc/6.2.2.jre8/mssql-jdbc-6.2.2.jre8.jar")))
  (should
   (equal [[com.oracle.jdbc/ojdbc8 "12.2.0.1"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/com/oracle/jdbc/ojdbc8/12.2.0.1/ojdbc8-12.2.0.1.jar")))
  (should
   (equal [[com.ibm.informix/jdbc "4.50.3"]]
          (ejc-path-to-lein-artifact
           "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar")))
  ;; There is nothing to derive the coordinates from when the jar file lies
  ;; outside of the local maven repository, or too shallow inside it.
  (should
   (equal nil
          (ejc-path-to-lein-artifact
           "/opt/cloudera-jdbc-drivers/lib/2.6.30.1036/ImpalaJDBC42-2.6.30.1036.jar")))
  (should
   (equal nil
          (ejc-path-to-lein-artifact
           "~/.m2/repository/ojdbc8.jar"))))

(defun ejc-test:get-boundaries (point-position sql)
  (with-temp-buffer
    (insert sql)
    (goto-char point-position)
    (ejc-get-sql-boundaries-at-point)))

(ert-deftest ejc-test:ejc-get-sql-boundaries-at-point ()
  :tags '(el)
  (should (equal '(1 20)
                 (ejc-test:get-boundaries 1 "SELECT * FROM table")))
  (should (equal '(1 1)
                 (ejc-test:get-boundaries 1 "/ SELECT * FROM table")))
  (should (equal '(3 22)
                 (ejc-test:get-boundaries 2 "/ SELECT * FROM table")))
  (should (equal '(1 21)
                 (ejc-test:get-boundaries 1 "SELECT * FROM table /")))
  ;; Separator useless without [RET]:
  (should (equal '(1 41)
                 (ejc-test:get-boundaries
                  1  "SELECT * FROM table / SELECT * FROM user")))
  (should (equal '(1 41)
                 (ejc-test:get-boundaries
                  23 "SELECT * FROM table / SELECT * FROM user")))
  (should (equal '(4 23)
                 (ejc-test:get-boundaries 3 "\n/\nSELECT * FROM table")))
  (should (equal '(1 22)
                 (ejc-test:get-boundaries 1 "\nSELECT * FROM table\n/")))
  (should (equal '(26 44)
                 (ejc-test:get-boundaries 26 (concat "\n"
                                                     "SELECT * FROM table\n"
                                                     "/  \n"
                                                     "SELECT * FROM user"))))
  (should (equal '(1 26)
                 (ejc-test:get-boundaries 1 "\n  SELECT * FROM table\n  /")))
  (should (equal '(1 23)
                 (ejc-test:get-boundaries 1 "\n  SELECT * FROM table\n  /  ")))
  (should (equal '(1 23)
                 (ejc-test:get-boundaries 1
                                          (concat "\n"
                                                  "  SELECT * FROM table\n"
                                                  "  /  \n"
                                                  "  SELECT * FROM user\n"))))
  (should (equal '(32 50)
                 (ejc-test:get-boundaries 32
                                          (concat "\n"
                                                  "  SELECT * FROM table\n"
                                                  "  /  \n"
                                                  "  SELECT * FROM user")))))

(ert-deftest ejc-test:ejc-eldoc-function-org-mode ()
  :tags '(el)
  (with-temp-buffer
    (insert "#+begin_src sql
"
            "UPDATE users SET name = 'x' WHERE id = 1
"
            "#+end_src
"
            "
"
            "* TODO Check the result
"
            "#+begin_src emacs-lisp
"
            "(setq where 1)
"
            "#+end_src
")
    (org-mode)
    (cl-letf (((symbol-function 'ejc-buffer-connected-p) #'ignore))
      (goto-char (point-min))
      (search-forward "WHERE id")
      (should (ejc-eldoc-function))
      ;; Outside of the SQL source block body there is no SQL.
      (dolist (text '("#+begin_src sql" "#+end_src" "TODO Check" "where 1"))
        (goto-char (point-min))
        (search-forward text)
        (should-not (ejc-eldoc-function)))
      ;; The other ElDoc functions of the buffer are shown outside of SQL.
      (add-hook 'eldoc-documentation-functions
                (lambda (&rest _) (when (looking-at "Check") "other doc"))
                nil t)
      (cl-letf (((symbol-function 'eldoc-mode) #'ignore))
        (ejc-eldoc-setup))
      (let ((doc (lambda ()
                   (run-hook-with-args-until-success
                    'eldoc-documentation-functions #'ignore))))
        (goto-char (point-min))
        (search-forward "TODO ")
        (should (equal "other doc" (funcall doc)))
        (goto-char (point-min))
        (search-forward "WHERE id")
        (should (string-match-p "UPDATE" (funcall doc)))))))

(ert-deftest ejc-test:get-log-file-path ()
  :tags '(el+cl)
  (let ((log-file-path (ejc-get-log-file-path)))
    (should (stringp log-file-path))
    (print (format "Log file path: %s" log-file-path))))

(defun ejc-test:get-m2-path ()
  (file-name-as-directory
   (if (eq system-type 'windows-nt)
       (cl-labels ((concat-path (&rest folders)
                                (let ((path))
                                  (dolist (folder folders)
                                    (if folder
                                        (setq path
                                              (expand-file-name folder path))))
                                  path)))
         (concat-path "C:" "Users" (user-login-name) ".m2"))
     (expand-file-name ".m2" (file-truename "~")))))

(defun ejc-test:get-temp-path ()
  (file-name-as-directory
   (if (eq system-type 'windows-nt)
       (let ((dpath "C:/temp"))
         (make-directory dpath t)
         dpath)
     (file-truename "~/tmp"))))

(ert-deftest ejc-test:ejc-append-without-duplicates ()
  :tags '(el)
  (should (equal
           '(("a" 1) ("b" 2) ("c" 4))
           (ejc-append-without-duplicates
            '(("a" 1) ("b" 2)) '(("a" 3) ("c" 4)) 'car :left)))
  (should (equal
           '(("a" 3) ("c" 4) ("b" 2))
           (ejc-append-without-duplicates
            '(("a" 1) ("b" 2)) '(("a" 3) ("c" 4)) 'car :right))))

(ert-deftest ejc-test:ejc-org-block-connection ()
  :tags '(el)
  (require 'ob-core)
  (let ((ejc-connections '(("first" (:subprotocol . "h2"))
                           ("second" (:subprotocol . "sqlite"))))
        (ejc-org-loaded-connections nil)
        (loaded nil)
        (repl (get-buffer-create " *ejc-test-repl*")))
    (cl-letf (((symbol-function 'clomacs-get-connection)
               (lambda (&rest _) repl))
              ((symbol-function 'ejc-connect-to-db)
               (lambda (db name) (push (cons name db) loaded))))
      (with-temp-buffer
        (insert "#+begin_src sql :ejc-sql second\nSELECT 1;\n#+end_src\n")
        (org-mode)
        (goto-char (point-min))
        (forward-line)
        (let ((params (nth 2 (org-babel-get-src-block-info 'no-eval))))
          (should (equal '("second" (:subprotocol . "sqlite"))
                         (ejc-org-block-connection params)))
          ;; The JDBC driver is loaded only once per nREPL.
          (ejc-org-block-connection params)
          (should (equal '(("second" (:subprotocol . "sqlite"))) loaded))))
      (should-not (ejc-org-block-connection '((:results . "replace"))))
      (should-error (ejc-org-block-connection '((:ejc-sql . "third")))
                    :type 'user-error))
    (kill-buffer repl)))

(cl-defun ejc-test:run-sql (sql &optional connect)
  ;; Type SQL query and eval it.
  (with-current-buffer (ejc-get-temp-editor-buffer "test")
    ;; Connect to test database, if bufer just created
    (when connect
      (setq cider-boot-parameters "repl -s -H localhost wait")
      (setq cider-lein-parameters "repl :headless :host localhost")
      (let ((dir (file-name-directory ejc-conn-statistics-file)))
        (if (not (file-accessible-directory-p dir))
            (make-directory dir)))
      (ejc-connect connect))
    (end-of-buffer)
    (insert sql)
    (ejc-eval-user-sql-at-point :sync t :display-result t))
  ;; Get the results.
  (with-current-buffer ejc-results-buffer
    (buffer-substring-no-properties (point-max) (point-min))))

(clomacs-defun ejc-test:mcp-connection-names
               connection-names
               :lib-name "ejc-sql"
               :namespace ejc-sql.mcp
               :return-type :list)

(ert-deftest ejc-test:get-connection ()
  :tags '(el+cl)
  (let* ((db-path (concat "file://" (ejc-test:get-temp-path)
                          "database;AUTO_SERVER=TRUE"))
         (classpath (vector (ejc-test:h2-jar)))
         (conn (ejc-create-connection
                "H2-test-connection"
                :classpath classpath
                :classname "org.h2.Driver"
                :subprotocol "h2"
                :subname db-path
                :user "a_user"
                :password "secret")))
    (should (file-exists-p (ejc-test:h2-jar)))
    (should
     (equal
      `("H2-test-connection"
        (:classname . "org.h2.Driver")
        (:classpath . ,classpath)
        (:password . "secret")
        (:user . "a_user")
        (:subname . ,db-path)
        (:subprotocol . "h2"))
      (car conn)))
    ;; Delete previous run temp database files
    (mapcar (lambda (x)
              (let ((path-to-x (expand-file-name x (ejc-test:get-temp-path))))
                (if (file-exists-p path-to-x) (delete-file path-to-x))))
            '("database.mv.db" "database.trace.db"))
    ;; Ensure temp SQL editor buffer is empty.
    (with-current-buffer (ejc-get-temp-editor-buffer "test")
      (erase-buffer))
    ;; ------------
    ;; Create table
    (should
     (equal
      "Executed\n"
      (ejc-test:run-sql
       (concat
        "CREATE TABLE IF NOT EXISTS users (           \n"
        "  id int NOT NULL AUTO_INCREMENT,            \n"
        "  login varchar(45),                         \n"
        "  email varchar(45),                         \n"
        "  first_name varchar(255) NOT NULL,          \n"
        "  last_name varchar(45) NOT NULL,            \n"
        "  register_date timestamp DEFAULT NULL,      \n"
        "  PRIMARY KEY (id),                          \n"
        "  CONSTRAINT id_UNIQUE UNIQUE (id)           \n"
        ")                                            \n")
       "H2-test-connection")))
    ;; -----------
    ;; Insert data
    (should
     (equal
      (concat "Records affected: 1\n"
              "Records affected: 1\n"
              "Records affected: 1\n")
      (ejc-test:run-sql
       (concat
        "/                                                       \n"
        "INSERT INTO users (login, email, first_name, last_name) \n"
        "VALUES ('admin', 'admin@mail.com', 'John', 'Doe');      \n"
        "INSERT INTO users (login, email, first_name, last_name) \n"
        "VALUES ('neo', 'neo@mail.com', 'Thomas', 'Anderson');   \n"
        "INSERT INTO users (login, email, first_name, last_name) \n"
        "VALUES ('morpheus', 'morpheus@mail.com', 'Nil', 'Nil'); \n"))))
    ;; -----------
    ;; Update data
    (should
     (equal
      "Records affected: 3\n"
      (ejc-test:run-sql
       (concat
        "/                                                       \n"
        "UPDATE users SET register_date = '2012-12-14 17:25:03'; \n"))))
    ;; -----------
    ;; Select data
    (should
     (equal
      (concat
       "| id | login    | email             | first_name | last_name | register_date         |\n"
       "|----+----------+-------------------+------------+-----------+-----------------------|\n"
       "|  1 | admin    | admin@mail.com    | John       | Doe       | 2012-12-14 17:25:03.0 |\n"
       "|  2 | neo      | neo@mail.com      | Thomas     | Anderson  | 2012-12-14 17:25:03.0 |\n"
       "|  3 | morpheus | morpheus@mail.com | Nil        | Nil       | 2012-12-14 17:25:03.0 |\n")
      (ejc-test:run-sql
       (concat
        "/                                                       \n"
        "SELECT * FROM users;                                    \n"))))
    ;; -----------------------------
    ;; Check cache for auto-complete
    (should
     (equal
      '("USERS")
      (progn
        (with-current-buffer (ejc-get-temp-editor-buffer "test")
          (auto-complete-mode t)
          (ejc-ac-setup)
          (insert "U")
          (auto-complete)
          ;; Wait for async cache creation.
          (sleep-for 15)
          (ejc-get-cached-tables-list ejc-db)))))
    (should
     (let ((actual-columns-list
            '("ID" "LOGIN" "EMAIL" "FIRST_NAME" "LAST_NAME" "REGISTER_DATE")))
       (equal actual-columns-list
              (-intersection
               actual-columns-list
               (progn
                 (with-current-buffer (ejc-get-temp-editor-buffer "test")
                   (auto-complete-mode t)
                   (ejc-ac-setup)
                   (insert "SERS.")
                   (auto-complete)
                   (ejc-get-cached-colomns-list ejc-db "USERS" t)))))))
    ;; ---------------------------------------------------
    ;; The connection is registered for the MCP server
    (should (member "H2-test-connection" (ejc-test:mcp-connection-names)))))


(ejc-test:fetch-h2-jar)

;; Keep the queries of the tests off the user's log, as `lein test' does.
(setenv "JVM_OPTS" (mapconcat #'identity
                              (delq nil (list (getenv "JVM_OPTS")
                                              "-Dejc-sql.log-dir=ejc-sql"))
                              " "))

(if noninteractive
    (clomacs-with-nrepl "ejc-sql"
      (lambda ()
        ;; (ert-run-tests-batch-and-exit '(tag el))
        ;; (ert-run-tests-batch-and-exit '(tag el+cl))
        (ert-run-tests-batch-and-exit t))))

(sit-for 60)
