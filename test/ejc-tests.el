(let ((current-directory (file-name-directory (or load-file-name ""))))
  (setq ejc-test-path (expand-file-name "." current-directory))
  (setq ejc-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path ejc-root-path)
(add-to-list 'load-path ejc-test-path)

(require 'cl)

(defun clomacs-get-connection (&optional library)
  (message "@@@ clomacs-get-connection - 2")
  (cider-current-repl))

(when (require 'undercover nil t)
  (undercover "*.el"))
(require 'ejc-sql)

(defun ejc-test:run-maven-dependency-plugin ()
  (print "Run maven-dependency-plugin")
  (print
   (shell-command-to-string
    (concat
     "mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get "
     "-Dartifact=com.h2database:h2:1.4.192"))))

(defun ejc-test:run-lein ()
  (print "Run lein test")
  (let ((result (shell-command-to-string "lein test")))
    (print result)
    result))

(ert-deftest ejc-test:run-lein-test ()
  :tags '(cl)
  (should
   (not
    (equal
     "Tests failed."
     (-last (lambda (x) (not (equal "" x)))
            (s-split "\n" (ejc-test:run-lein)))))))

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
           "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"))))

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
  (should (equal '(1 21)
                 (ejc-test:get-boundaries 1 "\nSELECT * FROM table\n/")))
  (should (equal '(26 44)
                 (ejc-test:get-boundaries 26 (concat "\n"
                                                     "SELECT * FROM table\n"
                                                     "/  \n"
                                                     "SELECT * FROM user"))))
  (should (equal '(1 23)
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

(cl-defun ejc-test:run-sql (sql &optional connect)
  ;; Type SQL query and eval it.
  (with-current-buffer (ejc-get-temp-editor-buffer "test")
    ;; Connect to test database, if bufer just created
    (message "@@@ ejc-test:run-sql")
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

(ert-deftest ejc-test:get-connection ()
  :tags '(el+cl)
  (let* ((db-path (concat "file://" (ejc-test:get-temp-path)
                          "database;AUTO_SERVER=TRUE"))
         (classpath (vector
                     (expand-file-name
                      "repository/com/h2database/h2/1.4.192/h2-1.4.192.jar"
                      (ejc-test:get-m2-path))))
         (conn (ejc-create-connection
                "H2-test-connection"
                :classpath classpath
                :classname "org.h2.Driver"
                :subprotocol "h2"
                :subname db-path
                :user "a_user"
                :password "secret")))
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
    (message "@@@ ejc-test:get-connection")
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
        "  id int(11) NOT NULL AUTO_INCREMENT,        \n"
        "  login varchar(45),                         \n"
        "  email varchar(45),                         \n"
        "  first_name varchar(255) NOT NULL,          \n"
        "  last_name varchar(45) NOT NULL,            \n"
        "  register_date datetime DEFAULT NULL,       \n"
        "  PRIMARY KEY (id),                          \n"
        "  UNIQUE KEY id_UNIQUE (id)                  \n"
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
                   (insert "SERS.")
                   (auto-complete)
                   (ejc-get-cached-colomns-list ejc-db "USERS" t)))))))))


(ejc-test:run-maven-dependency-plugin)

(when noninteractive
  (let ((nrepl-output
         (with-temp-buffer
           (insert-file-contents ".nrepl")
           (buffer-string)))
        (port (car
               (split-string
                (cadr (split-string nrepl-output
                                    "nREPL server started on port "))
                " "))))
    (ejc-connect-existing-repl "127.0.0.1" port))
  (clomacs-with-nrepl "ejc-sql"
    (lambda ()
      ;; (ert-run-tests-batch-and-exit '(tag el))
      ;; (ert-run-tests-batch-and-exit '(tag el+cl))
      (ert-run-tests-batch-and-exit t))))

(sit-for 60)
