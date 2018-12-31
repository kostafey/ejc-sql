(let ((current-directory (file-name-directory (or load-file-name ""))))
  (setq ejc-test-path (expand-file-name "." current-directory))
  (setq ejc-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path ejc-root-path)
(add-to-list 'load-path ejc-test-path)

(require 'cl)

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
  (with-current-buffer (get-buffer-create ejc-temp-editor-buffer-name)
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

(ert-deftest ejc-test:get-connection ()
  :tags '(el+cl)
  (let* ((db-path (concat "file://" (ejc-test:get-temp-path)
                          "database;AUTO_SERVER=TRUE"))
         (classpath (expand-file-name
                     "repository/com/h2database/h2/1.4.192/h2-1.4.192.jar"
                     (ejc-test:get-m2-path)))
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
    ;; Delete previous run temp database files
    (mapcar (lambda (x)
              (let ((path-to-x (expand-file-name x (ejc-test:get-temp-path))))
                (if (file-exists-p path-to-x) (delete-file path-to-x))))
            '("database.mv.db" "database.trace.db"))
    ;; Ensure temp SQL editor buffer is empty.
    (if (get-buffer ejc-temp-editor-buffer-name)
        (kill-buffer ejc-temp-editor-buffer-name))
    ;; ------------
    ;; Create table
    (should
     (equal
      "Records affected: 0"
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
      "Records affected: 1"
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
      "Records affected: 3"
      (ejc-test:run-sql
       (concat
        "/                                                       \n"
        "UPDATE users SET register_date = '2012-12-14 17:25:03'; \n"))))
    ;; -----------
    ;; Select data
    (should
     (equal
      (concat
       "id | login    | email             | first_name | last_name | register_date        \n"
       "---+----------+-------------------+------------+-----------+----------------------\n"
       "1  | admin    | admin@mail.com    | John       | Doe       | 2012-12-14 17:25:03.0\n"
       "2  | neo      | neo@mail.com      | Thomas     | Anderson  | 2012-12-14 17:25:03.0\n"
       "3  | morpheus | morpheus@mail.com | Nil        | Nil       | 2012-12-14 17:25:03.0\n")
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
        (with-current-buffer
            (get-buffer-create ejc-temp-editor-buffer-name)
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
                 (with-current-buffer
                     (get-buffer-create ejc-temp-editor-buffer-name)
                   (insert "SERS.")
                   (auto-complete)
                   (ejc-get-cached-colomns-list ejc-db "USERS" t)))))))))


(ejc-test:run-maven-dependency-plugin)

;; (ert-run-tests-batch-and-exit '(tag el))
;; (ert-run-tests-batch-and-exit '(tag el+cl))
(ert-run-tests-batch-and-exit t)
