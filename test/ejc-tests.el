(let ((current-directory (file-name-directory load-file-name)))
  (setq ejc-test-path (expand-file-name "." current-directory))
  (setq ejc-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path ejc-root-path)
(add-to-list 'load-path ejc-test-path)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(prefer-coding-system 'utf-8)
(package-initialize)
(package-refresh-contents)

(mapcar (lambda (p) (when (not (package-installed-p p))
                 (package-install p)))
        '(dash s auto-complete cider clomacs undercover))

(require 'cl)

(when (require 'undercover nil t)
  (undercover "../*.el"))
(require 'ejc-sql)

(defun ejc-test:run-maven-dependency-plugin ()
  (print "Run maven-dependency-plugin")
  (print
   (shell-command-to-string
    (concat
     "mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get "
     "-Dartifact=com.h2database:h2:1.4.192"))))

(ert-deftest ejc-test:get-log-file-path ()
  :tags '(el+cl)
  (let ((log-file-path (ejc-get-log-file-path)))
    (should (stringp log-file-path))
    (print (format "Log file path: %s" log-file-path))))

(ert-deftest ejc-test:get-connection ()
  :tags '(el)
  (let* ((db-path (concat "file://" default-directory
                          "database;AUTO_SERVER=TRUE"))
         (conn (ejc-create-connection
                "H2-test-connection"
                :classpath "~/.m2/repository/com/h2database/h2/1.4.192/h2-1.4.192.jar"
                :classname "org.h2.Driver"
                :subprotocol "h2"
                :subname db-path
                :user "a_user"
                :password "secret")))
    (should
     (equal
      `(("H2-test-connection" . [cl-struct-ejc-db-conn
                                 "~/.m2/repository/com/h2database/h2/1.4.192/h2-1.4.192.jar"
                                 "org.h2.Driver"
                                 "h2"
                                 nil
                                 ,db-path
                                 "a_user"
                                 "secret"
                                 nil
                                 nil]))
      conn))))

(ejc-test:run-maven-dependency-plugin)

;; (ert-run-tests-batch-and-exit '(tag el))
;; (ert-run-tests-batch-and-exit '(tag el+cl))
(ert-run-tests-batch-and-exit t)
