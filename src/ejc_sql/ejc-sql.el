(require 'cl)
(require 'nrepl)

(defvar results-buffer-name "sql_output.txt")
(defvar results-file-name "temp.txt")
(defvar results-file-path nil) ;"sql_output.txt"
(defvar clojure-src-file "connect.clj")
(defvar sql-separator "/")

(defstruct db-conn
  "DB connection information structure"
                                        ; the JDBC driver class
  (classname "<com.povider.jdbc.DataBaseDriver>")
                                        ; the kind of database, e.g:
                                        ; informix-sqli, mysql, postgresql,
                                        ; oracle, sqlserver, etc.
  (subprotocol "<sql-kind>") 
                                        ; db connection path
                                        ; locale, like ru_RU.1251
  (subname (concat
            "://<db-host>:<db-port>:"
            "<db-server>=<server-name>;" 
            "database=<db-name>;"
            "DB_LOCALE=<locale>;"
            "CLIENT_LOCALE=<locale>;"
            ))
  (user "<user-name>")
  (password "<password>"))

(defun launch-nrepl () 
  (nrepl-jack-in))

;; TODO:
(defun load-clojure-side ()
  (let ((clojure-scr-dir default-directory))
    (if (file-exists-p (expand-file-name clojure-src-file clojure-scr-dir))
        (progn
          (nrepl-load-file clojure-src-file)
          (setq results-file-path 
                (plist-get (nrepl-eval "(print output-file-path)") :stdout))
      (error (concat "Can't find file " clojure-src-file)))))

;; nrepl-eval-buffer

(defun add-quotes (str)
  (concat "\"" str "\""))

(defun connect-to-db (conn-struct)
  (nrepl-eval 
   (concat 
    " (def db {:classname " (add-quotes (db-conn-classname conn-struct))
    "          :subprotocol " (add-quotes (db-conn-subprotocol conn-struct))
    "          :subname " (add-quotes (db-conn-subname conn-struct))
    "          :user " (add-quotes (db-conn-user conn-struct))
    "          :password " (add-quotes (db-conn-password conn-struct))
    "         })"
    )))

(defun create-output-buffer ()
    (toggle-read-only t)
    (setq view-read-only t))

(defun eval-user-sql (sql)
  (progn    
    (nrepl-eval 
     ;; nrepl-eval-async
     (concat "(eval-user-sql" (add-quotes sql) ")"))

    (set-buffer (get-buffer-create results-buffer-name))
    ;(set-visited-file-name results-file-path)

    ;; (set-visited-file-name
    ;;  (expand-file-name 
    ;;   results-file-name
    ;;   (file-name-as-directory 
    ;;    (expand-file-name ".." (expand-file-name ".." default-directory)))))
    (revert-buffer t t)))

(defun eval-user-sql-region (beg end)
  (interactive "r")
  (let ((sql (buffer-substring beg end)))
    (eval-user-sql sql)))

(defun get-sql-boundaries-at-point ()
  "Returns list of the boundaries of the current sql expression.
The current sql expression is the expression under the point.
The boundaries are marked by `sql-separator's. If the top or
bottom boundary is absent - it returns beginning or end of the
buffer."
  (save-excursion
    (let* ((beg (progn
                  (if (search-backward sql-separator nil t nil)
                      (forward-char)
                    (beginning-of-buffer))
                  (point)))
           (end (progn
                  (if (search-forward sql-separator nil t nil)
                      (backward-char)
                    (end-of-buffer))
                  (point))))
      (list beg end))))

(defun apply-in-sql-boundaries (func)
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries))))
    ;; (message beg)
    ;; (message end)
    (apply func (list beg end))))

(defun eval-user-sql-at-point ()
  (interactive)  
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries)))
         (sql (buffer-substring beg end)))
    (eval-user-sql sql)))

(defun format-sql (beg end)
  (interactive "r")
  (save-excursion
    ;; (setq mark-active nil)
    ;; (goto-char beg)
    ;; (setq mark-active t)
    ;; (goto-char end)
    (apply-in-sql-boundaries 
     '(lambda (beg end) 
        (replace-string "," ",\n    " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string "select" "select \n    " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " from " "\nfrom \n     " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " where " "\nwhere \n     " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " and " "\n and " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " or " "\n or " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " order by " "\norder by \n" nil beg end)))))

(defun format-sql-at-point ()
  (interactive)  
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries))))
    (format-sql beg end)))

;; (shell-command "lein repl :headless")

(provide 'ejc-sql)

