;;; ejc-sql.el -- Uses clojure jdbc lib to eval sql scripts from emacs.

;;; Copyright © 2012 - Kostafey <kostafey@gmail.com>

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

;;; Commentary:

;; The aim is to make access to SQL databases through emacs easy.

;;; Usage:

;; The configuration of ejs-sql might looks like this:
;;
;; ; Append ejs-sql to `load-path':
;; (defvar site-lisp-path "~/.emacs.d/")
;; (add-to-list 
;;  'load-path 
;;  (expand-file-name "ejc-sql/src/ejc_sql/" site-lisp-path))
;;
;; (require 'ejc-sql)
;;
;; ; Create your database connection configuration:
;; (setq my-db-connection (make-ejc-db-conn
;;                         :classpath (concat 
;;                                     "/home/user/lib/"
;;                                     "mysql-connector-java-3.1.13-bin.jar")
;;                         :classname "com.mysql.jdbc.Driver"
;;                         :subprotocol "mysql"
;;                         :subname "//localhost:3306/my_db_name"
;;                         :user "a_user"
;;                         :password "secret"))
;;
;; ; Some keybindings - modify this on your taste:
;; (global-set-key (kbd "C-x S") 'ejc-eval-user-sql-region)
;; (global-set-key (kbd "C-x s") 'ejc-eval-user-sql-at-point)
;; (global-set-key (kbd "C-x <up>") 'ejc-show-last-result)
;; (global-set-key (kbd "C-x C-s") 'ejc-switch-to-sql-editor-buffer)
;;
;; * Using ejc-sql reqires nrepl process is running, so execution
;; `ejc-ensure-nrepl-runnig' ensures this.
;;
;; * Run to connect (ejc-connect "my-db-connection")
;; or M-x ejc-connect <RET> my-db-connection <RET>
;;
;; * `ejc-toggle-popup-results-buffer' -- Swithes between auto hidding results
;; buffer, or not.
;; 
;; * `ejc-eval-user-sql-at-point' -- Evaluate SQL bounded by the
;; `sql-separator' or/and buffer boundaries. 

(require 'cl)
(require 'nrepl)
(require 'popwin)

(defvar results-buffer nil
  "The results buffer.")
(defvar results-buffer-name "sql_output.txt" ; "*ejc-sql-output*"
  "The results buffer name.")
(defvar ejc-sql-editor-buffer-name "*sql-editor*"
  "The buffer for conveniently edit ad-hoc SQL scripts.")
(defvar results-file-path nil ;; "/<path>/sql_output.txt"
  "This value is returned by the clojure side.")
(defvar clojure-src-file "connect.clj"
  "Main clojure src file name.")
(defvar sql-separator "/"
  "The char with purpose to separate the SQL statement both other.")
(defvar ejc-popup-results-buffer t
  "Swithes between `popwin:popup-buffer' and `popwin:display-buffer'.")

(defstruct ejc-db-conn
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
  (subname (concat
            "://<db-host>:<db-port>:"
            "<db-server>=<server-name>;" 
            "database=<db-name>;"
            "DB_LOCALE=<locale>;"
            "CLIENT_LOCALE=<locale>;"
            ))
  (user "<user-name>")
  (password "<password>"))

(defun ejc-connect (arg)
  "Connect to selected db."
  (interactive "sDataBase connection name: ")
  (let ((db (eval (intern arg))))
    (message "Connection started...")
    (if (ejc-ensure-nrepl-runnig)
        (progn
          (ejc-load-clojure-side)
          (ejc-connect-to-db db)
          (message "Connected.")))))

(defun ejc-ensure-nrepl-runnig ()
  "Ensures nrepl is runnig.
If not, launch it, return nil. Return t otherwise."  
  (interactive)
  (let ((is-running t))
    (when (not (ejc-is-nrepl-runnig))
      (setq is-running nil)
      (ejc-launch-nrepl))
    is-running))

(defun ejc-is-nrepl-runnig ()
  "Return t if nrepl process is running, nil otherwise."
  (let ((ncb (get-buffer nrepl-connection-buffer)))
    (save-excursion
      (if (and nrepl-connection-buffer 
               (buffer-live-p ncb)
               (progn
                 (set-buffer ncb)
                 nrepl-session))
          t nil))))

(defun ejc-launch-nrepl ()
  ;; TODO: It looks like ad-hoc implementation, and it is, surely :).
  (set-buffer (find-file-noselect (ejc-find-clojure-file)))
  (nrepl-jack-in))

(defun ejc-find-clojure-file ()
  "Return the full path to `clojure-src-file'."
  (let ((result))
    (dolist (path load-path)
      (let ((clojure-scr-file-path (expand-file-name clojure-src-file path)))
        (if (file-exists-p clojure-scr-file-path)
            (setq result clojure-scr-file-path))))
    (if (not result)
        (error (concat "Can't find file " clojure-src-file))
      result)))

(defun ejc-load-clojure-side ()
  "Evaluate clojure side, run startup initialization functions."
  (if (not results-file-path)
      (progn
        (nrepl-load-file (ejc-find-clojure-file))
        (setq results-file-path
              (plist-get (nrepl-eval 
                          (concat
                           " (in-ns 'ejc-sql.core)"
                           " (print output-file-path)")) :stdout)))))

(defun add-quotes (str)
  (concat "\"" str "\""))

(defun ejc-connect-to-db (conn-struct)
  (nrepl-eval 
   (concat 
    " (in-ns 'ejc-sql.core)"
    " (add-to-cp " (add-quotes (ejc-db-conn-classpath conn-struct)) ")"
    " (import " (ejc-db-conn-classname conn-struct)")"
    " (def db {:classname " (add-quotes (ejc-db-conn-classname conn-struct))
    "          :subprotocol " (add-quotes (ejc-db-conn-subprotocol conn-struct))
    "          :subname " (add-quotes (ejc-db-conn-subname conn-struct))
    "          :user " (add-quotes (ejc-db-conn-user conn-struct))
    "          :password " (add-quotes (ejc-db-conn-password conn-struct))
    "         })"    
    )))

(defun ejc-create-output-buffer ()
  (set-buffer (get-buffer-create results-buffer-name))
  (setq results-buffer (current-buffer))
  (set-visited-file-name results-file-path t t)  
  (setq view-read-only t)
  results-buffer)

(defun ejc-get-output-buffer ()
  (if (and results-buffer (buffer-live-p results-buffer))
      results-buffer
    (ejc-create-output-buffer)))

(defun ejc-eval-user-sql (sql)
  "Evaluate SQL, reload and show query results buffer."
    (message "Processing SQL query...")
    (ejc-eval-sql sql)
    (ejc-show-last-result t)
    (message "Done SQL query."))

(defun ejc-toggle-popup-results-buffer ()
  (interactive)
  (setq ejc-popup-results-buffer (not ejc-popup-results-buffer)))

(defun ejc-show-last-result (&optional revert)
  "Popup buffer with last SQL execution result output."
  (interactive)
  (let ((output-buffer (ejc-get-output-buffer)))
    (set-buffer output-buffer)
    (when revert
      (revert-buffer t t))
    (toggle-read-only t)
    (beginning-of-buffer)
    (if ejc-popup-results-buffer
        (popwin:popup-buffer output-buffer)
      (popwin:display-buffer output-buffer))))

(defun ejc-eval-sql (sql)
  "Core function to evaluate SQL queries."
  (nrepl-eval
   ;; nrepl-eval-async
   (concat "(eval-user-sql" (add-quotes sql) ")")))

(defun ejc-eval-user-sql-region (beg end)
  "Evaluate SQL bounded by the selection area."
  (interactive "r")
  (let ((sql (buffer-substring beg end)))
    (ejc-eval-user-sql sql)))

(defun ejc-eval-user-sql-at-point ()
  "Evaluate SQL bounded by the `sql-separator' or/and buffer boundaries."
  (interactive)  
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries)))
         (sql (buffer-substring beg end)))
    (ejc-eval-user-sql sql)))

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

;; TODO: wrong
(defun ejc-mark-this-sql ()
  (interactive)
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries))))
    (setq mark-active nil)
    (goto-char beg)
    (setq mark-active t)
    (goto-char end)))

(defun apply-in-sql-boundaries (func)
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries))))
    ;; (message beg)
    ;; (message end)
    (apply func (list beg end))))

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
        (replace-string " or " "\n  or " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " order by " "\norder by \n" nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " inner join " "\ninner join " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " left join " "\nleft join " nil beg end)))
    (apply-in-sql-boundaries 
     '(lambda (beg end)
        (replace-string " on " "\n  on " nil beg end)))
    ))

(defun ejc-format-sql-at-point ()
  (interactive)  
  (let* ((boundaries (get-sql-boundaries-at-point))
         (beg (car boundaries))
         (end (car (cdr boundaries))))
    (format-sql beg end)))

(defun ejc-create-sql-editor-buffer ()
  "Create buffer dedicated to ad-hoc edit and SQL scripts."
  (let ((sql-editor-buffer (get-buffer-create ejc-sql-editor-buffer-name)))
    (save-excursion
      (set-buffer sql-editor-buffer)
      (sql-ansi-mode)
      (auto-complete-mode t)
      (auto-fill-mode t))
    sql-editor-buffer))

(defun ejc-switch-to-sql-editor-buffer ()
  "Switch to buffer dedicated to ad-hoc edit and SQL scripts.
If the buffer is not exists - create it."
  (interactive)
  (let ((sql-editor-buffer (get-buffer ejc-sql-editor-buffer-name)))
    (switch-to-buffer
     (if (and sql-editor-buffer
              (buffer-live-p sql-editor-buffer))
         sql-editor-buffer
       (ejc-create-sql-editor-buffer)))))

;; (shell-command "lein repl :headless")

(provide 'ejc-sql)

