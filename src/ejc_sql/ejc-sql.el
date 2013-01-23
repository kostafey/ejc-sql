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
;; (global-set-key (kbd "C-x <up>") 'ejc-show-last-result)
;; (global-set-key (kbd "C-x C-s") 'ejc-switch-to-sql-editor-buffer)
;;
;; New keybindings added to `sql-mode-map':
;; * (kbd "C-c C-c") 'ejc-eval-user-sql-at-point
;; * (kbd "C-x t")   'ejc-toggle-popup-results-buffer
;; * (kbd "C-h t")   'ejc-describe-table
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
;; `ejc-sql-separator' or/and buffer boundaries. 

(require 'cl)
(require 'sql)
(require 'nrepl)
(require 'popwin)
(require 'ejc-format)
(require 'ejc-autocomplete)

(define-key sql-mode-map (kbd "C-c C-c") 'ejc-eval-user-sql-at-point)
(define-key sql-mode-map (kbd "C-x t") 'ejc-toggle-popup-results-buffer)
(define-key sql-mode-map (kbd "C-h t") 'ejc-describe-table)

(defvar ejc-db-type nil
  "The type of RDBMS.")

(defvar ejc-results-buffer nil
  "The results buffer.")
(defvar ejc-results-buffer-name "sql_output.txt" ; "*ejc-sql-output*"
  "The results buffer name.")
(defvar ejc-results-file-path nil ;; "/<path>/sql_output.txt"
  "This value is returned by the clojure side.")

(defvar ejc-sql-editor-buffer-name "*sql-editor*"
  "The buffer for conveniently edit ad-hoc SQL scripts.")

(defvar ejc-sql-log-file-path nil
  "SQL scripts logs filepath.")
(defvar ejc-sql-log-buffer-name "sql_log.txt"
  "The buffer for view SQL scripts logs.")

(defvar clojure-src-file "connect.clj"
  "Main clojure src file name.")

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

(defun ejc-get-nrepl-stdout (expr)
  "Evaluate `expr', print it and return printed text as function's result."
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
  (if (not ejc-results-file-path)
      (progn
        (nrepl-load-file (ejc-find-clojure-file))
        (setq ejc-results-file-path 
              (ejc-get-nrepl-stdout "output-file-path"))
        (setq ejc-sql-log-file-path 
              (ejc-get-nrepl-stdout "sql-log-file-path")))))

(defun ejc-add-quotes (str)
  (concat "\"" str "\""))

(defun ejc-connect-to-db (conn-struct)
  (nrepl-eval 
   (concat 
    " (in-ns 'ejc-sql.core)"
    " (ejc-sql.lib/add-to-cp " (ejc-add-quotes (ejc-db-conn-classpath conn-struct)) ")"
    " (import " (ejc-db-conn-classname conn-struct)")"
    " (def db {:classname " (ejc-add-quotes (ejc-db-conn-classname conn-struct))
    "          :subprotocol " (ejc-add-quotes (ejc-db-conn-subprotocol conn-struct))
    "          :subname " (ejc-add-quotes (ejc-db-conn-subname conn-struct))
    "          :user " (ejc-add-quotes (ejc-db-conn-user conn-struct))
    "          :password " (ejc-add-quotes (ejc-db-conn-password conn-struct))
    "         })"    
    ))
  (setq ejc-db-type (ejc-db-conn-subprotocol conn-struct)))

(defun ejc-get-word-at-point (pos)
  "Return SQL word around the point."
  (interactive "d")
  (let* ((char (char-after pos))
         (str (char-to-string char)))
    (save-excursion
      (let* ((end (if (member str '(" " ")" "<" ">" "="))
                      (point)
                    (progn
                      (forward-same-syntax 1)
                      (point))))
             (beg (progn
                    (forward-same-syntax -1)
                    (point)))
             (sql-word (buffer-substring beg end)))
        sql-word))))

(defun ejc-describe-table (table-name)
  "Describe SQL table `table-name' (default table name - word around the point)."
  (interactive
   (let ((sql-symbol (ejc-get-word-at-point (point)))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if sql-symbol
				    (format "Describe table (default %s): " sql-symbol)
				  "Describe table: ")
				obarray))
     (list (if (equal val "")
               sql-symbol
             val))))
  (ejc-get-nrepl-result 
   (concat "(get-table-meta " (ejc-add-quotes table-name) ")"))
  (ejc-show-last-result t))

(defun ejc-eval-user-sql (sql)
  "Evaluate SQL by user: reload and show query results buffer, update log."
    (message "Processing SQL query...")
    (ejc-eval-sql sql)
    (ejc-show-last-result t)
    (save-excursion
      (set-buffer (ejc-get-buffer-or-create
                   ejc-sql-log-buffer-name
                   'ejc-create-sql-log-buffer))
      (revert-buffer t t))
    (message "Done SQL query."))

(defun ejc-eval-sql (sql)
  "Core function to evaluate SQL queries."
  (nrepl-eval
   ;; nrepl-eval-async
   (concat "(eval-user-sql" (ejc-add-quotes sql) ")")))

(defun ejc-eval-user-sql-region (beg end)
  "Evaluate SQL bounded by the selection area."
  (interactive "r")
  (let ((sql (buffer-substring beg end)))
    (ejc-eval-user-sql sql)))

(defun ejc-eval-user-sql-at-point ()
  "Evaluate SQL bounded by the `ejc-sql-separator' or/and buffer boundaries."
  (interactive)  
  (ejc-eval-user-sql (ejc-get-sql-at-point)))

;;-----------------------------------------------------------------------------
;; results buffer
;;
(defun ejc-create-output-buffer ()
  (set-buffer (get-buffer-create ejc-results-buffer-name))
  (setq ejc-results-buffer (current-buffer))
  (set-visited-file-name ejc-results-file-path t t)
  (setq view-read-only t)
  ejc-results-buffer)

(defun ejc-get-buffer-or-create (buffer-or-name create-buffer-fn)
  "Return buffer passed in `buffer-or-name' parameter.
If this buffer is not exists or it was killed - create buffer via
`create-buffer-fn' function (this function must return buffer)."
  (let ((buf (if (bufferp buffer-or-name) 
                 buffer-or-name 
               (get-buffer buffer-or-name))))
    (if (and buf (buffer-live-p buf))
        buf
      (apply create-buffer-fn nil))))

(defun ejc-get-output-buffer ()
  (if (and ejc-results-buffer (buffer-live-p ejc-results-buffer))
      ejc-results-buffer
    (ejc-create-output-buffer)))

(defun ejc-toggle-popup-results-buffer ()
  (interactive)
  (setq ejc-popup-results-buffer (not ejc-popup-results-buffer))
  (if ejc-popup-results-buffer
      (message "Popup window.")
    (message "Normal window.")))

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
;;
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; editor buffer
;;
(defun ejc-create-sql-editor-buffer ()
  "Create buffer dedicated to ad-hoc edit and SQL scripts."
  (let ((sql-editor-buffer (get-buffer-create ejc-sql-editor-buffer-name)))
    (save-excursion
      (set-buffer sql-editor-buffer)
      (sql-ansi-mode)
      (auto-complete-mode t)
      (auto-fill-mode t)
      (ejc-ac-setup))
    sql-editor-buffer))

(defun ejc-switch-to-sql-editor-buffer ()
  "Switch to buffer dedicated to ad-hoc edit and SQL scripts.
If the buffer is not exists - create it."
  (interactive)
  (switch-to-buffer
   (ejc-get-buffer-or-create
    ejc-sql-editor-buffer-name
    'ejc-create-sql-editor-buffer)))
;;
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; log buffer
;;
(defun ejc-create-sql-log-buffer ()
  (let ((sql-log-buffer (get-buffer-create ejc-sql-log-buffer-name)))
    (save-excursion
      (set-buffer sql-log-buffer)
      (set-visited-file-name ejc-sql-log-file-path t t)
      (setq view-read-only t)
      (sql-ansi-mode)
      (auto-complete-mode t)
      (auto-fill-mode t))
    sql-log-buffer))

(defun ejc-switch-to-sql-log-buffer ()
  (interactive)
  (switch-to-buffer
   (ejc-get-buffer-or-create
    ejc-sql-log-buffer-name
    'ejc-create-sql-log-buffer)))
;;
;;-----------------------------------------------------------------------------

;; (shell-command "lein repl :headless")

(provide 'ejc-sql)

