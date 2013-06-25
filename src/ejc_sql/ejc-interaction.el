(require 'ejc-lib)

(defvar ejc-nrepl-connrection-buffer-name (nrepl-connection-buffer-name))

(defvar ejc-clojure-src-file "connect.clj"
  "Main clojure src file name.")

(defvar ejc-clojure-offline-file "clojure_offline.clj"
  "Clojure-offline src helper file name.")

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
  (let ((ncb (get-buffer ejc-nrepl-connrection-buffer-name)))
    (save-excursion
      (if (and ejc-nrepl-connrection-buffer-name
               (buffer-live-p ncb)
               (progn
                 (set-buffer ncb)
                 nrepl-session))
          t nil))))

(defun ejc-find-clojure-main-file ()
  "Return the full path to `ejc-clojure-src-file'."
  (ejc-find-file-in-load-path ejc-clojure-src-file))

(defun ejc-find-clojure-offline-file ()
  "Return the full path to `ejc-clojure-offline-file'."
  (ejc-find-file-in-load-path ejc-clojure-offline-file))

(defun ejc-launch-nrepl ()
  ;; TODO: It looks like ad-hoc implementation, and it is, surely :).
  (set-buffer (find-file-noselect (ejc-find-clojure-main-file)))
  (nrepl-jack-in))

(defun ejc-get-nrepl-stdout (expr)
  "Evaluate `expr', print it and return printed text as function's result."
  ;; nrepl-eval-async
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
  (if (not ejc-sql-log-file-path)
      (progn
        ;; load clojure-offline lib
        (nrepl-load-file (ejc-find-clojure-offline-file))
        ;; add clojure-side files to classpath
        (nrepl-eval
         (concat
          " (ejc-sql.clojure-offline/add-to-cp "
          (ejc-add-quotes
           (file-name-directory
            (expand-file-name ".." (ejc-find-clojure-main-file)))) ")"))
        ;; load ejc-sql main clojure-side file
        (nrepl-load-file (ejc-find-clojure-main-file))
        (setq ejc-sql-log-file-path
              (ejc-get-nrepl-stdout "sql-log-file-path")))))

(defun ejc-connect-to-db (conn-struct)
  (nrepl-eval
   (concat
    " (in-ns 'ejc-sql.core)"
    " (ejc-sql.clojure-offline/add-to-cp "
    (ejc-add-quotes (ejc-db-conn-classpath conn-struct)) ")"
    " (import " (ejc-db-conn-classname conn-struct)")"
    " (def db {:classname " (ejc-add-quotes
                             (ejc-db-conn-classname conn-struct))
    "          :subprotocol " (ejc-add-quotes
                               (ejc-db-conn-subprotocol conn-struct))
    "          :subname " (ejc-add-quotes
                           (ejc-db-conn-subname conn-struct))
    "          :user " (ejc-add-quotes
                        (ejc-db-conn-user conn-struct))
    "          :password " (ejc-add-quotes
                            (ejc-db-conn-password conn-struct))
    "         })"
    ))
  (setq ejc-db-type (ejc-db-conn-subprotocol conn-struct))
  (setq ejc-db-owner (ejc-db-conn-owner conn-struct))
  (setq ejc-db-name (ejc-get-db-name (ejc-db-conn-subname conn-struct))))

(defun ejc-eval-sql (sql)
  "Core function to evaluate SQL queries."
  (if sql
      (let* ((prepared-sql (replace-regexp-in-string "\"" "'" sql))
             (result (ejc-get-nrepl-stdout
                      (concat "(eval-user-sql " (ejc-add-quotes prepared-sql)
                              ")"))))
        result)
    ""))

(defun ejc--eval-get-column (sql)
  (ejc-get-nrepl-stdout
   (concat "(eval-sql-internal-get-column "
           (ejc-add-quotes sql) " )")))

(defun ejc--eval-get-list (sql)
  (let ((sql-result-string (ejc--eval-get-column sql)))
    (split-string
     (substring sql-result-string
                1 (- (length sql-result-string) 1)))))

(defun ejc-get-table-meta (table-name)
  (ejc-get-nrepl-stdout
   (concat "(get-table-meta " (ejc-add-quotes table-name) ")")))

(provide 'ejc-interaction)
