;; (ns ejc-sql.core)

(use 'clojure.java.io)
(use 'clojure.java.jdbc)
(import com.informix.jdbc.IfxDriver)

(def db "DataBase connection properties list." nil)

(def isWindows
  "The value is true if it runs under the os Windows."
  (>= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(def isLinux
  "The value is true if it runs under the os Linux."
  (>= 0 (.indexOf (System/getProperty "os.name") "Linux")))

(def output-file-path 
  "The sql queries results output filepath."
  (str (System/getProperty  "user.home")
       (if (true? isWindows)
         "/Application Data")
       "/.emacs.d/tmp/sql_output.txt"))

(defn get-user-output-file-path []
  (-> (java.io.File. output-file-path) .getAbsolutePath))

(defn eval-sql [sql, get-output-file-path]
  (with-connection db 
    (with-query-results rs [sql] 
      (with-open 
          [wrtr (writer (get-output-file-path))]                
        (.write wrtr (format-output-header rs))
        (doseq [row rs]
          (.write wrtr (str 
                        (clojure.string/join (for [[_ v] row] 
                                               (str v " "))) "\n")))
        ))))

(defn format-output-header [rs]
  (str 
   (clojure.string/join 
    (for [[k _] (first rs)] 
      (subs (str k " ") 1))) "\n"
      "----------\n"))

;; (defn eval-sql [sql, get-output-file-path]
;;   (with-connection db 
;;     (with-query-results rs [sql] 
;;       (with-open 
;;           [wrtr (writer (get-output-file-path))]
;;         (doseq [row rs] (.write wrtr (str  row "\n")))
;;         ))))

(defn eval-user-sql [sql]
  (eval-sql sql get-user-output-file-path))

;; (eval-user-sql "
;; SELECT TRIM(t.tabname) || '.' || TRIM(c.colname) AS table_dot_column
;;   FROM \"informix\".systables AS t, \"informix\".syscolumns AS c
;;  WHERE t.tabid = c.tabid
;;    AND t.tabtype = 'T'
;;    AND t.tabid >= 100
;;  ORDER BY t.tabname, c.colno;
;; ")

;; (eval-user-sql " SELECT superregions.* from superregions ")

;; (def mm {:key "value" :key2 "value2"})


;; (print (clojure.string/join (for [[_ v] mm] 
;;        (str v " "))))

