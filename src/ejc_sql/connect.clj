;; (ns ejc-sql.core)

(use 'clojure.java.io)
(use 'clojure.java.jdbc)
(import com.informix.jdbc.IfxDriver)
;; (import com.mysql.jdbc.Driver)
(import java.sql.SQLException)

(def db "DataBase connection properties list." nil)

(def isWindows
  "The value is true if it runs under the os Windows."
  (<= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(def isLinux
  "The value is true if it runs under the os Linux."
  (<= 0 (.indexOf (System/getProperty "os.name") "Linux")))

(def output-file-path
  "The sql queries results output filepath."
  (str (System/getProperty  "user.home")
       (if (true? isWindows)
         "/Application Data")
       "/.emacs.d/tmp/sql_output.txt"))

(defn get-user-output-file-path []
  (-> (java.io.File. output-file-path) .getAbsolutePath))

(defn eval-user-sql [sql]
  (eval-sql sql get-user-output-file-path))

(defn eval-sql [sql, get-output-file-path]
(try 
  (with-connection db 
    (with-query-results rs [sql] 
      (with-open 
          [wrtr (writer (get-output-file-path))]                
        (.write wrtr (format-output rs)))))

  (catch SQLException e 
    (with-open 
        [wrtr (writer (get-output-file-path))]
      (.write wrtr (str "Error: " (.getMessage e)))))))


(defn format-output [rs]
  (let [records-data (filter-data (get-rs-data rs))
        headers-data (get-rs-headers rs)
        longest-list (find-longest-list 
                      (get-rs-lengths (cons headers-data records-data)))
        col-step 2
        result (new StringBuffer "")]

    (doseq [val (map vector longest-list headers-data)]
      (.append result (format (str "%-" (get val 0) "s") (get val 1)))
      (.append result (simple-join col-step " ")))

    (.append result "\n")

    (doseq [len longest-list]
      (.append result (simple-join len "-"))
      (.append result (simple-join col-step " ")))

    (.append result "\n")

    (doseq [row records-data]
      (doseq [val (map vector longest-list row)]
        (.append result (format (str "%-" (get val 0) "s") (get val 1) ))
        (.append result (simple-join col-step " ")))
      (.append result "\n"))

    (.toString result)))


(defn simple-join [n s]
  (clojure.string/join 
   (for [x (range 0 n)] s)))

(defn str-length [s]
  (.length (str s)))

(defn get-rs-lengths [rs]
  (map #(map str-length %) rs))

(defn get-rs-data [rs]
  (map vals rs))

(defn filter-data [rs]
  (map #(map trim %) rs))

(defn trim [s]
  (if (instance? java.lang.String s)
    (.trim s)
    s))

(defn get-rs-headers [rs]  
   (for [[k _] (first rs)] 
     (subs (str k) 1)))

(defn transpose [m]
  (apply mapv vector m))

(defn find-longest-list
  "Returns the list of the longest lengths per column."
  [lst]
  (map #(apply max %) (transpose lst)))


;; (defn eval-sql [sql, get-output-file-path]
;;   (with-connection db 
;;     (with-query-results rs [sql] 
;;       (with-open 
;;           [wrtr (writer (get-output-file-path))]
;;         (doseq [row rs] (.write wrtr (str  row "\n")))
;;         ))))


;; (eval-user-sql "
;; SELECT TRIM(t.tabname) || '.' || TRIM(c.colname) AS table_dot_column
;;   FROM \"informix\".systables AS t, \"informix\".syscolumns AS c
;;  WHERE t.tabid = c.tabid
;;    AND t.tabtype = 'T'
;;    AND t.tabid >= 100
;;  ORDER BY t.tabname, c.colno;
;; ")


;; (def mm {:key "value" :key2 "value2"})


;; (print (clojure.string/join (for [[_ v] mm] 
;;        (str v " "))))

;; (print
;;  (format-output rs-dd))

;; (def rs-dd 
;;  '({:id "1", :name "Some text",  :dataname "qweqweqwe"}
;;    {:id "2", :name "Other text", :dataname "asdsdfsdf"}
;;    {:id "3", :name "More text",  :dataname "sdfsdfjkllk"}
;;    ))

;; (eval-user-sql " SELECT superregions.* from superregions ")

;; (with-connection db 
;;   (with-query-results rs 
;;     [" SELECT superregions.* from superregions "] 
;;     (doseq [row rs] (println  row))))

