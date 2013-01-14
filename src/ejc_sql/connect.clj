(ns ejc-sql.core)

(in-ns 'ejc-sql.core)

(use 'clojure.java.io)
(use 'clojure.java.jdbc)
(require 'clojure.contrib.java-utils)

(import java.util.Date)
(import java.text.SimpleDateFormat)

(import (java.sql Connection 
                  DriverManager 
                  PreparedStatement 
                  ResultSet
                  SQLException))

(import (java.io File) 
        (java.net URL URLClassLoader) 
        (java.lang.reflect Method))

(def db "DataBase connection properties list." nil)

(defn add-to-cp "Since add-classpath is deprecated."
  [#^String jarpath] ; path without "file:///..." prefix.
  (let [#^URL url (.. (File. jarpath) toURI toURL) 
        url-ldr-cls (. (URLClassLoader. (into-array URL [])) getClass) 
        arr-cls (into-array Class [(. url getClass)]) 
        arr-obj (into-array Object [url]) 
        #^Method mthd (. url-ldr-cls getDeclaredMethod "addURL" arr-cls)] 
    (doto mthd 
      (.setAccessible true) 
      (.invoke (ClassLoader/getSystemClassLoader) arr-obj)) 
    (println (format "Added %s to classpath" jarpath))))

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

(def sql-log-file-path
  "The sql queries logging filepath."
  (str (System/getProperty  "user.home")
       (if (true? isWindows)
         "/Application Data")
       "/.emacs.d/tmp/sql_log.txt"))

(defn get-user-output-file-path []
  (-> (java.io.File. output-file-path) .getAbsolutePath))

(defn get-sql-log-file-path []
  (-> (java.io.File. sql-log-file-path) .getAbsolutePath))

(defn trim [s]
  (if (instance? java.lang.String s)
    (.trim s)
    s))

(defn filter-data [rs]
  (map #(map trim %) rs))

(defn get-rs-data [rs]
  (map vals rs))

(defn get-rs-headers [rs]  
  (for [[k _] (first rs)] 
    (subs (str k) 1)))

(defn transpose [m]
  (apply mapv vector m))

(defn find-longest-list
  "Returns the list of the longest lengths per column."
  [lst]
  (map #(apply max %) (transpose lst)))

(defn simple-join [n s]
  (clojure.string/join 
   (for [x (range 0 n)] s)))

(defn str-length [s]
  (.length (str s)))

(defn get-rs-lengths [rs]
  (map #(map str-length %) rs))

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

(defn eval-commands [sql wrtr] 
  (try
    (with-connection db 
      (let [res (str (with-connection db 
                       (clojure.java.jdbc/do-commands sql)))]
        (.write wrtr (str "Records affected: " res))))
    (catch SQLException e 
      (.write wrtr (str "Error: " (.getMessage e))))))

(defn log-sql [sql]
  (let [is-new-file (if (not (. (clojure.contrib.java-utils/file 
                                 (get-sql-log-file-path)) exists)) 
                      true false)]
    (with-open 
        [wrtr (writer (get-sql-log-file-path) :append true)]
      (if is-new-file
        (.write wrtr "-*- mode: sql; -*-*/\n"))
      (.write wrtr (str (simple-join 50 "-") " " 
                        (.format (new SimpleDateFormat "yyyy.MM.dd HH:mm:ss.S")
                                 (new Date))
                        " " (simple-join 2 "-") "\n" sql "\n")))))

(def select-on-manipulation-errors
  (list 
   "Method only for queries" ; informix
   "Can not issue data manipulation statements with executeQuery()." ; mySQL
   ))

(defn in? 
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn is-manipulation-error [err-msg]
  (in? (map #(.equals err-msg %) select-on-manipulation-errors) true))

(defn eval-sql [sql, get-output-file-path]
  (let [clear-sql (.trim sql)]
    (log-sql (str clear-sql "\n"))
    (with-open 
        [wrtr (writer (get-output-file-path))]
      (try 
        (with-connection db 
          (with-query-results rs [clear-sql]
            (.write wrtr (format-output rs))))
        (catch SQLException e 
          (if (is-manipulation-error (.getMessage e))
            (eval-commands clear-sql wrtr)
            (.write wrtr (str "Error: " (.getMessage e)))))))))
 
(defn eval-user-sql [sql]
  (eval-sql sql get-user-output-file-path))

(defn table-meta
  [table-name]
  (with-connection db
    (let 
        [connect (connection)
         statement (.createStatement connect)
         execResult (try 
                      (list 
                       (.executeQuery 
                        statement 
                        (str "select * from " table-name " where 0 = 1")) true)
                      (catch SQLException e 
                        (list (str "Error: " (.getMessage e)) false)))
         result-data (first execResult)
         success (last execResult)]    
      (if success
        {:success true
         :result
         (let [resultSet result-data
               rsMeta (.getMetaData resultSet)
               colCount (.getColumnCount rsMeta)]
           (loop [i 1
                  acc []]
             (if (> i colCount)
               acc
               (recur (inc i)
                      (conj acc {:name (.getColumnLabel rsMeta i)
                                 :type (.getColumnTypeName rsMeta i)})))))}
        {:success false :result result-data}))))

(defn get-table-meta
  "Discribe table."
  [table-name]
  (let [result-map (table-meta table-name)
        success (:success result-map)
        result-data (:result result-map)
        head (str "Table ``" table-name "`` description:\n")
        head-length (dec (.length head))]
    (with-open 
        [wrtr (writer (get-user-output-file-path))]
      (.write wrtr              
              (if success
                (str head
                     (simple-join head-length "-") "\n"
                     (format-output result-data))
                result-data)))))


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

