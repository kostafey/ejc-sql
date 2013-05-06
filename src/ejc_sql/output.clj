(ns ejc-sql.output
  (:use clojure.java.io
        ejc-sql.lib)
  (:import (java.io File)
           (java.lang.reflect Method)
           (java.util.Date)
           (java.text.SimpleDateFormat)))

(defn log-sql [sql sql-log-file-path]
  (let [is-new-file (if (not (. (clojure.contrib.java-utils/file
                                 sql-log-file-path) exists))
                      true false)]
    (with-open
        [wrtr (clojure.java.io/writer (get-absolute-file-path sql-log-file-path) :append true)]
      (if is-new-file
        (.write wrtr "-*- mode: sql; -*-*/\n"))
      (.write wrtr (str (simple-join 50 "-") " "
                        (.format (new java.text.SimpleDateFormat "yyyy.MM.dd HH:mm:ss.S")
                                 (new java.util.Date))
                        " " (simple-join 2 "-") "\n" sql "\n")))))

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
