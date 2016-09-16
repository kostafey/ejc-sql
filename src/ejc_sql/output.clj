;;; output.clj -- Output & formatting clojure functions for ejc-sql.

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.output
  (:use clojure.java.io
        ejc-sql.lib)
  (:require clojure.contrib.java-utils)
  (:import (java.io File)
           (java.lang.reflect Method)
           (java.util.Date)
           (java.text.SimpleDateFormat)))

(defn get-log-dir []
  (file (if windows?
          (System/getenv "AppData")
          "/var/log/")
        "ejc-sql"))

(defn get-log-file []
  (file (get-log-dir)
        (str (.format (new java.text.SimpleDateFormat
                           "yyyy-MM-dd")
                      (new java.util.Date)) ".log")))

(defn print-log-file-path []
  (print (.getAbsolutePath (get-log-file))))

(defn log-sql [sql]
  (let [log-file (get-log-file)
        is-new-file (not (.exists log-file))]
    (when is-new-file
      (.mkdirs (File. (.getParent log-file)))
      (.createNewFile log-file))
    (with-open [wrtr (clojure.java.io/writer log-file :append true)]
      (if is-new-file
        (.write wrtr (str "-- -*- mode: sql; -*-\n"
                          "-- Local Variables:\n"
                          "-- eval: (ejc-sql-mode)\n"
                          "-- End:\n")))
      (.write wrtr (str (simple-join 50 "-") " "
                        (.format (new java.text.SimpleDateFormat
                                      "yyyy.MM.dd HH:mm:ss.S")
                                 (new java.util.Date))
                        " " (simple-join 2 "-") "\n" sql "\n")))))

(defn get-column-name [column-symbol]
  (let [column-string (str column-symbol)]
    (.substring (str column-symbol) 1 (count column-string))))

(defn format-output [rs & {:keys [as-arrays? add-headers?]
                           :or {as-arrays? false
                                add-headers? true}}]
  (let [records-data (if as-arrays?
                       (if add-headers? (rest rs) rs)
                       (filter-data (get-rs-data rs)))
        headers-data (if add-headers? (if as-arrays?
                                        (map get-column-name (first rs))
                                        (get-rs-headers rs))
                         (range (count rs)))
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

(defn format-array-output [result]
  (format-output result :as-arrays? true))

