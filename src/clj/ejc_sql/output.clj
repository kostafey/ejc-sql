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

(defn log-sql [sql sql-log-file-path]
  (let [is-new-file (if (not (. (clojure.contrib.java-utils/file
                                 sql-log-file-path) exists))
                      true false)]
    (if is-new-file
      (let [file (File. sql-log-file-path)]
        (.mkdirs (File. (.getParent file)))
        (.createNewFile file)))
    (with-open [wrtr (clojure.java.io/writer sql-log-file-path :append true)]
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

(defn format-output [rs & {:keys [as-arrays?]
                           :or {as-arrays? false}}]
  (let [records-data (if as-arrays?
                       (rest rs)
                       (filter-data (get-rs-data rs)))
        headers-data (if as-arrays?
                       (map get-column-name (first rs))
                       (get-rs-headers rs))
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

