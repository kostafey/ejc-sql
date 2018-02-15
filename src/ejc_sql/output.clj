;;; output.clj -- Output & formatting clojure functions for ejc-sql.

;;; Copyright © 2013, 2016 - Kostafey <kostafey@gmail.com>

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
           (java.text.SimpleDateFormat)
           (org.apache.openjpa.lib.jdbc SQLFormatter)
           (org.hibernate.engine.jdbc.internal BasicFormatterImpl)))

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

(def rows-limit
  "Limit number of records to output."
  (atom 1000))

(defn set-rows-limit [val]
  "Set limit for number of records to output. When nil no limit."
  (reset! rows-limit val))

(defn print-table
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows."
  ([ks rows limit]
   (when (seq rows)
     (let [row-limit (or limit @rows-limit)
           [rows msg] (if (and row-limit
                               (> row-limit 0)
                               (> (count rows) row-limit))
                        [(take row-limit rows)
                         (format "Too many rows. Only %s from %s is shown.\n\n"
                                 row-limit (count rows))]
                        [rows ""])
           widths (map
                   (fn [k]
                     (apply max (count (name k)) (map #(count (str (get % k))) rows)))
                   ks)
           headers (map name ks)
           spacers (map #(apply str (repeat % "-")) widths)
           ;; TODO: #(str "%" % "d") for numbers
           fmts (map #(str "%-" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (format fmt (str col)))))
                          trailer))
           result (new StringBuffer "")
           ;; TODO: cahnge to #(println %) when async output ready
           out #(.append result (str % "\n"))]
       (out (fmt-row "" " | " "" (zipmap ks headers)))
       (out (fmt-row "" "-+-" "" (zipmap ks spacers)))
       (doseq [row rows]
         (out (fmt-row "" " | " "" row)))
       (str msg (.toString result)))))
  ([rows limit] (print-table (keys (first rows)) rows limit)))

(defn pretty-print [sql formatter]
  (if (= formatter :jpa)
    (print (.prettyPrint (SQLFormatter.) sql))
    ;; :hibernate
    (print (.format (BasicFormatterImpl.) sql))))
