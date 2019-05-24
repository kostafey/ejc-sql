;;; output.clj -- Output & formatting clojure functions for ejc-sql.

;;; Copyright © 2013-2019 - Kostafey <kostafey@gmail.com>

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
  (:require clojure.contrib.java-utils
            [clojure.string :as s])
  (:import (java.io File)
           (java.lang.reflect Method)
           (java.util.Date)
           (java.text.SimpleDateFormat)
           (org.apache.commons.lang3 StringUtils)
           (org.hibernate.engine.jdbc.internal BasicFormatterImpl
                                               DDLFormatterImpl)))

(defn get-log-dir []
  (file (if windows?
          (System/getenv "AppData")
          "/var/log/")
        "ejc-sql"))

(defn get-log-file [& [create-new]]
  (loop [xs (take 100 (range))]
    (if xs
      (let [prev-day (first xs)
            log-file (file
                      (get-log-dir)
                      (format "%s.log"
                              (.format
                               (new java.text.SimpleDateFormat
                                    "yyyy-MM-dd")
                               (let [cal (java.util.Calendar/getInstance)]
                                 (.setTime cal (new java.util.Date))
                                 (.add cal java.util.Calendar/DATE
                                       (if prev-day (- prev-day) 0))
                                 (.getTime cal)))))]
        (if (or create-new (.exists log-file))
          log-file
          (recur (next xs)))))))

(defn print-log-file-path []
  (print (.getAbsolutePath (or (get-log-file) (get-log-dir)))))

(defn log-sql [sql]
  (let [log-file (get-log-file true)
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

(defn fmap [f m]
  (reduce (fn [altered-map [k v]] (assoc altered-map k (f v))) {} m))

(defn rotate-table [data]
  "Rotate result set to show fields list vertically.
Applied to single-record result set.
E.g. transtofm from: a | b | c into: a | 1
                     --+---+--       b | 2
                     1 | 2 | 3       c | 3"
  (apply mapv vector data))

(defn print-table
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows."
  ([rows limit]
   (when (seq rows)
     (let [row-limit (or limit @rows-limit)
           [rows msg] (if (and row-limit
                               (> row-limit 0)
                               (> (count rows) row-limit))
                        [(take row-limit rows)
                         (format "Too many rows. Only %s from %s is shown.\n\n"
                                 row-limit (count rows))]
                        [rows ""])
           [rows rotated] (if (and (= (count rows) 2)
                                      (> (count (first rows)) 1))
                               [(rotate-table rows) true]
                               [rows false])
           [headers rows] [(map name (first rows)) (rest rows)]
           rows (map #(if (string? %) (clojure.string/replace % "\n" " ") %) rows)
           widths (for [col (rotate-table (conj rows headers))]
                    (apply max (map #(count (str %)) col)))
           spacers (map #(apply str (repeat % "-")) widths)
           ;; TODO: #(str "%" % "d") for numbers
           fmts (map #(str "%-" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector row fmts)]
                                                  (format fmt (str col)))))
                          trailer))
           result (new StringBuffer "")
           ;; TODO: cahnge to #(println %) when async output ready
           out #(.append result (str % "\n"))]
       (out (fmt-row "" " | " "" headers))
       (if (not rotated)
         (out (fmt-row "" "-+-" "" spacers)))
       (doseq [row rows]
         (out (fmt-row "" " | " "" row)))
       (str msg
            (if rotated
              (String/join "\n"
                           (mapv #(.trim %) (.split (.toString result) "\n")))
              (.toString result))))))
  ([rows] (print-table rows @rows-limit)))

(defn format-sql [sql]
  (s/trim
   (let [sql (s/trim sql)]
     (.format (if (determine-dml sql)
                (BasicFormatterImpl.)
                (DDLFormatterImpl.))
              sql))))

(defn format-sql-print [sql]
  "SQL should be printed to provide cross-platform newline handling."
  (print (format-sql sql)))

(defn format-sql-if-required [sql]
  (if (> (StringUtils/countMatches sql "\n") 1)
    sql
    (format-sql sql)))

(defn write-result-file [text & {:keys [result-file
                                        append]
                                 :or {append false}}]
  (spit result-file text :append append)
  result-file)

(defn clear-result-file []
  (write-result-file ""))
