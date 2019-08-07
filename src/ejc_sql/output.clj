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
  (:require [clojure.string :as s])
  (:import (java.io File)
           (java.lang.reflect Method)
           (java.util.Date)
           (java.text.SimpleDateFormat)
           (org.apache.commons.lang3 StringUtils)
           (org.hibernate.engine.jdbc.internal BasicFormatterImpl
                                               DDLFormatterImpl)))

(def ^:dynamic *add-outside-borders* true)

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
  (atom 100))

(def column-width-limit
  "Limit number of chars per column to output."
  (atom 30))

(defn set-rows-limit [val]
  "Set limit for number of records to output. When nil no limit."
  (reset! rows-limit val))

(defn set-column-width-limit [val]
  "Set limit for number of chars per column to output to output.
When nil no limit."
  (reset! column-width-limit val))

(defn fmap [f m]
  (reduce (fn [altered-map [k v]] (assoc altered-map k (f v))) {} m))

(defn rotate-table [data]
  "Rotate result set to show fields list vertically.
Applied to single-record result set.
E.g. transtofm from: a | b | c into: a | 1
                     --+---+--       b | 2
                     1 | 2 | 3       c | 3"
  (apply mapv vector data))

(defn print-maps
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows."
  ([ks rows add-borders]
   (when (seq rows)
     (let [widths (map
                   (fn [k]
                     (apply max (count (str k))
                            (map #(count (str (get % k))) rows)))
                   ks)
           spacers (map #(apply str (repeat % "-")) widths)
           fmts (map #(str "%-" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str (if add-borders leader "")
                          (apply str
                                 (interpose
                                  divider
                                  (for [[col fmt]
                                        (map vector (map #(get row %) ks) fmts)]
                                    (format fmt (str col)))))
                          (if add-borders trailer "")))
           aob *add-outside-borders*]
       (println (fmt-row (if aob "| " "") " | " (if aob " |" "")
                         (zipmap ks (map name ks))))
       (println (fmt-row (if aob "|-" "") "-+-" (if aob "-|" "")
                         (zipmap ks spacers)))
       (doseq [row rows]
         (println (fmt-row (if aob "| " "") " | " (if aob " |" "") row))))))
  ([rows add-borders]
   (print-maps (keys (first rows)) rows add-borders)))

(defn trim-max-width [x]
  (let [s (str x)]
    (if (and *max-column-width* (> (count s) *max-column-width*))
      (str (subs s 0 (- *max-column-width* 3)) "...")
      s)))

(defn print-table
  ([rows limit]
   "Converts a seq of seqs to a textual table. Uses the first seq as the table
  headings and the remaining seqs as rows."
   (when (seq rows)
     (let [row-limit (or limit @rows-limit)
           [rows msg] (if (and row-limit
                               (> row-limit 0)
                               (> (count rows) row-limit))
                        [(take row-limit rows)
                         (format "Too many rows. Only %s from %s is shown."
                                 row-limit (count rows))]
                        [rows ""])
           [headers rows] [(map name (first rows)) (rest rows)]
           aob *add-outside-borders*
           [rows rotated] (if (and (not aob)
                                   (= (count rows) 1)
                                   (> (count (first rows)) 1))
                            ;; Rotatate result set table representation
                            ;; if it has 1 row and many columns.
                            [(rotate-table [headers (first rows)]) true]
                            [rows false])
           single-column-and-row (and (= (count rows) 1)
                                      (= (count (first rows)) 1))
           [headers rows] (if (or rotated single-column-and-row)
                            ;; Do not restrict column width if result
                            ;; set has only 1 row or 1 column & 1 row.
                            [headers rows]
                            [(map trim-max-width headers)
                             (map #(map trim-max-width %) rows)])
           rows (for [row rows]
                  (map #(if (or (not (string? %))
                                single-column-and-row)
                          ;; Do not remove [\n\r] if result set is not
                          ;; a string or has only 1 column & 1 row.
                          %
                          (s/replace % #"[\n\r]" " "))
                       row))
           widths (if single-column-and-row
                    (list
                     (apply max
                            (cons
                             (count (first headers))
                             (map count (s/split (ffirst rows)
                                                 #"[\n\r]")))))
                    (for [col (rotate-table (conj rows headers))]
                      (apply max (map #(count (str %)) col))))
           spacers (map #(apply str (repeat % "-")) widths)
           ;; TODO: #(str "%" % "d") for numbers
           fmts (if (and rotated (not aob))
                  ;; Remove trailing spaces for data column of single-row
                  ;; result if no outer borders required.
                  (list (str "%-" (first widths) "s") "%s")
                  (map #(str "%-" % "s") widths))
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose
                                      divider
                                      (for [[col fmt] (map vector row fmts)]
                                        (format fmt (str col)))))
                          trailer))]
       (when (not-empty msg)
         (println msg)
         (println))
       (if (not rotated)
         (do
           (println (fmt-row (if aob "|" "") " | " (if aob "|" "") headers))
           (println (fmt-row (if aob "|" "") "-+-" (if aob "|" "") spacers))))
       (doseq [row (if (and aob single-column-and-row)
                     (map list (s/split (ffirst rows) #"[\n\r]"))
                     rows)]
         (println (fmt-row (if aob "|" "") " | " (if aob "|" "") row))))))
  ([rows] (print-table rows @rows-limit)))

(defn format-sql [sql]
  (s/trim
   (let [sql (s/trim sql)]
     (.format (if (dml? sql)
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
