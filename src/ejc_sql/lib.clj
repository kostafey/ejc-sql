;;; lib.clj -- Misc clojure functions for ejc-sql emacs extension.

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

(ns ejc-sql.lib
  (:require [clojure.string :as s]
            [clojure.reflect :refer [resolve-class]]))

(def ^:dynamic *max-column-width* 30)

(def windows?
  "The value is true if it runs under the os Windows."
  (>= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(defn in?
  "true if `seq` contains `elm`.
  Search case insensitive wthen `case-sensitive` is false."
  [seq elm & {:keys [case-sensitive] :or {case-sensitive true}}]
  (if (and elm seq)
    (if (not case-sensitive)
      (in? (mapv s/lower-case seq) (s/lower-case elm))
      (some #(= elm %) seq))))

(defn get->in [obj path & {:keys [case-sensitive] :or {case-sensitive false}}]
  "Case insensitive `get-in` for last key in sequence."
  (if case-sensitive
    (get-in obj path)
    (get-in obj
            (conj (into [] (butlast path))
                  (last path))
            (get-in obj
                    (conj (into [] (butlast path))
                          (s/upper-case (last path)))
                    (get-in obj
                            (conj (into [] (butlast path))
                                  (s/lower-case (last path))))))))

(defn simple-join [n s]
  (s/join
   (for [x (range 0 n)] s)))

(defn class-exists? [c]
  (resolve-class (.getContextClassLoader (Thread/currentThread)) c))

(defn is-clob? [x]
  (or (instance? java.sql.Clob x)
      (and (class-exists? 'oracle.sql.CLOB)
           (instance? (Class/forName "oracle.sql.CLOB") x))))

(defn clob-to-string
  "Turn an Oracle Clob into a String"
  ([clob]
   (clob-to-string clob true))
  ([clob single-record?]
   (with-open [rdr (java.io.BufferedReader. (.getCharacterStream clob))]
     (if (or single-record? (not *max-column-width*))
       (apply str (line-seq rdr))
       ;; Show only first `*max-column-width*` (30 by default) symbols
       ;; of Clob field. If longer, replace last 3 symbols by dots.
       (let [result (apply str (take (+ *max-column-width* 1)
                                     (mapcat seq (line-seq rdr))))]
         (if (> (count result) *max-column-width*)
           (str (subs result 0 (- *max-column-width* 3)) "...")
           result))))))

(defn clob-to-string-row [row single-record?]
  "Check all data in row if it's a CLOB and convert CLOB to string."
  (mapv (fn [field]
          (if (is-clob? field)
            (clob-to-string field single-record?)
            field))
        row))

(defn clean-sql [sql]
  (-> sql
      (s/replace #"(--).*\n" "")
      (s/replace #"(  )|( \t)|\t" " ")
      (s/replace "^\n" "")
      s/trim))

(def select-words
  '("SELECT"
    "SHOW"
    "WITH"))

(def dml-words
  (concat
   select-words
   '("INSERT"
     "UPDATE"
     "DELETE")))

(def ddl-words
  '("CREATE"
    "ALTER"
    "DROP"
    "RENAME"))

(defn determine-query [sql words]
  "Determine if current SQL starts with one of the words from `words` list."
  (let [sql (clean-sql sql)
        ignore-set #{\( \[}
        pos (loop [pos 0]
              (if (ignore-set (get sql pos))
                (recur (inc pos))
                pos))
        sql (s/upper-case (subs sql pos))]
    (first (filter (fn [w] (s/starts-with? sql w)) words))))

(defn select? [sql]
  (determine-query sql select-words))

(defn dml? [sql]
  "Determine if current SQL is Data Manipulation Language (DML) case."
  (determine-query sql dml-words))

(defn ddl? [sql]
  "Determine if current SQL is Data Definition Language (DDL) case."
  (determine-query sql ddl-words))
