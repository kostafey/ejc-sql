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
  (:import (java.io File))
  (:require [clojure.string :as s]
            [clojure.reflect :refer [resolve-class]]))

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

(defn array? [x]
  (-> x .getClass .isArray))

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

(defn get-absolute-file-path [relative-file-path]
  (-> (java.io.File. relative-file-path) .getAbsolutePath))

(defn class-exists? [c]
  (resolve-class (.getContextClassLoader (Thread/currentThread)) c))

(defn clob-to-string [clob]
  "Turn an Oracle Clob into a String"
  (with-open [rdr (java.io.BufferedReader. (.getCharacterStream clob))]
    (apply str (line-seq rdr))))

(defn is-clob? [x]
  (or (instance? java.sql.Clob x)
      (and (class-exists? 'oracle.sql.CLOB)
           (instance? (Class/forName "oracle.sql.CLOB") x))))

(defn clob-to-string-row [row]
  "Check all data in row if it's a CLOB and convert CLOB to string."
  (loop [acc {}
         rest-keys (keys row)]
    (if rest-keys
      (let [k (first rest-keys)
            v (row k)]
        (recur (assoc acc k (if (is-clob? v)
                              (clob-to-string v)
                              v))
               (next rest-keys)))
      acc)))

(defn clean-sql [sql]
  (-> sql
      (s/replace #"(--).*\n" "")
      (s/replace #"(  )|( \t)|\t" " ")
      (s/replace "^\n" "")
      trim))

(def dml-set
  #{"SELECT"
    "INSERT"
    "UPDATE"
    "DELETE"})

(def ignore-set #{\( \[})

(defn determine-dml [sql]
  "Determine if current SQL is Data Manipulation Language (DML) case."
  (let [sql (clean-sql sql)
        pos (loop [pos 0]
              (if (ignore-set (get sql pos))
                (recur (inc pos))
                pos))]
    (or
     (dml-set (.toUpperCase (subs sql pos (+ 6 pos))))
     (#{"SHOW"} (.toUpperCase (subs sql pos (+ 4 pos)))))))
