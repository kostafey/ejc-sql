;;; lib.clj -- Misc clojure functions for ejc-sql emacs extension.

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

(ns ejc-sql.lib
  (:import (java.io File)
           (java.net URL URLClassLoader)
           (java.lang.reflect Method)
           (java.util.Date)
           (java.text.SimpleDateFormat)))

;; (in-ns 'ejc-sql.lib)

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

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

(def is-windows
  "The value is true if it runs under the os Windows."
  (<= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(def is-linux
  "The value is true if it runs under the os Linux."
  (<= 0 (.indexOf (System/getProperty "os.name") "Linux")))

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
