;;; connect.clj -- Core clojure functions for ejc-sql emacs extension.

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

(ns ejc-sql.core)

(in-ns 'ejc-sql.core)

;; Load all external namespaces
(use 'ejc-sql.clojure-offline)
(add-to-cp (get-jar-location '[org.clojure/clojure-contrib "1.2.0"]))

(use 'clojure.java.io)
(use 'clojure.java.jdbc)
(require 'clojure.contrib.java-utils)
(use 'ejc-sql.lib)
(use 'ejc-sql.output)

(import (java.sql Connection
                  DriverManager
                  PreparedStatement
                  ResultSet
                  SQLException))

(def db "DataBase connection properties list." nil)

(def sql-log-file-path
  "The sql queries logging filepath."
  (str (System/getProperty  "user.home")
       "/.ejc-sql/sql_log.txt"))

(defn get-sql-log-file-path []
  (ejc-sql.lib/get-absolute-file-path sql-log-file-path))

(def select-on-manipulation-errors
  (list
   "Method only for queries" ; informix
   "Can not issue data manipulation statements with executeQuery()." ; mySQL
   ))

(defn is-manipulation-error [err-msg]
  (ejc-sql.lib/in? (map #(.equals err-msg %) select-on-manipulation-errors) true))

(defn eval-sql-core
  "The core SQL evaluation function.
Params list:
* sql          -- SQL script to send to JDBC.
* rs-handler   -- function with 1 arg, the returned ResultSet.
* exec-handler -- function with 1 arg, the returned by JDBC text in case of
                  SQL, which is not results in ResultSet or error text.
* err-handler  -- function with 1 arg, the returned by JDBC text in case of
                  SQL, which results in error text.
* log-handler  -- function with 1 arg, the cleaned up SQL script."
  [& {:keys [sql log-handler rs-handler exec-handler err-handler]
      :or {log-handler (fn [sql])
           rs-handler (fn [rs])
           exec-handler (fn [str])
           err-handler (fn [str])}}]
  (let [clear-sql (.trim sql)]
    (log-handler clear-sql)
    (try
      (with-connection db
        (with-query-results rs [clear-sql]
          (rs-handler rs)))
      (catch SQLException e
        (if (is-manipulation-error (.getMessage e))
          (let [[message result]
                (try
                  (list
                   (str "Records affected:"
                        (with-connection db
                          (clojure.java.jdbc/do-commands clear-sql)))
                   true)
                  (catch SQLException e
                    (list
                     (str "Error: "(.getMessage e))
                     false)))]
            (if result
              (exec-handler message)
              (err-handler message)))
          (err-handler (str "Error: " (.getMessage e))))))))

;;----------------------------------------------------------------------
;; handlers
;; TODO: unused
(defn write-to-temp-file "Write string to temp file
- the file, showing in the results buffer."
  [str output-file-path]
  (with-open
      [wrtr (writer output-file-path)]
    (.write wrtr str)))

(defn rs-to-list "Feach ResultSet to plain list.
The every element of the list is a map {:column-name value}"
  [rs]
  (loop [currRs rs
         acc (list)]
    (if (= currRs (list))
      acc
      (recur (rest currRs)
             (conj acc (first currRs))))))

(defn column-to-list "Feach first column of the ResultSet to plain list."
  [rs]
  (loop [currRs rs
         acc (list)]
    (if (= currRs (list))
      acc
      (recur (rest currRs)
             (conj acc (last (first (first currRs))))))))
;;
;;----------------------------------------------------------------------

(defn eval-sql "Evaluate users SQL scripts - common functtion."
  [sql, sql-log-file-path]
  (eval-sql-core :sql sql
                 :rs-handler (fn [rs] (ejc-sql.output/format-output rs))
                 :log-handler (fn [clear-sql] (ejc-sql.output/log-sql
                                               (str clear-sql "\n")
                                               sql-log-file-path))
                 :exec-handler identity
                 :err-handler identity))

(defn eval-user-sql "Evaluate users SQL scripts."
  [sql]
  (eval-sql sql (get-sql-log-file-path)))

(defn eval-sql-internal [sql]
  (eval-sql-core :sql sql
                 :rs-handler rs-to-list
                 :exec-handler identity
                 :err-handler identity))

(defn eval-sql-internal-get-column [sql]
  (eval-sql-core :sql sql
                 :rs-handler column-to-list
                 :exec-handler identity
                 :err-handler identity))

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
    (if success
      (str head
           (ejc-sql.lib/simple-join head-length "-") "\n"
           (ejc-sql.output/format-output result-data))
      result-data)))
