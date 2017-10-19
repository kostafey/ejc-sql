;;; connect.clj -- Core clojure functions for ejc-sql emacs extension.

;;; Copyright © 2013-2016 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.connect
  (:use [clojure.java.io]
        [ejc-sql.lib]
        [ejc-sql.output])
  (:require [clojure.java.jdbc :as j]
            [clojure.java.jdbc.deprecated :as jd]
            [clojure.contrib.java-utils]
            [clojure.string :as s])
  (:import [java.sql Connection
                     DriverManager
                     PreparedStatement
                     ResultSet
                     SQLException]))

(def db
  "DataBase connection properties list from the last transaction.
For debug purpose."
  (atom nil))

;; TODO: running query termination handler
(def current-query
  "Current running query."
  (atom nil))

(defn set-db [ejc-db]
  (reset! db ejc-db))

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

(defn handle-special-cases [db sql]
  (case (:subprotocol db)
    "oracle" (case (clojure.string/upper-case sql)
               "SHOW ERRORS" (str "SELECT line, position, sequence, text
                                  FROM all_errors
                                  WHERE type = 'FUNCTION'
                                    AND name = 'FILTERDICT'
                                    AND attribute != 'WARNING'
                                    AND owner = '" (:user db)  "'
                                  ORDER BY line, position, sequence")
               sql)
    sql))

(defn eval-sql-core
  "The core SQL evaluation function."
  [& {:keys [db sql]
      :or {db @ejc-sql.connect/db}}]
  (set-db db)
  (let [statement-separator (or (:separator db) ";")]
    (last
     (for [sql-part (seq (.split (handle-special-cases db sql)
                                 statement-separator))]
       (try
         (let [sql-query-word (determine-dml sql-part)]
           (if (and sql-query-word (or (.equals sql-query-word "SELECT")
                                       (.equals sql-query-word "SHOW")))
             (list :result-set
                   (j/query db (list sql-part) {:as-arrays? false}))
             (list :message
                   (str "Records affected: "
                        (first (j/execute! db (list sql-part)))))))
         (catch SQLException e
           (list :message
                 (str "Error: "(.getMessage e)))))))))

(defn eval-user-sql [db sql]
  (let [clear-sql (.trim sql)]
    (ejc-sql.output/log-sql (str clear-sql "\n"))
    (let [[result-type result] (eval-sql-core
                                :db  db
                                :sql clear-sql)]
      (if (= result-type :result-set)
        (ejc-sql.output/print-table result)
        result))))

(defn eval-sql-and-log-print
  "Write SQL to log file, evaluate it and print result."
  [db sql]
  (print (eval-user-sql db sql)))

(defn eval-sql-internal-get-column [db sql]
  (let [[result-type result] (eval-sql-core :db db
                                            :sql sql)]
    (if (= result-type :result-set)
      (-> result rest flatten)
      result)))

(defn query-meta [db sql]
  "Get metadata for `sql` result dataset."
  (jd/with-connection db
    (let [connect (jd/connection)
          statement (.createStatement connect)]
      (try
        {:success true
         :result (let [resultSet (.executeQuery
                                  statement
                                  (str
                                   "SELECT nodata.* "
                                   "FROM (" sql ") nodata "
                                   "WHERE 0 = 1"))
                       rsMeta (.getMetaData resultSet)
                       colCount (.getColumnCount rsMeta)]
                   (mapv (fn [i]
                           {:name (.getColumnLabel rsMeta i)
                            :type (.getColumnTypeName rsMeta i)})
                         (range 1 (+ 1 colCount))))}
        (catch SQLException e
          {:success false
           :result (str "Error: " (.getMessage e))})))))

(defn table-meta
  [db table-name]
  (set-db db)
  (query-meta db (str "SELECT * FROM " table-name)))

(defn get-table-meta
  "Discribe table."
  [db table-name]
  (let [result-map (table-meta db table-name)
        success (:success result-map)
        result-data (:result result-map)
        head (str "Table ``" table-name "`` description:\n")
        head-length (dec (.length head))]
    (if success
      (str head
           (ejc-sql.lib/simple-join head-length "-") "\n"
           (ejc-sql.output/print-table result-data))
      result-data)))
