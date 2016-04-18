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
            [clojure.contrib.java-utils])
  (:import [java.sql Connection
                     DriverManager
                     PreparedStatement
                     ResultSet
                     SQLException]))

(def db "DataBase connection properties list." nil)

(defn set-db [classname subprotocol subname user password database]
  (def db {:classname   classname
           :subprotocol subprotocol
           :subname     subname
           :user        user
           :password    password
           :database    database}))

(def sql-log-file-path
  "The sql queries logging filepath."
  (str (System/getProperty  "user.home")
       "/.ejc-sql/sql_log.txt"))

(defn get-sql-log-file-path []
  (ejc-sql.lib/get-absolute-file-path sql-log-file-path))

(defn print-sql-log-file-path []
  (print (get-sql-log-file-path)))

(def dml-set
  #{"SELECT"
    "INSERT"
    "UPDATE"
    "DELETE"})

(def ignore-set #{"(" "["})

(defn determine-dml [sql]
  "Determine if current SQL is Data Manipulation Language (DML) case."
  (let [sql (.trim sql)
        pos (loop [rest-sql sql
                   pos 0]
              (let [symb (subs rest-sql 0 1)]
                (if (ignore-set symb)
                  (recur (subs rest-sql 1) (inc pos))
                  pos)))]
    (or
     (dml-set (.toUpperCase (subs sql pos 6)))
     (#{"SHOW"} (.toUpperCase (subs sql pos 4))))))

(defn eval-sql-core
  "The core SQL evaluation function."
  [& {:keys [db sql]
      :or {db ejc-sql.connect/db}}]
  (last
   (for [sql-part (seq (.split sql ";"))]
     (try
       (let [sql-query-word (determine-dml sql-part)]
         (if (and sql-query-word (or (.equals sql-query-word "SELECT")
                                     (.equals sql-query-word "SHOW")))
           (list :result-set
                 (j/query db (list sql-part) {:as-arrays? true}))
           (list :message
                 (str "Records affected: "
                      (first (j/execute! db (list sql-part)))))))
       (catch SQLException e
         (list :message
               (str "Error: "(.getMessage e))))))))

(defn eval-user-sql [sql & {:keys [sql-log-file-path]
                            :or {sql-log-file-path (get-sql-log-file-path)}}]
  (let [clear-sql (.trim sql)]
    (ejc-sql.output/log-sql (str clear-sql "\n") sql-log-file-path)
    (let [[result-type result] (eval-sql-core :sql clear-sql)]
      (if (= result-type :result-set)
        (ejc-sql.output/format-array-output result)
        result))))

(defn eval-sql-and-log-print
  "Write SQL to log file, evaluate it and print result."
  [sql]
  (print (eval-user-sql sql :sql-log-file-path (get-sql-log-file-path))))

(defn eval-sql-internal-get-column [sql]
  (let [[result-type result] (eval-sql-core :sql sql)]
    (if (= result-type :result-set)
      (-> result rest flatten)
      result)))

(defn table-meta
  [table-name]
  (jd/with-connection db
    (let
        [connect (jd/connection)
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
