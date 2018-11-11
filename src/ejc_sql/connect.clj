;;; connect.clj -- Core clojure functions for ejc-sql emacs extension.

;;; Copyright © 2013-2018 - Kostafey <kostafey@gmail.com>

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
        [clomacs])
  (:require [clojure.java.jdbc :as j]
            [clojure.contrib.java-utils]
            [clojure.string :as s]
            [ejc-sql.output :as o])
  (:import [java.sql Connection
                     DriverManager
                     PreparedStatement
                     ResultSet
            SQLException]
           [java.io File]))

(def db
  "DataBase connection properties list from the last transaction.
For debug purpose."
  (atom nil))

(def current-query
  "Current running query data."
  (atom {}))

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

(defn is-query-running? []
  (let [stmt (:stmt @current-query)]
    (and (not (nil? stmt))
         (not (.isClosed stmt)))))

(defn cancel-query []
  "Terminate current (long) running query. Aimed to cancel SELECT queries.
Unsafe for INSERT/UPDATE/CREATE/ALTER queries."
  (future
    (if (is-query-running?)
      (let [conn (:conn @current-query)
            runner (:runner @current-query)]
        (try
          (.rollback conn)
          (finally
            (.close conn)))
        (if (and
             (not (nil? runner))
             (not (future-done? runner))
             (not (future-cancelled? runner)))
          (future-cancel runner)))))
  (:start-time @current-query))

(defn validate-connection [& {:keys [db timeout]
                              :or {db @ejc-sql.connect/db
                                   timeout 5}}]
  (.isValid (j/get-connection db) timeout))

(defn get-separator-re [separator]
  "Handle cases where separator is a part of string in SQL query.
E.g. you can use default separator char `/` in this query:
SELECT * FROM urls WHERE path like '%http://localhost%'"
  (String/format "%s(?=(([^('|\")]*('|\")){2})*[^('|\")]*$)"
                 (into-array separator)))

(defn eval-sql-core
  "The core SQL evaluation function."
  [& {:keys [db sql]
      :or {db @ejc-sql.connect/db}}]
  (set-db db)
  (java.util.Locale/setDefault (java.util.Locale. "UK"))
  (let [statement-separator (or (:separator db) ";")]
    (last
     (for [sql-part (seq (.split (handle-special-cases db sql)
                                 (get-separator-re statement-separator)))]
       (try
         (let [sql-query-word (determine-dml sql-part)]
           (if (and sql-query-word (or (.equals sql-query-word "SELECT")
                                       (.equals sql-query-word "SHOW")))
             (list :result-set
                   (with-open [conn (j/get-connection db)]
                     (let [stmt (j/prepare-statement conn sql-part)]
                       (swap! current-query assoc
                              :stmt stmt
                              :conn conn)
                       (j/query db stmt
                                {:as-arrays? false
                                 :row-fn clob-to-string-row}))))
             (list :message
                   (str "Records affected: "
                        (first (j/execute! db (list sql-part)))))))
         (catch SQLException e
           (list :message
                 (str "Error: "(.getMessage e)))))))))

(clomacs-defn complete-query ejc-complete-query
              :doc "Show file contents with SQL query evaluation results.")

(defn- eval-user-sql [db sql & {:keys [rows-limit append]}]
  (let [clear-sql (.trim sql)]
    (o/log-sql (str clear-sql "\n"))
    (let [[result-type result] (eval-sql-core
                                :db  db
                                :sql clear-sql)]
      (complete-query
       (o/write-result-file (if (= result-type :result-set)
                              (o/print-table result rows-limit)
                              result)
                            :append append)
       :start-time (:start-time @current-query)
       :result (if (and
                    (not (= result-type :result-set))
                    (= (s/lower-case (subs result 0 (min 5 (count result))))
                       "error"))
                 (if (.contains (s/lower-case result)
                                "closed connection")
                   :terminated
                   :error)
                 :done)))))

(defn eval-sql-and-log-print
  "Write SQL to log file, evaluate it and print result."
  [db sql & {:keys [rows-limit append start-time sync]
             :or {append false
                  sync false}}]
  (letfn [(run-query []
            (eval-user-sql db sql
                           :rows-limit rows-limit
                           :append append))]
    (if sync
      (run-query)
      (swap! current-query assoc
             :runner (future (run-query))
             :start-time start-time))))

(defn eval-sql-internal-get-column [db sql]
  (let [[result-type result] (eval-sql-core :db db
                                            :sql sql)]
    (if (= result-type :result-set)
      (-> result rest flatten)
      result)))

(defn query-meta [db sql]
  "Get metadata for `sql` result dataset."
  (try
    {:success true
     :result (j/db-query-with-resultset
              db
              [(str "SELECT nodata.* "
                    "FROM (" sql ") nodata "
                    "WHERE 0 = 1")]
              (fn [rs]
                (let [rs-meta (.getMetaData rs)
                      col-count (.getColumnCount rs-meta)]
                  (mapv (fn [i]
                          {:name (.getColumnLabel rs-meta i)
                           :type (.getColumnTypeName rs-meta i)})
                        (range 1 (+ 1 col-count))))))}
    (catch SQLException e
      {:success false
       :result (str "Error: " (.getMessage e))})))

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
        head (str "Table \"" table-name "\" description:\n")
        head-length (dec (.length head))]
    (complete-query
     (o/write-result-file
      (if success
        (str head
             (ejc-sql.lib/simple-join head-length "-") "\n"
             (o/print-table result-data))
        result-data)))))
