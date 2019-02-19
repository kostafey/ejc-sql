;;; connect.clj -- Core clojure functions for ejc-sql emacs extension.

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
  (letfn [(abstract-is-valid [msg]
            (clomacs/format-result
             {:status true
              :message (str "Warning: can't validate connection. "
                            "Please, update your JDBC driver and "
                            "restart the REPL.\n"
                            msg)}))]
    (try
      (clomacs/format-result
       {:status (.isValid (j/get-connection db) timeout)
        :message "Connected."})
      (catch AbstractMethodError e
        (abstract-is-valid (.getMessage e)))
      (catch java.sql.SQLFeatureNotSupportedException e
        (abstract-is-valid (.getMessage e))))))

(defn get-separator-re [separator]
  "Handle cases where separator is a part of string in SQL query.
E.g. you can use default separator char `/` in this query:
SELECT * FROM urls WHERE path like '%http://localhost%'"
  (String/format
   "%s(?<!\\s{0,1000}--.{0,1000})(?=(([^\"']*[\"']){2})*[^\"']*$)"
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

(clomacs-defn spinner-stop ejc-spinner-stop
              :doc "Stop spinner indicating current running query.")

(defn complete [text & {:keys [display-result
                               append
                               mode
                               start-time
                               status
                               connection-name
                               db]
                        :or {display-result true
                             append false
                             mode 'ejc-result-mode}}]
  "Complete query and display `text` as a result."
  (let [complete-output (complete-query
                         (o/write-result-file text :append append)
                         :start-time start-time
                         :status status
                         :display-result display-result
                         :mode mode
                         :connection-name connection-name
                         :db db)]
    (spinner-stop)
    complete-output))

(defn- eval-user-sql [db sql & {:keys [rows-limit append display-result]}]
  (let [clear-sql (.trim sql)]
    (o/log-sql (str clear-sql "\n"))
    (let [[result-type result] (eval-sql-core
                                :db  db
                                :sql clear-sql)]
      (complete
       (if (= result-type :result-set)
         (o/print-table result rows-limit)
         result)
       :append append
       :start-time (:start-time @current-query)
       :status (if (and
                    (not (= result-type :result-set))
                    (= (s/lower-case (subs result 0
                                           (min 5 (count result))))
                       "error"))
                 (if (.contains (s/lower-case result)
                                "closed connection")
                   :terminated
                   :error)
                 :done)
       :display-result display-result))))

(defn eval-sql-and-log-print
  "Write SQL to log file, evaluate it and print result."
  [db sql & {:keys [rows-limit append start-time sync display-result]
             :or {append false
                  sync false
                  display-result true}}]
  (letfn [(run-query []
            (try
              (eval-user-sql db sql
                             :rows-limit rows-limit
                             :append append
                             :display-result display-result)
              (catch Exception e
                (complete
                 (str (.getMessage e) "\n"
                      (s/join "\n" (.getStackTrace e)))
                 :start-time (:start-time @current-query)
                 :status :error
                 :display-result true))))]
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
  "Describe table."
  [db connection-name table-name]
  (let [result-map (table-meta db table-name)
        success (:success result-map)
        result-data (:result result-map)
        head (str "Table \"" table-name "\" description:\n")
        head-length (dec (.length head))]
    (complete
     (if success
       (str head
            (ejc-sql.lib/simple-join head-length "-") "\n"
            (o/print-table result-data))
       result-data)
     :display-result true
     :connection-name connection-name
     :db db)))
