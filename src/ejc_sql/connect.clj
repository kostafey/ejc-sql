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
        [ejc-sql.cache]
        [clomacs])
  (:require [clojure.java.jdbc :as j]
            [clojure.java.io :as io]
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

(defn is-statement-not-closed? []
  (let [stmt (:stmt @current-query)]
    (and (not (nil? stmt))
         (not (.isClosed stmt)))))

(defn is-query-process-running? []
  (let [runner (:runner @current-query)]
    (and
     (not (nil? runner))
     (not (future-done? runner))
     (not (future-cancelled? runner)))))

(defn is-query-running? []
  (or (is-statement-not-closed?)
      (is-query-process-running?)))

(defn cancel-query []
  "Terminate current (long) running query. Aimed to cancel SELECT queries.
Unsafe for INSERT/UPDATE/CREATE/ALTER queries."
  (future
    (if (is-statement-not-closed?)
      (let [conn (:conn @current-query)]
        (try
          (.rollback conn)
          (finally
            (.close conn)))))
    (if (is-query-process-running?)
      (future-cancel (:runner @current-query))))
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
  (re-pattern
   (format
    "%s(?<!\\s{0,1000}--.{0,1000})(?=(([^\"']*[\"']){2})*[^\"']*$)"
    ;; Escape chars for `$$` separators.
    (.replace separator "$" "\\$"))))

(def delimiter-re
  "Regex to search a `delimiter` command in SQL expression."
  (re-pattern "(?i)delimiter\\s+(.+)"))

(def comments-re
  "Regex to search comments in SQL expression."
  (java.util.regex.Pattern/compile
   "(?:/\\*.*?\\*/)|(?:--.*?$)",
   (bit-or java.util.regex.Pattern/DOTALL
           java.util.regex.Pattern/MULTILINE)))

(clomacs-defn complete-query ejc-complete-query
              :doc "Show file contents with SQL query evaluation results.")

(defn complete [text & {:keys [display-result
                               result-file
                               append
                               mode
                               start-time
                               status
                               connection-name
                               db
                               goto-symbol]
                        :or {display-result true
                             append false
                             mode 'ejc-result-mode}}]
  "Complete query and display `text` as a result."
  (complete-query
   (if text
     (o/write-result-file text :result-file result-file :append append))
   :start-time start-time
   :status status
   :display-result display-result
   :mode mode
   :connection-name connection-name
   :db db
   :goto-symbol goto-symbol))

(defn- eval-user-sql [db sql & {:keys [rows-limit
                                       append
                                       display-result
                                       result-file]}]
  (set-db db)
  (java.util.Locale/setDefault (java.util.Locale. "UK"))
  (let [sql (.trim sql)
        _ (do (o/log-sql (str sql "\n"))
              (o/clear-result-file :result-file result-file))
        [sql
         manual-separator] (if (s/starts-with? (s/lower-case sql) "delimiter")
                             ;; User defined statement separator manually
                             ;; before SQL expression.
                             ;; Remove `delimiter` command from SQL expression.
                             [(s/trim (s/replace-first sql delimiter-re ""))
                              (second (re-find delimiter-re sql))]
                             [sql nil])
        statement-separator (or manual-separator (:separator db) ";")
        results (doall
                 (for [sql-part (filter
                                 ;; Remove parts contains comments only.
                                 (fn [part]
                                   (not (empty?
                                         (s/trim
                                          (s/replace part comments-re "")))))
                                 (s/split
                                  (handle-special-cases db sql)
                                  (get-separator-re statement-separator)))]
                   (->
                    (try
                      (if (select? sql-part)
                        (list
                         :result-set
                         (with-open [conn (j/get-connection db)]
                           (let [stmt (j/prepare-statement
                                       conn sql-part
                                       {:fetch-size (or @o/fetch-size 0)
                                        :max-rows (or @o/max-rows 0)})]
                             (swap! current-query assoc
                                    :stmt stmt
                                    :conn conn)
                             (j/query db stmt
                                      {:as-arrays? true
                                       :result-set-fn
                                       (fn [rs]
                                         (let [single-record?
                                               (not (next (next rs)))]
                                           (mapv
                                            #(clob-to-string-row
                                              % single-record?)
                                            rs)))}))))
                        (let [result (first (j/execute! db (list sql-part)))
                              msg (if (> result 0)
                                    (str "Records affected: " result)
                                    "Executed")]
                          (if (ddl? sql-part)
                            (invalidate-cache db))
                          (list :message msg)))
                      (catch SQLException e
                        (list :message
                              (o/unify-str "Error: " (.getMessage e)))))
                    ((fn [[result-type result]]
                       (if (= result-type :result-set)
                         (o/print-table result rows-limit)
                         (println result))
                       [result-type result])))))]
    (complete
     nil
     :start-time (:start-time @current-query)
     :status (if (or
                  (every? (fn [[result-type result]]
                            (= result-type :result-set))
                          results)
                  (not (some (fn [[result-type result]]
                               (and
                                (not (= result-type :result-set))
                                (s/starts-with? (s/lower-case result)
                                                "error")))
                             results)))
               :done
               (if (and
                    (= (count results) 1)
                    (some (fn [[result-type result]]
                            (.contains (s/lower-case result)
                                       "closed connection"))
                          results))
                 :terminated
                 :error))
     :display-result display-result
     :result-file result-file)))

(defn eval-sql-and-log-print
  "Write SQL to log file, evaluate it and print result."
  [db sql & {:keys [rows-limit
                    append
                    start-time
                    sync
                    display-result
                    result-file
                    add-outside-borders]
             :or {append false
                  sync false
                  display-result true
                  add-outside-borders true}}]
  (letfn [(run-query []
            (try
              (with-open [out (io/writer result-file)]
                (binding [*out* out
                          o/*add-outside-borders* add-outside-borders
                          *max-column-width* @o/column-width-limit]
                  (eval-user-sql db sql
                                 :rows-limit rows-limit
                                 :append append
                                 :display-result display-result
                                 :result-file result-file)))
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
