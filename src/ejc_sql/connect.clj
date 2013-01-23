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

(use 'clojure.java.io)
(use 'clojure.java.jdbc)
(require 'clojure.contrib.java-utils)
(use 'ejc-sql.lib)

(import (java.sql Connection 
                  DriverManager 
                  PreparedStatement 
                  ResultSet
                  SQLException))

(def db "DataBase connection properties list." nil)

(def output-file-path
  "The sql queries results output filepath."
  (str (System/getProperty  "user.home")
       (if (true? ejc-sql.lib/isWindows)
         "/Application Data")
       "/.emacs.d/tmp/sql_output.txt"))

(def sql-log-file-path
  "The sql queries logging filepath."
  (str (System/getProperty  "user.home")
       (if (true? ejc-sql.lib/isWindows)
         "/Application Data")
       "/.emacs.d/tmp/sql_log.txt"))

(defn get-user-output-file-path []
  (ejc-sql.lib/get-absolute-file-path output-file-path))

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

(defn write-to-temp-file "Write string to temp file 
- the file, showing in the results buffer."
  [str output-file-path]
  (with-open 
      [wrtr (writer output-file-path)]
    (.write wrtr str)))

(defn eval-sql "Evaluate users SQL scripts - common functtion."
  [sql, output-file-path sql-log-file-path]
  (eval-sql-core :sql sql
                 :rs-handler (fn [rs] (write-to-temp-file 
                                       (ejc-sql.lib/format-output rs)
                                       output-file-path))
                 :log-handler (fn [clear-sql] (ejc-sql.lib/log-sql 
                                               (str clear-sql "\n") 
                                               sql-log-file-path))
                 :exec-handler (fn [str] (write-to-temp-file output-file-path))
                 :err-handler (fn [str] (write-to-temp-file output-file-path))))

(defn eval-user-sql "Evaluate users SQL scripts."
  [sql]
  (eval-sql sql (get-user-output-file-path) (get-sql-log-file-path)))

(defn row-to-list "Feach ResultSet to plain list. 
The every element of the list is a map {:column-name value}"
  [rs]
  (loop [currRs rs
         acc (list)]
    (if (= currRs (list))
      acc
      (recur (rest currRs)
             (conj acc (first currRs))))))

(defn eval-sql-internal [sql]
  (eval-sql-core :sql sql
                 :rs-handler row-to-list
                 :exec-handler (fn [str] str)
                 :err-handler (fn [str] str)))

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
    (with-open 
        [wrtr (writer (get-user-output-file-path))]
      (.write wrtr              
              (if success
                (str head
                     (ejc-sql.lib/simple-join head-length "-") "\n"
                     (ejc-sql.lib/format-output result-data))
                result-data)))))


;; (defn eval-sql [sql, get-output-file-path]
;;   (with-connection db 
;;     (with-query-results rs [sql] 
;;       (with-open 
;;           [wrtr (writer (get-output-file-path))]
;;         (doseq [row rs] (.write wrtr (str  row "\n")))
;;         ))))


;; (eval-user-sql "
;; SELECT TRIM(t.tabname) || '.' || TRIM(c.colname) AS table_dot_column
;;   FROM \"informix\".systables AS t, \"informix\".syscolumns AS c
;;  WHERE t.tabid = c.tabid
;;    AND t.tabtype = 'T'
;;    AND t.tabid >= 100
;;  ORDER BY t.tabname, c.colno;
;; ")


;; (def mm {:key "value" :key2 "value2"})


;; (print (clojure.string/join (for [[_ v] mm] 
;;        (str v " "))))

;; (print
;;  (format-output rs-dd))

;; (def rs-dd 
;;  '({:id "1", :name "Some text",  :dataname "qweqweqwe"}
;;    {:id "2", :name "Other text", :dataname "asdsdfsdf"}
;;    {:id "3", :name "More text",  :dataname "sdfsdfjkllk"}
;;    ))

;; (eval-user-sql " SELECT superregions.* from superregions ")

;; (with-connection db 
;;   (with-query-results rs 
;;     [" SELECT superregions.* from superregions "] 
;;     (doseq [row rs] (println  row))))

;; (eval-sql-core "SELECT * from superregions" row-to-list print)
;; (eval-sql-core "execute procedure SomeProc(103)" print print)

