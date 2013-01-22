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

(defn eval-commands [sql wrtr] 
  (try
    (with-connection db 
      (let [res (str (with-connection db 
                       (clojure.java.jdbc/do-commands sql)))]
        (.write wrtr (str "Records affected: " res))))
    (catch SQLException e 
      (.write wrtr (str "Error: " (.getMessage e))))))

(def select-on-manipulation-errors
  (list 
   "Method only for queries" ; informix
   "Can not issue data manipulation statements with executeQuery()." ; mySQL
   ))

(defn is-manipulation-error [err-msg]
  (ejc-sql.lib/in? (map #(.equals err-msg %) select-on-manipulation-errors) true))

(defn eval-sql-core [sql rs-processing exec-or-err-processing]
  (let [clear-sql (.trim sql)]
    (try 
      (with-connection db 
        (with-query-results rs [clear-sql]
          (rs-processing rs)))
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
            (exec-or-err-processing message result))
          (exec-or-err-processing (str "Error: " (.getMessage e)) false))))))

(defn row-to-list "Feach ResultSet to plain list. 
The every element of the list is a map {:column-name value}"
  [rs]
  (loop [currRs rs
         acc (list)]
    (if (= currRs (list))
      acc
      (recur (rest currRs)
             (conj acc (first currRs))))))

(defn eval-sql [sql, output-file-path sql-log-file-path]
  (let [clear-sql (.trim sql)]
    (ejc-sql.lib/log-sql (str clear-sql "\n") sql-log-file-path)
    (with-open 
        [wrtr (writer output-file-path)]
      (try 
        (with-connection db 
          (with-query-results rs [clear-sql]
            (.write wrtr (ejc-sql.lib/format-output rs))))
        (catch SQLException e 
          (if (is-manipulation-error (.getMessage e))
            (eval-commands clear-sql wrtr)
            (.write wrtr (str "Error: " (.getMessage e)))))))))
 
(defn eval-user-sql [sql]
  (eval-sql sql (get-user-output-file-path) (get-sql-log-file-path)))

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

