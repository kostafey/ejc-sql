;;; structure.clj -- Receive database stucture and keep it in cache.

;;; Copyright © 2016-2017 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.structure
  (:use [ejc-sql.lib])
  (:require
   [clojure.java.jdbc :as j]
   [ejc-sql.connect :as c]))

(def cache (atom {}))

(defn- db->column [db sql]
  "Execute `sql`, return first column of result set as result list."
  (if sql
    (rest
     (map first
          (j/query db (list sql) {:as-arrays? true})))))

(defn- db->value [db sql]
  "Execute `sql`, return first value of first column of result set as result."
  (first (db->column db sql)))

(defn get-ms-sql-server-version [db]
  (->
   (filter not-empty
           (.split
            (db->value db "SELECT @@VERSION AS 'SQL Server Version'")
            " "))
   (nth 3)
   Integer/parseInt))

(def default-queries
  {:owners  (fn [& _]
              (str " SELECT schema_owner              \n"
                   " FROM information_schema.schemata \n"))
   :tables  (fn [& {:keys [schema]}]
              (str " SELECT table_name                \n"
                   " FROM information_schema.tables   \n"
                   (if schema
                     (str " WHERE table_schema = '" schema "'")
                     "")
                   " ORDER BY table_name              \n"))
   :columns (fn [& {:keys [table]}]
              (str " SELECT column_name               \n"
                   " FROM information_schema.columns  \n"
                   " WHERE table_name = '" table "'   \n"
                   " ORDER BY column_name             \n"))})

(def queries
  {
   ;;--------
   :oracle
   ;;--------
   {:entity      (fn [& {:keys [entity-name]}]
                   (str " SELECT text             \n"
                        " FROM all_source         \n"
                        " WHERE name = '"entity-name"'"))
    :types       (fn [& _] "SELECT * FROM USER_TYPES")
    :owners      (fn [& _] (str " SELECT DISTINCT(owner) \n"
                                " FROM ALL_OBJECTS       \n"
                                " ORDER BY owner         \n"))
    :tables      (fn [& {:keys [owner]}]
                   (str " SELECT table_name, owner \n"
                        " FROM all_tables          \n"
                        (if owner
                          (str " WHERE owner = '"owner"'"))
                        " ORDER BY table_name"))
    :columns     (fn [& {:keys [table]}]
                   (str " SELECT column_name      \n"
                        " FROM ALL_TAB_COLUMNS    \n"
                        " WHERE table_name = '"table"'"))
    :constraints (fn [& {:keys [owner table]}]
                   (if table
                     (str " SELECT * FROM all_constraints    \n"
                          " WHERE owner = "owner"            \n"
                          "       AND table_name = '"table"' \n")
                     "SELECT * FROM user_constraints"))
    :procedures  (fn [& {:keys [owner]}]
                   (str " SELECT object_name, procedure_name \n"
                        " FROM all_procedures                \n"
                        (if owner
                          (str " WHERE owner = '" owner "'")
                          "")))
    :objects     (fn [& _]
                   (str "SELECT * FROM all_objects WHERE object_type IN "
                        "('FUNCTION','PROCEDURE','PACKAGE')"))}
   ;;--------
   :informix
   ;;--------
   {:owners nil
    :tables  (fn [& _]
               (str " SELECT TRIM(t.tabname) as tablesList \n"
                    " FROM systables AS t                  \n"
                    " WHERE t.tabtype = 'T'                \n"
                    "   AND t.tabid >= 100                 \n"
                    " ORDER BY t.tabname;                  \n"))
    :columns (fn [& {:keys [table]}]
               (str " SELECT TRIM(c.colname) AS column_name \n"
                    "  FROM systables AS t, syscolumns AS c \n"
                    " WHERE t.tabid = c.tabid               \n"
                    "   AND t.tabtype = 'T'                 \n"
                    "   AND t.tabid >= 100                  \n"
                    "   AND TRIM(t.tabname) = '" table "'   \n"
                    " ORDER BY c.colno;                     \n"))}
   ;;-------
   :mysql
   ;;-------
   {:owners  (default-queries :owners)
    :tables  (default-queries :tables)
    :columns (default-queries :columns)}
   ;;--------
   :h2
   ;;--------
   {:tables  (fn [& _] ((default-queries :tables) :schema "PUBLIC"))
    :columns (default-queries :columns)}
   ;;-------
   :sqlserver ; ms sql server
   ;;-------
   {:owners  (default-queries :owners)
    :tables  (default-queries :tables)
    :columns (default-queries :columns)
    :constraints (fn [& {:keys [table]}]
                   (str "SELECT type_desc AS constraint_type, \n"
                        "       name                          \n"
                        "FROM sys.objects                     \n"
                        "WHERE type_desc LIKE '%CONSTRAINT'   \n"
                        "  AND OBJECT_NAME(parent_object_id)='" table "'"))
    :entity      (fn [& {:keys [db entity-name]}]
                   (if (> (get-ms-sql-server-version db) 2000)
                     (str
                      "SELECT definition                                   \n"
                      "FROM sys.objects     o                              \n"
                      "JOIN sys.sql_modules m ON m.object_id = o.object_id \n"
                      "WHERE o.object_id = object_id('" entity-name "')    \n")
                     (str
                      "SELECT c.text                     \n"
                      "FROM sysobjects  o                \n"
                      "JOIN syscomments c ON c.id = o.id \n"
                      "WHERE o.name = '" entity-name "'  \n")))}
   ;;-------
   :postgresql
   ;;-------
   {:owners  (default-queries :owners)
    :tables  (default-queries :tables)
    :columns (default-queries :columns)}})

(defn autocomplete-available-for-db? [db-type]
  (queries db-type))

(defn get-db-name [db]
  (let [{:keys [database subname connection-uri]} db]
    (or database
        (if subname
          (let [separator (if (= (first (.split subname "/")) subname)
                            ":" "/")
                raw-db-name (last (.split subname separator))
                raw-db-name (first (.split raw-db-name "\\?"))
                raw-db-name (first (.split raw-db-name ";"))]
            raw-db-name)
          ;; No subname - parse connection-uri
          (let [props-list (.split connection-uri ";")
                db-name-prop (first
                              (filter
                               (fn [prop]
                                 (=
                                  "databasename"
                                  (.toLowerCase (first (.split prop "=")))))
                               props-list))]
            (if db-name-prop
              ;; "databaseName=my_db_name;"
              (second (.split db-name-prop "="))
              ;; "jdbc:jtds:sqlserver://localhost:1433/my_db_name;"
              (last (.split (first props-list) "/"))))))))

(defn get-user [db]
  (or
   (:user db)
   (let [{:keys [user connection-uri]} db]
     (or user
         (second (.split
                  (let [props-list (.split connection-uri ";")]
                    (first
                     (filter
                      (fn [prop]
                        (=
                         "user"
                         (.toLowerCase (first (.split prop "=")))))
                      props-list))) "="))))))

(defn get-db-type [db]
  (let [{:keys [subprotocol connection-uri]} db]
    (keyword
     (or subprotocol
         (let [attrs (.split connection-uri ":")]
           (if (= (second attrs) "jtds")
             ;; jdbc:jtds:sqlserver://...
             (nth attrs 2)
             ;; jdbc:sqlserver://localhost\instance:1433;
             (second attrs)))))))

(defn get-this-owner [db & [owner]]
  "Return current owner/schema."
  (or owner
      (get? (get-in @cache [db :this-owner]) false)
      (do
        (swap! cache assoc-in [db :this-owner]
               (future
                 ((fn [db]
                    (let [db-type (get-db-type db)]
                      (case db-type
                        :sqlserver (if (> (get-ms-sql-server-version db) 2000)
                                     ;; Get default SQL Server schema for session
                                     (db->value db "SELECT SCHEMA_NAME()")
                                     "dbo")
                        :oracle (db->value
                                 db
                                 "SELECT sys_context('userenv', 'current_schema') FROM dual")
                        ;; By default
                        ;; Assume owner == schema (it can be wrong in general).
                        (get-user db)))) db)))
        (get? (get-in @cache [db :this-owner]) true))))

(defn select-db-meta-script [db meta-type &
                             {:keys [owner
                                     table
                                     entity-name]}]
  "Return SQL request to obtain some database structure info."
  (let [db-type (get-db-type db)
        meta-type (if (keyword? meta-type) meta-type (keyword meta-type))
        owner (get-this-owner db owner)
        sql-receiver (get-in queries [db-type meta-type])
        sql (if sql-receiver
              (sql-receiver :db db
                            :owner owner
                            :schema owner
                            :table table
                            :entity-name entity-name
                            :db-name (get-db-name db)))]
    sql))

(defn- get? [obj & [force?]]
  (if (and obj (or force? (realized? obj)))
    @obj))

(defn get-owners [db & [force?]]
  "Return owners list from cache if already received from DB,
check if receiveing process is not running, then start it."
  (let [db-type (get-db-type db)
        need-owners? (get-in queries [db-type :owners])]
    (if need-owners?
      (do
        (if (not (get-in @cache [db :owners]))
          (swap! cache assoc-in [db :owners]
                 (future ((fn [db]
                            (let [sql (select-db-meta-script db :owners)]
                              (db->column db sql))) db))))
        (get? (get-in @cache [db :owners]) force?))
      (list))))

(defn get-tables [db & [owner_ force?]]
  "Return tables list for this owner from cache if already received from DB,
check if receiveing process is not running, then start it."
  (let [owner (get-this-owner db owner_)]
    (if (not (get-in @cache [db :tables (keyword owner)]))
      (swap! cache assoc-in [db :tables (keyword owner)]
             (future ((fn [db owner]
                        (let [sql (select-db-meta-script db :tables
                                                         :owner owner)]
                          (db->column db sql)))
                      db owner))))
    (get? (get-in @cache [db :tables (keyword owner)]) force?)))

(defn get-colomns [db table force?]
  "Return colomns list for this table from cache if already received from DB,
check if receiveing process is not running, then start it."
  (if (not (get-in @cache [db :colomns (keyword table)]))
    (swap! cache assoc-in [db :colomns (keyword table)]
           (future ((fn [db table]
                      (let [sql (select-db-meta-script db :columns
                                                       :table table)]
                        (db->column db sql)))
                    db table))))
  (get? (get-in @cache [db :colomns (keyword table)]) force?))

(defn is-owner? [db prefix]
  (in? (get-owners db) prefix))

(defn get-owners-candidates [db & _]
  "Return owners candidates autocomplete list from the database structure
cache, async request to fill it, if not yet.
The result list has the following structure:
(pending item1 item2 ...)
If `pending` is t - the async request to get the structure is running
if `pending` is nil - no request is running, return result immediately."
  (let [owners (get-owners db)]
    (if owners
      (cons "nil" owners)
      ;; pending owners...
      (list "t"))))

(defn get-tables-candidates [db prefix-1 & _]
  "Return tables candidates autocomplete list."
  (let [tables (if (not prefix-1)
                 ;; something#
                 (get-tables db)
                 (if (is-owner? db prefix-1)
                   ;; [owner].#<tables-list>
                   (get-tables db prefix-1)
                   ;; owners still pending or
                   ;; unknown?.# case
                   (list)))]
    (if tables
      (cons "nil" tables)
      ;; pending tables...
      (list "t"))))

(defn get-colomns-candidates [db prefix-1 & [prefix-2]]
  "Return colomns candidates autocomplete list."
  ;; Possible 2 cases:
  ;; 1. owner.table.#<colomns-list>
  ;; 2. [owner|table].#<tables-list|colomns-list>
  ;; In both cases consider prefix-1 as table
  (let [tables-list (get-tables db)]
    (if tables-list
      (if (in? tables-list prefix-1)
        ;; table.#<colomns-list>
        (let [table prefix-1
              ;; force columns-cache obtaining...
              columns-list (get-colomns db table true)]
          ;; ok - columns
          (cons "nil" columns-list))
        ;; unknown?.# case
        ;; nothing to complete
        (list "nil"))
      ;; no tables yet
      ;; pending tables...
      (list "t"))))

(defn get-cache []
  "Output actual cache."
  @cache)

(defn invalidate-cache [db]
  "Clean your current connection cache (database owners and tables list)."
  (swap! cache assoc-in [db] nil))
