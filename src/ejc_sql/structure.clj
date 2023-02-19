;;; structure.clj -- Receive database stucture and keep it in cache.

;;; Copyright © 2016-2023 - Kostafey <kostafey@gmail.com>

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
  (:use [ejc-sql.lib]
        [ejc-sql.cache]
        [clomacs])
  (:require
   [clojure.java.jdbc :as j]
   [clojure.string :as s]
   [clojure.java.io :as io]
   [ejc-sql.connect :as c]
   [ejc-sql.output :as o]
   [ejc-sql.keywords :as k]))

(defn- safe-query
  "Return `sql` query result or nil in case of error."
  [db sql & {:keys [row-fn column-name]
             :or {row-fn identity}}]
  (try
    (if column-name
      [[(keyword column-name)]
       (mapv (keyword column-name)
             (row-fn (j/query db (list sql) {:as-arrays? false})))]
      (j/query db (list sql) {:as-arrays? true
                              :row-fn row-fn}))
    (catch Exception _)))

(defn- db->column
  "Execute `sql`, return `column-name` or first column
  (if `column-name` is not passed) of result set as result list."
  [db sql & {:keys [row-fn column-name]
             :or {row-fn identity}}]
  (when sql
    (rest (map first (safe-query db sql
                                 :row-fn row-fn
                                 :column-name column-name)))))

(defn- db->>column
  "Execute `sql`, return last column of result set as result list."
  [db sql]
  (when sql
    (rest
     (map last
          (j/query db (list sql) {:as-arrays? true})))))

(defn- db->value
  "Execute `sql`, return first value of first column of result set as result."
  [db sql & {:keys [row-fn]
             :or {row-fn identity}}]
  (first (db->column db sql :row-fn row-fn)))

(defn get-ms-sql-server-version [ver]
  (->
   (filter not-empty (.split ver " "))
   (nth 3)
   Integer/parseInt))

(defn is-modern-sql-server? [db]
  (let [ver (db->value db "SELECT @@VERSION AS 'SQL Server Version'")]
    (if (.startsWith ver "Microsoft SQL Azure")
      true
      (> (get-ms-sql-server-version ver) 2000))))

(def default-queries
  {:owners  (fn [& _] "
              SELECT schema_owner
              FROM information_schema.schemata ")
   :schemas (fn [& _] "
              SELECT schema_name
              FROM information_schema.schemata ")
   :tables  (fn [& {:keys [schema]}]
              (format "
               SELECT table_name
               FROM information_schema.tables
               %s
               ORDER BY table_name "
                      (if schema
                        (format " WHERE table_schema = '%s'" schema)
                        "")))
   :views   (fn [& _] "
              SELECT table_name
              FROM information_schema.views ")
   :all-tables (fn [& _] "
                 SELECT s.schema_owner, s.schema_name, t.table_name
                 FROM information_schema.schemata AS s,
                      information_schema.tables AS t
                 WHERE t.table_schema = s.schema_name ")
   :columns (fn [& {:keys [table]}]
              (format "
               SELECT column_name
               FROM information_schema.columns
               WHERE UPPER(table_name) = '%s'
               ORDER BY column_name "
                      (s/upper-case table)))
   :view    (fn [& {:keys [entity-name]}]
              (format "
               SELECT v.view_definition
               FROM information_schema.views AS v
               WHERE UPPER(v.table_name) = '%s' "
                      (s/upper-case entity-name)))})

(def queries
  "Database - specific queries used for obtain data about database structure."
  {
   ;;--------
   :oracle
   ;;--------
   {:entity      (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT text
                    FROM all_source
                    WHERE UPPER(name) = '%s' "
                           (s/upper-case entity-name)))
    :entity-type (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT object_type
                    FROM all_objects
                    WHERE UPPER(OBJECT_NAME) = '%s'
                      AND object_type NOT IN ('SYNONYM', 'PACKAGE BODY')
                    UNION
                    SELECT 'PROCEDURE' FROM all_procedures
                    WHERE UPPER(OBJECT_NAME) = '%s'
                      AND object_type = 'PROCEDURE'
                      AND rownum = 1"
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)))
    :view        (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT text
                    FROM all_views
                    WHERE UPPER(view_name) = '%s' "
                           (s/upper-case entity-name)))
    :types       (fn [& _] "
                    SELECT * FROM USER_TYPES ")
    :owners      (fn [& _] "
                    SELECT DISTINCT(owner)
                    FROM ALL_OBJECTS
                    ORDER BY owner ")
    :tables      (fn [& {:keys [owner]}]
                   (format "
                    SELECT table_name, owner
                    FROM all_tables
                    %s
                    ORDER BY table_name "
                           (if owner
                             (format " WHERE UPPER(owner) = '%s' "
                                     (s/upper-case owner))
                             "")))
    :views       (fn [& {:keys [owner]}]
                   (format "
                    SELECT view_name
                    FROM all_views
                    %s
                    ORDER BY view_name "
                           (if owner
                             (format " WHERE UPPER(owner) = '%s' "
                                     (s/upper-case owner))
                             "")))
    :all-tables  (fn [& _] "
                    SELECT owner, table_name
                    FROM all_tables
                    WHERE owner NOT IN ('SYS', 'SYSTEM')
                    ORDER BY owner ")
    :columns     (fn [& {:keys [table]}]
                   (format "
                    SELECT column_name
                    FROM ALL_TAB_COLUMNS
                    WHERE UPPER(table_name) = '%s' "
                           (s/upper-case table)))
    :constraints (fn [& {:keys [owner table]}]
                   (cond
                     (and owner table)
                     (format "
                      SELECT * FROM all_constraints
                      WHERE owner = '%s'
                        AND table_name = '%s' "
                             owner table)
                     table
                     (format "
                      SELECT * FROM all_constraints
                      WHERE table_name = '%s' " table)
                     :else
                     "SELECT * FROM user_constraints"))
    :procedures  (fn [& {:keys [owner]}]
                   (format "
                    SELECT object_name, procedure_name
                    FROM all_procedures
                    %s " (if owner
                           (format "WHERE owner = '%s' " owner)
                           "")))
    :parameters (fn [& {:keys [package entity-name]}]
                  (format "
                      SELECT data_type, argument_name
                      FROM SYS.ALL_ARGUMENTS
                      WHERE object_name  = '%s'
                        AND in_out = 'IN'
                        %s "
                          (s/upper-case entity-name)
                          (if package
                            (format "AND package_name = '%s'" package)
                            "")))
    :package   (fn [& {:keys [entity-name]}]
                 ;; entity-name is a package name here
                 (format "
                  SELECT text FROM
                  (
                    SELECT text AS text,
                           1 AS ordered,
                           line AS line
                    FROM all_source
                    WHERE UPPER(name) = '%s'
                      AND type = 'PACKAGE'
                    UNION SELECT '\n\n',                     2, 1 from dual
                    UNION SELECT '    ------------------\n', 3, 1 from dual
                    UNION SELECT '    -- PACKAGE BODY --\n', 4, 1 from dual
                    UNION SELECT '    ------------------\n', 5, 1 from dual
                    UNION SELECT '\n',                       6, 1 from dual
                    UNION
                    SELECT text,
                           7,
                           line
                    FROM all_source
                    WHERE UPPER(name) = '%s'
                      AND type = 'PACKAGE BODY'
                  )
                  ORDER BY ordered, line "
                         (s/upper-case entity-name)
                         (s/upper-case entity-name)))
    :table     (fn [& {:keys [entity-name]}]
                 (format "
                  SELECT dbms_metadata.get_ddl('TABLE','%s')
                  FROM dual " (s/upper-case entity-name)))
    :objects   (fn [& _] "
                 SELECT * FROM all_objects
                 WHERE object_type IN ('FUNCTION','PROCEDURE','PACKAGE') ")
    :keywords  (fn [& _] "
                 SELECT * FROM V$RESERVED_WORDS ORDER BY keyword ")}
   ;;--------
   :informix
   ;;--------
   {:owners nil
    :tables  (fn [& _] "
               SELECT TRIM(t.tabname) as tablesList
               FROM systables AS t
               WHERE t.tabtype = 'T'
                 AND t.tabid >= 100
               ORDER BY t.tabname; ")
    :columns (fn [& {:keys [table]}]
               (format "
                SELECT TRIM(c.colname) AS column_name
                 FROM systables AS t, syscolumns AS c
                WHERE t.tabid = c.tabid
                  AND t.tabtype = 'T'
                  AND t.tabid >= 100
                  AND TRIM(t.tabname) = '%s'
                ORDER BY c.colno; " table))}
   ;;-------
   :mysql
   ;;-------
   {:owners  (fn [& _] "
               SELECT schema_name
               FROM information_schema.schemata ")
    :tables  (default-queries :tables)
    :all-tables (fn [& _] "
                  SELECT s.schema_name, t.table_name
                  FROM information_schema.schemata AS s,
                       information_schema.tables AS t
                  WHERE t.table_schema = s.schema_name ")
    :columns (default-queries :columns)
    :keywords (fn [& _] "
                  SELECT name FROM mysql.help_keyword")
    :entity-type (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT routine_type
                    FROM information_schema.routines
                    WHERE UPPER(routine_name) = '%s'
                    UNION
                    SELECT 'standard-function' FROM mysql.help_topic t
                    INNER JOIN mysql.help_category AS c
                      ON c.help_category_id = t.help_category_id
                      AND c.parent_category_id IN (SELECT help_category_id
                                                  FROM mysql.help_category
                                                  WHERE name = 'Functions')
                    WHERE UPPER(t.name) = '%s'
                    UNION
                    SELECT 'table' as table_name
                    FROM information_schema.tables
                    WHERE UPPER(table_name) = '%s'"
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)))
    :constraints (fn [& {:keys [owner table]}]
                   (cond
                     (and owner table)
                     (format "
                      SELECT column_name, constraint_name,
                             referenced_column_name, referenced_table_name
                      FROM information_schema.key_column_usage
                      WHERE constraint_schema = '%s'
                        AND table_name = '%s' "
                             owner table)
                     table
                     (format "
                      SELECT column_name, constraint_name,
                             referenced_column_name, referenced_table_name
                      FROM information_schema.key_column_usage
                      WHERE table_name = '%s' " table)
                     :else
                     "SELECT * FROM information_schema.key_column_usage"))
    :procedures (fn [& _] "
                  SELECT routine_schema, routine_name
                  FROM information_schema.routines ")
    :procedure (fn [& {:keys [entity-name]}]
                 (format "
                  SHOW CREATE PROCEDURE %s "
                         entity-name))
    :function (fn [& {:keys [entity-name]}]
                (format "
                  SHOW CREATE FUNCTION %s "
                        entity-name))
    :table (fn [& {:keys [entity-name]}]
             (format "SHOW CREATE TABLE %s " entity-name))
    :standard-function (fn [& {:keys [entity-name]}]
                         (format "
                  SELECT description FROM mysql.help_topic
                  WHERE UPPER(name) = '%s' "
                                 (s/upper-case entity-name)))
    :parameters (fn [& {:keys [entity-name]}]
                  (format "
                      SELECT data_type, parameter_name
                      FROM information_schema.parameters
                      WHERE parameter_mode = 'IN'
                        AND UPPER(specific_name) = '%s'"
                          (s/upper-case entity-name)))}
   ;;--------
   :h2
   ;;--------
   {:tables  (fn [& _] ((default-queries :tables) :schema "PUBLIC"))
    :views   (default-queries :views)
    :all-tables (fn [& _] "
                 SELECT s.schema_owner, s.schema_name, t.table_name
                 FROM information_schema.schemata AS s,
                      information_schema.tables AS t
                 WHERE t.table_schema = s.schema_name
                   AND LCASE(s.schema_name) != 'information_schema' ")
    :columns (default-queries :columns)
    :keywords (fn [& _]
                "SELECT topic FROM information_schema.help")
    :entity-type (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT t.table_type
                    FROM information_schema.tables AS t
                    WHERE UPPER(t.table_name) = '%s' "
                    (s/upper-case entity-name)))
    :view    (fn [& {:keys [entity-name]}]
               ((default-queries :view) :entity-name entity-name))}
   ;;--------
   :sqlite
   ;;--------
   {:tables     (fn [& _] "SELECT name FROM sqlite_master WHERE type='table'")
    :views      (fn [& _] "SELECT name FROM sqlite_master WHERE type='view'")
    :all-tables (fn [& _] "SELECT name FROM sqlite_master WHERE type='table'")
    :columns    (fn [& {:keys [table]}]
                  (format "
                   SELECT p.name as columnName
                   FROM sqlite_master m
                   LEFT OUTER JOIN pragma_table_info((m.name)) p
                        ON m.name <> p.name
                   WHERE m.name = '%s'
                   ORDER BY columnName " table))
    :keywords    (:sqlite k/keywords)
    :entity-type (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT type FROM sqlite_master
                    WHERE UPPER(name) = '%s' "
                           (s/upper-case entity-name)))
    :view        (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT sql FROM sqlite_master
                    WHERE UPPER(name) = '%s' "
                    (s/upper-case entity-name)))}
   ;;-------
   :sqlserver ; ms sql server
   ;;-------
   {:owners  (default-queries :owners)
    :schemas (default-queries :schemas)
    :tables  (default-queries :tables)
    :all-tables (default-queries :all-tables)
    :columns (default-queries :columns)
    :constraints (fn [& {:keys [table]}]
                   (format "
                    SELECT type_desc AS constraint_type,
                           name
                    FROM sys.objects
                    WHERE type_desc LIKE '%CONSTRAINT'
                      AND OBJECT_NAME(parent_object_id)='%s' " table))
    :entity      (fn [& {:keys [db entity-name]}]
                   (if (is-modern-sql-server? db)
                     (format "
                      SELECT definition
                      FROM sys.objects o
                      JOIN sys.sql_modules m ON m.object_id = o.object_id
                      WHERE o.object_id = object_id('%s') " entity-name)
                     (format "
                      SELECT c.text
                      FROM sysobjects o
                      JOIN syscomments c ON c.id = o.id
                      WHERE o.name = '%s' " entity-name)))}
   ;;-------
   :postgresql
   ;;-------
   {:procedure (fn [& {:keys [entity-name]}]
                 (format "
                  SELECT pg_get_functiondef(p.oid)
                  FROM   pg_catalog.pg_proc p
                  WHERE  upper(p.proname) = '%s' "
                         (s/upper-case entity-name)))
    :table (fn [& {:keys [entity-name]}]
             (format "
              SELECT 'CREATE TABLE %s (\n' ||
                  string_agg(column_list.column_expr, ', \n') ||
                  '\n);'
              FROM (
                SELECT '  ' || column_name || ' ' || data_type ||
                     coalesce('(' || character_maximum_length || ')', '') ||
                     case when is_nullable = 'YES'
                            then ''
                            else ' NOT NULL'
                          end as column_expr
                FROM information_schema.columns
                WHERE table_schema = 'public'
                  AND upper(table_name) = '%s'
                ORDER BY ordinal_position) column_list"
                     entity-name
                     (s/upper-case entity-name)))
    :owners     (default-queries :owners)
    :schemas    (default-queries :schemas)
    :tables     (default-queries :tables)
    :all-tables (default-queries :all-tables)
    :columns    (default-queries :columns)
    :procedures (fn [& _] "
                    SELECT p.proname
                    FROM   pg_catalog.pg_namespace n
                    JOIN   pg_catalog.pg_proc p
                      ON   p.pronamespace = n.oid
                    ORDER BY proname ")
    :entity-type (fn [& {:keys [entity-name]}]
                   (format "
                    SELECT 'namespace'
                    FROM   pg_catalog.pg_namespace n
                    WHERE  upper(n.nspname) = '%s'
                    UNION
                    SELECT 'table'
                    FROM   information_schema.tables
                    WHERE  upper(table_name) = '%s'
                    UNION
                    SELECT 'view'
                    FROM   information_schema.views
                    WHERE  upper(table_name) = '%s'
                    UNION
                    SELECT 'procedure'
                    FROM   pg_catalog.pg_namespace n
                    JOIN   pg_catalog.pg_proc p
                      ON   p.pronamespace = n.oid
                    WHERE  upper(p.proname) = '%s' "
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)
                           (s/upper-case entity-name)))
    :parameters (fn [& {:keys [entity-name]}]
                  (format "
                      SELECT pg_catalog.pg_get_function_identity_arguments(
                               p.oid)
                      FROM   pg_catalog.pg_proc p
                      WHERE  p.proname = '%s'" entity-name))
    :keywords (fn [& _]
                "SELECT word FROM pg_get_keywords()")}})

;; Use the same database introspection queries for MariaDB as for MySQL.
(def queries (assoc queries :mariadb (queries :mysql)))

(defn autocomplete-available-for-db? [db-type]
  (queries db-type))

(defn get-db-name [db]
  (let [{:keys [database subname connection-uri dbname]} db]
    (or dbname
        database
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
         (if connection-uri
           (second (.split
                    (let [props-list (.split connection-uri ";")]
                      (first
                       (filter
                        (fn [prop]
                          (=
                           "user"
                           (.toLowerCase (first (.split prop "=")))))
                        props-list))) "="))
           :undefined-user)))))

(def product-assoc {"oracle:sid" "oracle"})

(defn get-db-type [db]
  (let [{:keys [subprotocol connection-uri dbtype]} db
        result (or dbtype
                   subprotocol
                   (let [attrs (.split connection-uri ":")]
                     (if (= (second attrs) "jtds")
                       ;; jdbc:jtds:sqlserver://...
                       (nth attrs 2)
                       ;; jdbc:sqlserver://localhost\instance:1433;
                       (second attrs))))]
    (keyword (or (product-assoc result) result))))

(defn- get? [obj & [force?]]
  (when (and obj (or force? (realized? obj)))
    @obj))

(defn get-this-owner
  "Return current owner/schema."
  [db & [owner]]
  (or owner
      (get? (get-in @cache [db :this-owner]) false)
      (do
        (swap! cache assoc-in [db :this-owner]
               (future
                 ((fn [db]
                    (let [db-type (get-db-type db)]
                      (case db-type
                        :sqlserver (if (is-modern-sql-server? db)
                                     ;; Get default SQL Server schema for session
                                     (db->value db "SELECT SCHEMA_NAME()")
                                     "dbo")
                        :oracle (db->value
                                 db
                                 "SELECT sys_context('userenv', 'current_schema')
                                  FROM dual")
                        ;; By default
                        ;; Assume owner == schema (it can be wrong in general).
                        (get-user db)))) db)))
        (get? (get-in @cache [db :this-owner]) true))))

(defn select-db-meta-script
  "Return SQL request to obtain some database structure info."
  [db meta-type &
   {:keys [owner
           table
           package
           entity-name]}]
  (let [db-type (get-db-type db)
        meta-type (if (keyword? meta-type) meta-type (keyword meta-type))
        ;; owner (get-this-owner db owner)
        sql-receiver (get-in queries [db-type meta-type])
        sql (if sql-receiver
              (sql-receiver :db db
                            :owner owner
                            :schema owner
                            :table table
                            :package package
                            :entity-name entity-name
                            :db-name (get-db-name db)))]
    sql))

(defn get-namespace
  "Find tables namespace for current database type.
  Possible cases for namespace:
  * schema is sutable (or both schema and owner)
  for this DB type - use `:schemas`
  * only owner is sutable, so use `:owners`
  * none of them is sutable - `nil`"
  [db]
  (let [db-type (get-db-type db)]
    (cond
      (get-in queries [db-type :schemas]) :schemas
      (get-in queries [db-type :owners]) :owners
      :else nil)))

(defn get-or-create-cache
  "Fill `cache` atom with database structure data, then get data from it.
  Parameters:
  - `db` - database connection map
  - `path` - the keys used to define the place to store data in `cache` atom
  - `sql` - query to obtain the desired data from the database
  - `force?` - when true, eval `sql` immediately and return data
  - `direct-value` - sometimes there is no need to eval `sql` to obtain some
                   data,and value of this parameter, if it's defined, can be
                   used as-is
  - `exec-fn` - SQL execution function (`db->column` by default)
  - `filter-fn` - filer function applied to SQL execution results
  - `handler-fn` - additional processing applied to SQL execution results."
  [& {:keys [db
             path
             sql
             force?
             direct-value
             exec-fn
             filter-fn
             handler-fn]
      :or {exec-fn db->column}}]
  (let [full-path (into [db] path)]
    (if (not (get->in @cache full-path))
      (let [flat-path [db (keyword (s/join "-" (map name path)))]
            c-promise (promise)]
        (swap! cache-creation-promises assoc-in flat-path c-promise)
        (swap! cache assoc-in full-path
               (if direct-value
                 (future (deliver c-promise :ok)
                         direct-value)
                 (future (try
                           (let [result (exec-fn db sql)
                                 result (distinct result)
                                 result (if filter-fn
                                          (filter filter-fn result)
                                          result)
                                 result (if handler-fn
                                          (mapv handler-fn result)
                                          result)
                                 result (if (vector? result)
                                          (sort result)
                                          result)]
                             (deliver c-promise :ok)
                             result)
                           (catch Exception e
                             (deliver c-promise e)
                             (list))))))))
    (get? (get-in @cache full-path) force?)))

(defn get-owners
  "Return owners list from cache if already received from DB,
  check if receiveing process is not running, then start it."
  [db & [force?]]
  (let [namespace (get-namespace db)]
    (if namespace
      (get-or-create-cache :db db
                           :path [namespace]
                           :sql (select-db-meta-script db namespace)
                           :force? force?)
      ;; Schemas or owners are not applicable to this database type.
      (list))))

(defn get-tables
  "Return tables list for this owner from cache if already received from DB,
  check if receiveing process is not running, then start it."
  [db & [owner_ force?]]
  (let [owner (get-this-owner db owner_)]
    (if owner_
      (get-or-create-cache :db db
                           :path [:tables owner]
                           :sql (select-db-meta-script db :tables :owner owner)
                           :force? force?)
      (get-or-create-cache :db db
                           :path [:all-tables]
                           :sql (select-db-meta-script db :all-tables)
                           :force? force?
                           :exec-fn db->>column))))

(defn get-views
  "Return views list from cache if already received from DB,
  check if receiveing process is not running, then start it."
  [db & [owner_ force?]]
  (let [owner (get-this-owner db owner_)]
    (get-or-create-cache :db db
                         :path [:views owner]
                         :sql (select-db-meta-script db :views :owner owner)
                         :force? force?)))

(defn get-colomns
  "Return colomns list for this table from cache if already received from DB,
  check if receiveing process is not running, then start it."
  [db table force?]
  (when (not (get->in @cache [db :colomns table]))
    (swap! cache assoc-in [db :colomns table]
           (future ((fn [db table]
                      (let [sql (select-db-meta-script db :columns
                                                       :table table)]
                        (sort (distinct (db->column db sql)))))
                    db table))))
  (get? (get->in @cache [db :colomns table])
        force?))

(def postgresql-type-aliases
  "Some of PostgreSQL type aliases used for show in eldoc."
  {"character varying" "varchar"
   "bit varying" "varbit"})

(defn get-parameters
  "Return parameters list of `stored-procedure` from cache if already received
  from DB, check if receiveing process is not running, then start it."
  [db package stored-procedure & [force?]]
  (let [db-type (get-db-type db)]
    (get-or-create-cache
     :db db
     :path (if package
             [:stored-procedures package stored-procedure]
             [:stored-procedures stored-procedure])
     :sql (select-db-meta-script db :parameters
                                 :package package
                                 :entity-name stored-procedure)
     :force? force?
     :exec-fn
     (case db-type
       :postgresql (fn [db sql]
                     ;; Use only first possible list of parameters if many.
                     (list (db->value db sql)))
       :mysql (fn [db sql]
                (list (rest (safe-query db sql))))
       :oracle (fn [db sql]
                 (list
                  (map
                   (fn [[param-type param-name]]
                     (format "%s %s" param-type param-name))
                   (rest
                    (safe-query db
                                sql
                                :row-fn identity
                                :column-name nil)))))
       nil)
     :handler-fn
     (case db-type
       :postgresql (fn [types]
                     (map
                      (fn [type]
                        (let [type (s/trim type)]
                          (or (postgresql-type-aliases type)
                              type)))
                      (s/split types #",")))
       :mysql (fn [types]
                (map #(s/join " " %) types))
       nil))))

(defn get-keywords
  "Return keywords list for this database type from cache if already received
  from DB, check if receiveing process is not running, then start it."
  [db force?]
  (if-let [keywords-query (get-in queries [(get-db-type db) :keywords])]
    (get-or-create-cache :db db
                         :path [:keywords]
                         :direct-value (if (vector? keywords-query)
                                         keywords-query)
                         :sql (select-db-meta-script db :keywords)
                         :force? force?
                         :filter-fn #(not (nil? %)))))

(defn get-packages
  "Return packages list for this owner from cache if already received from DB,
  check if receiveing process is not running, then start it."
  [db & [owner_ force?]]
  (let [owner (get-this-owner db owner_)]
    (get-or-create-cache :db db
                         :path [:packages owner]
                         :sql (select-db-meta-script db :procedures
                                                     :owner owner)
                         :force? force?)))

(defn is-owner? [db prefix]
  (in? (get-owners db) prefix :case-sensitive false))

(clomacs-defn
 complete-auto-complete
 ejc-complete-auto-complete
 :doc "Call Emacs auto-complete if point is the same as before loading.")

(defn run-autocomplete [db buffer-name point]
  (future
    ;; Wait for all cache building promises ready.
    (mapv deref (vals (get-in @cache-creation-promises [db])))
    ;; Remove them from memory.
    (swap! cache-creation-promises assoc-in [db] nil)
    ;; Pass to Emacs a note that db structure cache ready (try to autocomplete).
    (complete-auto-complete buffer-name point)))

(defn autocomplete-loading
  "Not loaded tables yet. Output pending db structure data..."
  ([db buffer-name point]
   (run-autocomplete db buffer-name point)
   (autocomplete-loading))
  ([]
   (list "t")))

(defn autocomplete-result
  "Output autocomplete result."
  [result]
  (cons "nil" result))

(defn autocomplete-nothing
  "Output autocomplete nothing."
  []
  (list "nil"))

(defn get-owners-candidates
  "Return owners candidates autocomplete list from the database structure
  cache, async request to fill it, if not yet.
  The result list has the following structure:
  (pending item1 item2 ...)
  If `pending` is t - the async request to get the structure is running
  if `pending` is nil - no request is running, return result immediately."
  [& {:keys [db sql prefix-1 buffer-name point]}]
  (let [owners (get-owners db)]
    (if prefix-1
      ;; Assume schema|table.#<schemas-list> is not applicable.
      ;; So, return empty list.
      (autocomplete-nothing)
      (if owners
        (autocomplete-result owners)
        ;; pending owners...
        (autocomplete-loading db buffer-name point)))))

(defn get-tables-candidates
  "Return tables candidates autocomplete list."
  [& {:keys [db sql prefix-1 buffer-name point]}]
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
      (autocomplete-result tables)
      ;; pending tables...
      (autocomplete-loading))))

(defn get-views-candidates
  "Return views candidates autocomplete list."
  [& {:keys [db sql prefix-1 buffer-name point]}]
  (if-let [views (if (not prefix-1)
                 ;; something#
                 (get-views db)
                 (if (is-owner? db prefix-1)
                   ;; [owner].#<views-list>
                   (get-views db prefix-1)
                   ;; owners still pending or
                   ;; unknown?.# case
                   (list)))]
    (autocomplete-result views)
    ;; pending views...
    (autocomplete-loading)))

(defn get-packages-candidates
  "Return packages candidates autocomplete list."
  [& {:keys [db sql buffer-name point]}]
  (if-let [packages (get-packages db)]
    (autocomplete-result packages)
    ;; pending packages...
    (autocomplete-loading)))

(defn match-alias?
  "Check if prefix (`probable-alias`) is alias for `table` in this `sql`."
  [sql owner table probable-alias]
  (let [sql (s/lower-case sql)
        owner (if owner
                (str (s/lower-case owner) "\\.")
                "\\s+(\\w+\\.)?")
        table (s/lower-case table)
        probable-alias (s/lower-case probable-alias)
        alias-pattern (re-pattern
                       (str owner table "(\\s+as)?"
                            "\\s+" probable-alias))]
    (not (nil? (re-find alias-pattern sql)))))

(defn get-all-tables
  "Get all tables for all owners/schemas."
  [db]
  (or (get-tables db nil true)
      (flatten
       (mapv #(get-tables db % true)
             (distinct (get-owners db true))))))

(def select-re
  (re-pattern (str "(?i)\\s*SELECT\\s+(.*\\s+)?FROM\\s+(\\S+)")))

(defn select-sql?
  "If `sql` is SELECT query, return a table name used in FROM clause.
  Otherwise return nil."
  [sql]
  (nth (re-find select-re sql) 2))

(def insert-re
  (re-pattern (str "(?i)\\s*INSERT\\s+INTO\\s+(\\S+)\\s+")))

(defn insert-sql?
  "If `sql` is INSERT query, return a table name used to insert new records.
  Otherwise return nil."
  [sql]
  (second (re-find insert-re sql)))

(def update-re
  (re-pattern (str "(?i)\\s*UPDATE\\s(\\S+)\\s+\n*SET.*")))

(defn update-sql?
  "If `sql` is UPDATE query, return a table name used to modify the existing
  records. Otherwise return nil."
  [sql]
  (second (re-find update-re sql)))

(defn get-colomns-candidates
  "Return colomns candidates autocomplete list."
  [& {:keys [db sql prefix-1 prefix-2
             buffer-name point]}]
  ;; Possible cases:
  ;; 1. [owner|schema.]table.#<colomns-list>
  ;; 2. SELECT alias.#<colomns-list> FROM [owner|schema.]table AS alias
  ;; 3. SELECT complex-alias.#<colomns-list> FROM (SELECT...) AS complex-alias
  ;; 4. INSERT INTO table (field1, .#) values (123, 'text')
  ;; 5. UPDATE table SET field1 = 123, .# = 'text' WHERE id = 1
  ;;
  ;; In any case, consider `prefix-1` as table or alias
  ;; and optional `prefix-2` as owner or schema.
  ;; When no any prefix at all, check for 4 (INSERT) or 5 (UPDATE) case.
  (let [owner prefix-2
        tables-list (concat
                     (if owner
                       (get-tables db owner)
                       ;; In case of queries like
                       ;; "SELECT u.# FROM custom.Users as u"
                       ;; when "custom" is not the current
                       ;; schema, the full DB structure tree
                       ;; sould be built to obtain overall
                       ;; tables list for all owners/schemas.
                       (if (get-namespace db)
                         ;; Database has namespaces
                         (if (not (get-owners db))
                           ;; Not received owners list yet -
                           ;; pending...
                           (do
                             (future (get-all-tables db))
                             nil)
                           ;; Owners list is already here -
                           ;; force tables list receiving!
                           (get-all-tables db))
                         ;; Database hasn't namespaces -
                         ;; no owners needed
                         (get-tables db)))
                     (get-views db))]
    (if (not (and tables-list
                  (not-empty (filter not-empty tables-list))))
      ;; no tables yet
      ;; pending tables...
      (autocomplete-loading)
      ;; Tables list loaded:
      (if (not prefix-1)
        (let [insert-or-update-table (or (select-sql? sql)
                                         (insert-sql? sql)
                                         (update-sql? sql))]
          (cond
            (and (not-empty insert-or-update-table)
                 (in? tables-list insert-or-update-table
                      :case-sensitive false))
            ;; SELECT # FROM table
            ;; or
            ;; INSERT INTO table (field1, .#) values (123, 'text')
            ;; or
            ;; UPDATE table SET field1 = 123, .# = 'text' WHERE id = 1
            (autocomplete-result
             (get-colomns db insert-or-update-table true))
            ;; SELECT or other queries types
            :else (autocomplete-nothing)))
        (if (in? tables-list prefix-1 :case-sensitive false)
          ;; table.#<colomns-list>
          (let [table prefix-1
                ;; force columns-cache obtaining...
                columns-list (get-colomns db table true)]
            ;; ok - columns
            (autocomplete-result columns-list))
          (let [sql (clean-sql sql)
                table-alias (first
                             (filter
                              (fn [table]
                                (match-alias? sql owner table prefix-1))
                              tables-list))]
            (if table-alias
              ;; table-alias.#<colomns-list>
              (autocomplete-result (get-colomns db table-alias true))
              ;; Check "SELECT t.# FROM (SELECT ... ) AS t" case.
              (let [pattern (re-pattern (str "\\(.+\\)(\\s|\\s(as)\\s)"
                                             prefix-1))
                    complex-alias (first (re-find pattern sql))
                    complex-alias (if complex-alias
                                    (subs complex-alias 1
                                          (.lastIndexOf complex-alias ")")))]
                (if complex-alias
                  (let [{:keys [success result]} (c/query-meta db complex-alias)]
                    (if success
                      ;; complex-alias.#<colomns-list>
                      (autocomplete-result (mapv :name result))
                      ;; Can't execute "blah blah" to get metadata in
                      ;; "SELECT t.# FROM (blah blah) AS t" case.
                      (autocomplete-nothing)))
                  ;; unknown?.# case
                  ;; nothing to complete
                  (autocomplete-nothing))))))))))

(defn get-entity-type
  "Determine DB entity type, whether it is a `:view`, `:type`,
  `:package`, `:namespace` or `:procedure`."
  [db entity-name]
  (if-let [sql (and entity-name
                    (select-db-meta-script db :entity-type
                                           :entity-name entity-name))]
    (if-let [found-type (db->value db sql)]
      (-> (.split found-type " ")
          first
          s/lower-case
          keyword))))

(defn get-entity-type-fmt [db entity-name]
  (clomacs/format-result (get-entity-type db entity-name)))

(def creation-headers
  {:oracle {:view (fn [db view]
                    (format "CREATE OR REPLACE VIEW %%s\n(%s)\nAS\n%%s"
                            (s/join ", "
                                    (map #(-> % :name s/lower-case)
                                         (:result
                                          (c/query-meta db view))))))
            :package (fn [db _] "CREATE OR REPLACE %2$s\n")}})

(defn add-creation-header [db entity-type entity-name sql]
  (let [db-type (get-db-type db)]
    (if-let [header-creator (get-in creation-headers [db-type entity-type])]
      (let [header-format (header-creator db entity-name)]
        (format header-format entity-name sql))
      sql)))

(defn get-entity-description
  "Get DB entity creation SQL.
  Database entity types:
  - `:table`
  - `:view`
  - `:package`
  - `:function`
  - `:procedure`
  - `:standard-function`."
  [& {:keys [db
             connection-name
             prefix
             entity-name
             result-file]}]
  (let [sql-object (or prefix entity-name)
        db-type (get-db-type db)]
    (if-let [type (get-entity-type db sql-object)]
      (if (and prefix (= :namespace type))
        ;; Omit prefix if it's a `:namespace`.
        (get-entity-description :db db
                                :connection-name connection-name
                                :prefix nil
                                :entity-name entity-name
                                :result-file result-file)
        (if-let [entity-obtainig-sql (select-db-meta-script
                                      db
                                      type
                                      :entity-name sql-object)]
          (if-let [entity-sql (s/join
                               ""
                               (db->column
                                db
                                entity-obtainig-sql
                                :row-fn (fn [row]
                                          (mapv #(if (is-clob? %)
                                                   (clob-to-string %)
                                                   %)
                                                row))
                                :column-name
                                (case (get-db-type db)
                                  :mysql (case type
                                           :function "create function"
                                           :procedure "create procedure"
                                           :table "create table"
                                           nil)
                                  nil)))]
            (c/complete (o/unify-str
                         (add-creation-header
                          db
                          type
                          sql-object
                          (o/format-sql-if-required entity-sql)))
                        :mode (if (and (= type :standard-function)
                                       (= db-type :mysql))
                                'rst-mode
                                'sql-mode)
                        :connection-name connection-name
                        :db db
                        :result-file result-file
                        :goto-symbol (if prefix entity-name))
            (c/complete (format "Can't find %s named %s."
                                (name type) sql-object)
                        :result-file result-file))
          (c/complete (format (str "Script for obtaining DB entity of type %s "
                                   "was not added for this database type.")
                              (name type))
                      :result-file result-file)))
      (c/complete (format "Can't determine type of %s." sql-object)
                  :result-file result-file))))

(defn print-table-meta [db table-name]
  (let [result-map (c/table-meta db table-name)
        success (:success result-map)
        result-data (:result result-map)]
    (if success
      (do (println (format "Table \"%s\" description:" table-name))
          (println)
          (o/print-maps result-data true))
      (println result-data))
    success))

(defn describe-table [& {:keys [db
                                connection-name
                                table
                                owner
                                result-file
                                add-outside-borders]}]
  (with-open [out (io/writer result-file)]
    (binding [*out* out
              o/*add-outside-borders* add-outside-borders]
      (when (print-table-meta db (if owner (str owner "." table) table))
        (when-let [sql (select-db-meta-script db :constraints
                                              :owner owner
                                              :table table)]
          (println)
          (println "Constraints:")
          (println)
          (o/print-table (second (c/eval-sql-core :db db
                                                  :sql sql
                                                  :fetch-size 0
                                                  :max-rows 0)))))))
  (c/complete
   nil
   :display-result true
   :connection-name connection-name
   :db db
   :result-file result-file))
