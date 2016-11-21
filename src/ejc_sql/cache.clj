;;; cache.clj -- Receive database stucture.

;;; Copyright © 2016 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.cache
  (:require
   [clojure.java.jdbc :as j]
   [ejc-sql.connect :as c]))

(def structure (atom {}))

(def queries
  {
   ;;--------
   :oracle
   ;;--------
   {:entity (fn [entity-name]
              (str " SELECT text             \n"
                   " FROM all_source         \n"
                   " WHERE name = '"entity-name"'"))
    :types (fn [] "SELECT * FROM USER_TYPES")
    :owners (fn [] (str " SELECT DISTINCT(owner) \n"
                        " FROM ALL_OBJECTS       \n"
                        " ORDER BY owner         \n"))
    :tables (fn [& [owner]]
              (str " SELECT table_name, owner \n"
                   " FROM all_tables          \n"
                   (if owner
                     (str " WHERE owner = '"owner"'"))
                   " ORDER BY table_name"))
    :columns (fn [table]
               (str " SELECT column_name      \n"
                    " FROM ALL_TAB_COLUMNS    \n"
                    " WHERE table_name = '"table"'"))
    :constraints (fn [& [owner table]]
                   (if table
                     (str " SELECT * FROM all_constraints    \n"
                          " WHERE owner = "owner"            \n"
                          "       AND table_name = '"table"' \n")
                     "SELECT * FROM user_constraints"))
    :procedures (fn [owner]
                  (str " SELECT object_name, procedure_name \n"
                       " FROM all_procedures                \n"
                       " WHERE owner = "owner"              \n"))
    :objects (fn []
               (str "SELECT * FROM all_objects WHERE object_type IN "
                    "('FUNCTION','PROCEDURE','PACKAGE')"))}
   ;;--------
   :informix
   ;;--------
   {:owners nil
    :tables (fn []
              (str " SELECT TRIM(t.tabname) as tablesList \n"
                   " FROM systables AS t                  \n"
                   " WHERE t.tabtype = 'T'                \n"
                   "   AND t.tabid >= 100                 \n"
                   " ORDER BY t.tabname;                  \n"))
    :columns (fn [table]
               (str " SELECT TRIM(c.colname) AS column_name \n"
                    "  FROM systables AS t, syscolumns AS c \n"
                    " WHERE t.tabid = c.tabid               \n"
                    "   AND t.tabtype = 'T'                 \n"
                    "   AND t.tabid >= 100                  \n"
                    "   AND TRIM(t.tabname) = '" table "'   \n"
                    " ORDER BY c.colno;                     \n"))}
   })

(defn- get-one-row-result [db sql]
  (if sql
    (rest
     (flatten
      (j/query db (list sql) {:as-arrays? true})))))

(defn- get-first-row-result [db sql]
  (if sql
    (rest
     (map first
          (j/query db (list sql) {:as-arrays? true})))))

(defn get-owners [db]
  (let [db-type (keyword (:subprotocol db))
        sql-receiver (get-in queries [db-type :owners])
        sql (sql-receiver)]
    (get-one-row-result db sql)))

(defn get-tables [db owner]
  (let [db-type (keyword (:subprotocol db))
        sql-receiver (get-in queries [db-type :tables])
        sql (sql-receiver owner)]
    (get-first-row-result db sql)))

(defn get-stucture [db context]
  (let [owner (:user db)
        owners-list (get-in @structure [db :owners])
        tables-list (get-in @structure [db :tables (keyword owner)])]
    (if (not owners-list)
      (swap! structure assoc-in [db :owners]
             (future (get-owners db))))
    (if (not tables-list)
      (swap! structure assoc-in [db :tables (keyword owner)]
             (future (get-tables db owner))))
    ;; ----------
    ;; (if (and owners-list (realized? owners-list))
    ;;   @owners-list)
    (if (and tables-list (realized? tables-list))
      @tables-list)))

(defn invalidate-cache [db]
  (swap! structure assoc-in [db] nil))

(comment
  @structure
  (invalidate-cache @c/db)
  (get-stucture @c/db nil)
  )
