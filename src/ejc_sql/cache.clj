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
  (:use [ejc-sql.lib])
  (:require
   [clojure.java.jdbc :as j]
   [ejc-sql.connect :as c]))

(def structure (atom {}))

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
                        " WHERE owner = "owner"              \n"))
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
   })

(defn select-db-meta-script [db-type meta-type &
                             {:keys [owner
                                     table
                                     entity-name]}]
  "Return SQL request to obtain some database structure info."
  (let [db-type (if (keyword? db-type) db-type (keyword db-type))
        meta-type (if (keyword? meta-type) meta-type (keyword meta-type))
        sql-receiver (get-in queries [db-type meta-type])
        sql (sql-receiver :owner owner
                          :table table
                          :entity-name entity-name)]
    sql))

(defn- get-single-row-result [db sql]
  (if sql
    (rest
     (flatten
      (j/query db (list sql) {:as-arrays? true})))))

(defn- get-first-row-result [db sql]
  (if sql
    (rest
     (map first
          (j/query db (list sql) {:as-arrays? true})))))

(defn- get? [obj & [force?]]
  (if (and obj (or force? (realized? obj)))
    @obj))

(defn get-owners [db]
  "Return owners list from cache if already received from DB,
check if receiveing process is not running, then start it."
  (let [db-type (keyword (:subprotocol db))
        need-owners? (get-in queries [db-type :owners])]
    (if need-owners?
      (do
        (if (not (get-in @structure [db :owners]))
          (swap! structure assoc-in [db :owners]
                 (future ((fn [db]
                            (let [db-type (keyword (:subprotocol db))
                                  sql (select-db-meta-script db-type :owners)]
                              (get-single-row-result db sql))) db))))
        (get? (get-in @structure [db :owners]))))))

(defn get-tables [db & [owner]]
  "Return tables list for this owner from cache if already received from DB,
check if receiveing process is not running, then start it."
  (let [;; default owner
        owner (:user db)]
    (if (not (get-in @structure [db :tables (keyword owner)]))
      (swap! structure assoc-in [db :tables (keyword owner)]
             (future ((fn [db owner]
                        (let [db-type (keyword (:subprotocol db))
                              sql (select-db-meta-script db-type :tables
                                                         :owner owner)]
                          (get-first-row-result db sql)))
                      db owner))))
    (get? (get-in @structure [db :tables (keyword owner)]))))

(defn get-colomns [db table force?]
  "Return colomns list for this table from cache if already received from DB,
check if receiveing process is not running, then start it."
  (if (not (get-in @structure [db :colomns (keyword table)]))
    (swap! structure assoc-in [db :colomns (keyword table)]
           (future ((fn [db table]
                      (let [db-type (keyword (:subprotocol db))
                            sql (select-db-meta-script db-type :columns
                                                       :table table)]
                        (get-single-row-result db sql)))
                    db table))))
  (get? (get-in @structure [db :colomns (keyword table)]) force?))

(defn get-stucture [db prefix-1 prefix-2]
  (let [db-type (keyword (:subprotocol db))
        need-owners? (get-in queries [db-type :owners])]
    ;; Check against following cases:
    ;; prefix-2.prefix-1.#
    ;; prefix-1.#
    ;; something#
    (cond
      ;; owner.table.#<colomns-list>
      prefix-2 (let [table prefix-1
                     colomns-list (get-colomns db table true)]
                 (if colomns-list
                   (cons "nil" colomns-list)
                   ;; pending colomns...
                   (list "colomns")))
      ;; [owner|table].#<tables-list|colomns-list>
      prefix-1 (if need-owners?
                 (let [owners-list (get-owners db)]
                   (if owners-list
                     (if (in? owners-list prefix-1)
                       ;; owner.#<tables-list>
                       (let [owner prefix-1
                             tables-list (get-tables db)]
                         (if tables-list
                           ;; ok - tables
                           (cons "nil" tables-list)
                           ;; pending tables...
                           (list "tables")))
                       ;; not-owner.#<tables-list>
                       (let [tables-list (get-tables db)]
                        (if tables-list
                          (if (in? tables-list prefix-1)
                            ;; table.#<colomns-list>
                            (let [table prefix-1
                                  ;; force columns-cache obtaining...
                                  columns-list (get-colomns db table true)]
                              ;; ok - columns
                              (cons "nil" columns-list))
                            ;; unknown.# case
                            ;; nothing to complete
                            (list "nil"))
                          ;; no tables yet
                          ;; pending tables...
                          (list "tables"))))
                     ;; no owners yet
                     ;; pending owners...
                     (list "owners"))))
      ;; #<owners-list&tables-list>
      :else (let [owners-list (get-owners db)
                  tables-list (get-tables db)]
              (cons (cond
                      (and (not owners-list)
                           (not tables-list)) "owners and tables"
                      (not owners-list) "owners"
                      (not tables-list) "tables"
                      :else "nil")
                  (distinct (concat owners-list tables-list)))))))

(defn invalidate-cache [db]
  (swap! structure assoc-in [db] nil))

(comment
  (:user @c/db)
  (get-owners @c/db)
  @structure
  (get-tables @c/db)
  (invalidate-cache @c/db)
  (get-stucture @c/db nil nil)
  )
