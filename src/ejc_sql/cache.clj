;;; cache.clj -- Keep database stucture cache.

;;; Copyright © 2019-2023 - Kostafey <kostafey@gmail.com>

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
  (:require [clomacs :as clomacs]))

(def cache
  "Keep information about structure of databases used for autocomplete & eldoc.
  This data contains (depends on database type):
  ├── owners/schemas
  |   ├── tables
  |   |   └── columns
  |   └── views
  ├── packages
  |   └── stored procedures & functions
  |       └── parameters
  └── keywords
  It's global: same database structure information shared beetween
  different buffers connected to the same database."
  (atom {}))

(def cache-creation-promises
  "Signs of database structure cache created."
  (atom {}))

(defn get-cache
  "Get actual cache."
  []
  @cache)

(defn deref-cache [db]
  [db
   (into
    {}
    (map
     (fn [entity-k]
       [entity-k
        (let [item (get-in (get-cache) [db entity-k])]
          (if (future? item)
            (deref item)
            (if (map? item)
              (into
               {}
               (map
                (fn [sub-entity-k]
                  [sub-entity-k
                   (let [sub-item (get-in (get-cache)
                                          [db entity-k sub-entity-k])]
                     (if (future? sub-item)
                       (deref sub-item)
                       sub-item))])
                (keys item)))
              item)))])
     (keys
      ((get-cache) db))))])

(defn output-cache
  "Output actual cache to printable format."
  [db]
  (clomacs/format-result
   (if db
     (second (deref-cache db))
     (into {} (map deref-cache (keys (get-cache)))))))

(defn invalidate-cache
  "Clean your current connection cache (database owners and tables list)."
  [db]
  (swap! cache assoc-in [db] nil)
  (swap! cache-creation-promises assoc-in [db] nil))
