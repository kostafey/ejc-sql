;;; cache.clj -- Keep database stucture cache.

;;; Copyright © 2019 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.cache)

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

(defn get-cache []
  "Output actual cache."
  @cache)

(defn invalidate-cache [db]
  "Clean your current connection cache (database owners and tables list)."
  (swap! cache assoc-in [db] nil)
  (swap! cache-creation-promises assoc-in [db] nil))

