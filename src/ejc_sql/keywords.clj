;;; keywords.clj -- Keep database-specific keywords.

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

(ns ejc-sql.keywords)

(def keywords
  {:sqlite
   ["ABORT" "ACTION" "ADD" "AFTER" "ALL" "ALTER" "ANALYZE" "AND" "AS" "ASC" "ATTACH"
    "AUTOINCREMENT" "BEFORE" "BEGIN" "BETWEEN" "BY" "CASCADE" "CASE" "CAST" "CHECK"
    "COLLATE" "COLUMN" "COMMIT" "CONFLICT" "CONSTRAINT" "CREATE" "CROSS" "CURRENT"
    "CURRENT_DATE" "CURRENT_TIME" "CURRENT_TIMESTAMP" "DATABASE" "DEFAULT"
    "DEFERRABLE" "DEFERRED" "DELETE" "DESC" "DETACH" "DISTINCT" "DO" "DROP" "EACH"
    "ELSE" "END" "ESCAPE" "EXCEPT" "EXCLUSIVE" "EXISTS" "EXPLAIN" "FAIL" "FILTER"
    "FOLLOWING" "FOR" "FOREIGN" "FROM" "FULL" "GLOB" "GROUP" "HAVING" "IF" "IGNORE"
    "IMMEDIATE" "IN" "INDEX" "INDEXED" "INITIALLY" "INNER" "INSERT" "INSTEAD"
    "INTERSECT" "INTO" "IS" "ISNULL" "JOIN" "KEY" "LEFT" "LIKE" "LIMIT" "MATCH"
    "NATURAL" "NO" "NOT" "NOTHING" "NOTNULL" "NULL" "OF" "OFFSET" "ON" "OR" "ORDER"
    "OUTER" "OVER" "PARTITION" "PLAN" "PRAGMA" "PRECEDING" "PRIMARY" "QUERY" "RAISE"
    "RANGE" "RECURSIVE" "REFERENCES" "REGEXP" "REINDEX" "RELEASE" "RENAME" "REPLACE"
    "RESTRICT" "RIGHT" "ROLLBACK" "ROW" "ROWS" "SAVEPOINT" "SELECT" "SET" "TABLE"
    "TEMP" "TEMPORARY" "THEN" "TO" "TRANSACTION" "TRIGGER" "UNBOUNDED" "UNION"
    "UNIQUE" "UPDATE" "USING" "VACUUM" "VALUES" "VIEW" "VIRTUAL" "WHEN" "WHERE"
    "WINDOW" "WITH" "WITHOUT"]})
