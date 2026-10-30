;;; lib.clj -- Misc clojure functions for ejc-sql emacs extension.

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

(ns ejc-sql.lib
  (:require [clojure.string :as s]))

(def ^:dynamic *max-column-width* 30)

(def windows?
  "The value is true if it runs under the os Windows."
  (s/includes? (System/getProperty "os.name") "Windows"))

(defn in?
  "true if `seq` contains `elm`.
  Search case insensitive wthen `case-sensitive` is false."
  [seq elm & {:keys [case-sensitive] :or {case-sensitive true}}]
  (when (and elm seq)
    (if (not case-sensitive)
      (in? (mapv s/lower-case seq) (s/lower-case elm))
      (some #(= elm %) seq))))

(defn get->in
  "Case insensitive `get-in` for last key in sequence."
  [obj path & {:keys [case-sensitive] :or {case-sensitive false}}]
  (if case-sensitive
    (get-in obj path)
    (get-in obj
            (conj (into [] (butlast path))
                  (last path))
            (get-in obj
                    (conj (into [] (butlast path))
                          (s/upper-case (last path)))
                    (get-in obj
                            (conj (into [] (butlast path))
                                  (s/lower-case (last path))))))))

(defn simple-join [n s]
  (s/join
   (for [_ (range 0 n)] s)))

(defn is-clob?
  "true if `x` is a CLOB, `oracle.sql.CLOB` included: it implements
  `java.sql.Clob`. Called on every value read, so it must stay cheap, with no
  class lookup on the classpath."
  [x]
  (instance? java.sql.Clob x))

(defn clob-to-string
  "The text of `clob`, line breaks kept. Unless `single-record?`, it's cut to
  `*max-column-width*`, ending with ..., and only that much is read, since a
  CLOB can be large."
  ([clob]
   (clob-to-string clob true))
  ([^java.sql.Clob clob single-record?]
   (if (or single-record? (not *max-column-width*))
     (slurp (.getCharacterStream clob))
     ;; One character more tells whether the text has to be cut.
     (let [result (.getSubString clob 1 (inc *max-column-width*))]
       (if (> (count result) *max-column-width*)
         (str (subs result 0 (- *max-column-width* 3)) "...")
         result)))))

(defn lob-to-string-row
  "Check all data in row if it's a LOB and convert CLOB/BLOB to string."
  [row single-record?]
  (mapv (fn [field]
          (cond
            (is-clob? field) (clob-to-string field single-record?)
            (bytes? field) (String. ^bytes field)
            :else field))
        row))

(def comments-re
  "Regex to search comments in SQL expression.
Line comment starts with `--` or, in MySQL, with `#` and lasts until the line
break or the end of the SQL expression, hence the trailing line break is
optional. Both `\\n` and `\\r\\n` line breaks are handled."
  (java.util.regex.Pattern/compile
   "(?:/\\*.*?\\*/[\\s;]*(?:\\r?\\n)?)|(?:(?:--|#)[^\\r\\n]*(?:\\r?\\n)?)",
   java.util.regex.Pattern/DOTALL))

(defn clean-sql [sql]
  (-> sql
      (s/replace #"(--).*\n" "")
      (s/replace #"(  )|( \t)|\t" " ")
      (s/replace "^\n" "")
      s/trim))

(def select-words
  '("SELECT"
    "SHOW"
    "WITH"
    "EXPLAIN"))

(def dml-words
  (concat
   select-words
   '("INSERT"
     "UPDATE"
     "DELETE")))

(def ddl-words
  '("CREATE"
    "ALTER"
    "DROP"
    "RENAME"))

(defn determine-query
  "Determine if current SQL starts with one of the words from `words` list."
  [sql words]
  (let [sql (clean-sql sql)
        ignore-set #{\( \[}
        pos (loop [pos 0]
              (if (ignore-set (get sql pos))
                (recur (inc pos))
                pos))
        sql (s/upper-case (subs sql pos))]
    (first (filter (fn [w] (s/starts-with? sql w)) words))))

(defn select? [sql]
  (determine-query sql select-words))

(defn dml?
  "Determine if current SQL is Data Manipulation Language (DML) case."
  [sql]
  (determine-query sql dml-words))

(defn ddl?
  "Determine if current SQL is Data Definition Language (DDL) case."
  [sql]
  (determine-query sql ddl-words))

(def read-only-words
  "Statements that only read: the first keyword of SQL `read-only?` accepts."
  '("SELECT"
    "WITH"
    "VALUES"
    "SHOW"
    "EXPLAIN"
    "DESCRIBE"
    "DESC"))

(defn- first-keyword-re
  "Regex to find the first word of SQL expression after whitespace, `(` and
comments, the line ones starting with `line-comment`, a regex. A block
comment holding `/*` or starting with `!` or `M!` stops the search, and
nothing is matched, since the quantifiers are possessive."
  [line-comment]
  (java.util.regex.Pattern/compile
   (str "^(?:[\\s(]"
        "|(?:" line-comment ")[^\\r\\n]*+"
        "|/\\*(?!M?!)(?:(?!/\\*|\\*/).)*+\\*/"
        ")*+(\\w+)")
   java.util.regex.Pattern/DOTALL))

(def ^:private first-keyword-res
  "`first-keyword-re` by whether `#` starts a line comment."
  {false (first-keyword-re "--")
   true (first-keyword-re "--|#")})

(defn first-keyword
  "The first word of `sql` upper-cased, leading comments and `(` skipped,
  `#` lines among the comments if `hash-comments?`, as in MySQL and MariaDB.
  nil if `sql` doesn't start with a word, or a comment before the word may
  be read differently by the database: a block comment holding `/*`, which
  PostgreSQL and SQL Server nest, or an executable comment of MySQL and
  MariaDB, `/*!...*/` or `/*M!...*/`, which they run. A quoted name is not
  a word: `[SELECT]` calls a procedure on SQL Server."
  [sql & [hash-comments?]]
  (when-let [[_ word] (re-find (first-keyword-res (boolean hash-comments?))
                               sql)]
    (s/upper-case word)))

(defn read-only?
  "Determine if `sql` starts with one of `read-only-words`, as
  `first-keyword` finds it, `hash-comments?` passed on. Unlike
  `determine-query` the whole word is matched: `SELECTX` is not `SELECT`."
  [sql & [hash-comments?]]
  (boolean (in? read-only-words (first-keyword sql hash-comments?))))
