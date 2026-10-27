;;; mcp.clj -- MCP tools over the databases connected in ejc-sql.

;;; Copyright © 2026 - Kostafey <kostafey@gmail.com>

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

;;; The Model Context Protocol (MCP) server core: the registry of the
;;; connections the user connected to in Emacs, the configuration file, the
;;; tools and the JSON-RPC dispatcher. The transport is `ejc-sql.mcp-http'.
;;;
;;; Nothing here writes `ejc-sql.connect/db' or `current-query': an agent's
;;; query must neither be cancelled by the user's `ejc-cancel-query' nor
;;; cancel the user's query.

(ns ejc-sql.mcp
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as j]
            [clojure.string :as s]
            [ejc-sql.cache :as cache]
            [ejc-sql.connect :as c]
            [ejc-sql.lib :as lib]
            [ejc-sql.output :as o]
            [ejc-sql.structure :as structure])
  (:import [java.io File]
           [java.sql Connection SQLException SQLTimeoutException]
           [java.util Base64]
           [java.util.regex Pattern]))

;;; Registry

(defonce ^{:doc "The connections the user connected to: {name db-spec}."}
  registry
  (atom {}))

(defn register-db!
  "Make the db-spec `db` available to the agents under the Emacs connection
  `name`. A spec registered under this name before is replaced."
  [name db]
  (swap! registry assoc name db))

(defn connections
  "The registered connections: {name db-spec}."
  []
  @registry)

(defn connection-names
  "The registered connection names, sorted."
  []
  (sort (keys @registry)))

;;; Configuration

(def config-file
  "The MCP server configuration. The server runs only while the file exists."
  (io/file (System/getProperty "user.home") ".ejc-sql" "mcp.edn"))

(defn- config-problem
  "Describe what's wrong with the parsed `config`, nil if nothing."
  [config]
  (let [{:keys [port token writable]} config]
    (cond
      (not (map? config))
      "not a map"
      (not (or (nil? port) (and (int? port) (<= 0 port 65535))))
      ":port must be an integer from 0 to 65535"
      (not (or (nil? token) (and (string? token) (not (s/blank? token)))))
      ":token must be a non-blank string"
      (not (or (nil? writable) (and (coll? writable) (every? string? writable))))
      ":writable must be a set of connection names")))

(defn read-config
  "Read `config-file`: nil if there is no such file, its map otherwise, with
  `:writable` as a set. Throw `ex-info` if the file is malformed."
  []
  (let [^File file (io/file config-file)]
    (when (.exists file)
      (let [config (try
                     (or (edn/read-string (slurp file)) {})
                     (catch Exception e
                       (throw (ex-info (format "Malformed %s: %s"
                                               file (.getMessage e))
                                       {}))))]
        (when-let [problem (config-problem config)]
          (throw (ex-info (format "Malformed %s: %s" file problem) {})))
        (update config :writable set)))))

;;; Tools

(def default-max-rows 200)
(def max-rows-limit 1000)
(def default-timeout 30)
(def timeout-limit 50)
(def metadata-timeout
  "Seconds to wait for database metadata."
  30)

(def name-re
  "Owner and table names accepted from an agent. `structure.clj` puts them
  into SQL as is, so no quotes, spaces, dots, `;`, `\\` or comment markers."
  #"^[\p{L}\p{N}_$#@]{1,128}$")

(def ^:private connection-property
  {:type "string"
   :description "Connection name, as list_connections shows it."})

(def tools
  "The tools, in the order `tools/list` returns them."
  [{:name "list_connections"
    :title "List database connections"
    :description (str "List the database connections the user connected to "
                      "in Emacs (ejc-sql): name, database type, host, port, "
                      "database and whether execute may write to it. "
                      "Call it first. If the database you need is missing, "
                      "ask the user to connect to it with M-x ejc-connect "
                      "in Emacs.")
    :inputSchema {:type "object"
                  :properties {}
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "list_tables"
    :title "List tables"
    :description (str "List the tables of a connection, of one owner (schema) "
                      "if given, optionally filtered by a case-insensitive "
                      "substring of the name.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :owner {:type "string"
                                       :description "Owner or schema name."}
                               :pattern {:type "string"
                                         :description (str "Return only names "
                                                           "containing this "
                                                           "substring, "
                                                           "ignoring case.")}}
                  :required ["connection"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "describe_table"
    :title "Describe table"
    :description "Show the columns of a table with their types, and its constraints."
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :table {:type "string"
                                       :description "Table name, without the owner."}
                               :owner {:type "string"
                                       :description "Owner or schema name."}}
                  :required ["connection" "table"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "query"
    :title "Run read-only SQL"
    :description (str "Run one read-only SQL statement (SELECT, WITH, VALUES, "
                      "SHOW, EXPLAIN, DESCRIBE) and return the result as a "
                      "text table. It runs in a read-only transaction, which "
                      "is always rolled back. One statement per call, with "
                      "no ';' inside it, not even in a string literal or a "
                      "comment; other statements are rejected, use execute "
                      "to change data. On SQL Server and Sybase, which need "
                      "no ';' between statements, SQL holding a word of "
                      "another statement (INSERT, DROP, EXEC, SET, INTO, "
                      "COMMIT...) outside string literals, quoted names and "
                      "comments is rejected too. Rows are limited by "
                      "max_rows, the run time by timeout_s.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :sql {:type "string"
                                     :minLength 1
                                     :description "One SQL statement."}
                               :max_rows {:type "integer"
                                          :minimum 1
                                          :description (format
                                                        (str "Maximum number of "
                                                             "rows to return, "
                                                             "%d by default, "
                                                             "at most %d.")
                                                        default-max-rows
                                                        max-rows-limit)}
                               :timeout_s {:type "integer"
                                           :minimum 1
                                           :description (format
                                                         (str "Seconds before "
                                                              "the statement is "
                                                              "cancelled, %d by "
                                                              "default, at most "
                                                              "%d.")
                                                         default-timeout
                                                         timeout-limit)}}
                  :required ["connection" "sql"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "execute"
    :title "Run SQL that changes data"
    :description (str "Run one SQL statement that changes data or schema "
                      "(INSERT, UPDATE, DELETE, CREATE, ALTER, DROP, CALL...) "
                      "and commit it. The user is asked to approve every "
                      "call. It works only on the connections the user made "
                      "writable; on any other it fails, then ask the user "
                      "instead of retrying. One statement per call. To send "
                      "a statement containing ';', e.g. a procedure body, "
                      "start the SQL with a line \"delimiter X\", where X is "
                      "a separator that doesn't occur in the statement, not "
                      "even in a string literal or a comment.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :sql {:type "string"
                                     :minLength 1
                                     :description "One SQL statement."}
                               :timeout_s {:type "integer"
                                           :minimum 1
                                           :description (format
                                                         (str "Seconds before "
                                                              "the statement is "
                                                              "cancelled, %d by "
                                                              "default, at most "
                                                              "%d.")
                                                         default-timeout
                                                         timeout-limit)}}
                  :required ["connection" "sql"]
                  :additionalProperties false}
    :annotations {:readOnlyHint false
                  :destructiveHint true
                  :openWorldHint false}
    :_meta {"anthropic/requiresUserInteraction" true}}])

(defn- tool-by-name [name]
  (first (filter #(= name (:name %)) tools)))

(defn- text-result
  "A `tools/call` result of `text`."
  [text & [error?]]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- error-result [text]
  (text-result text true))

(defn mask-secrets
  "Replace the password of the db-spec `db`, and whatever looks like a
  password in a JDBC URL, by *** in `text`."
  [db text]
  (let [password (:password db)
        text (str text)
        text (if (and (string? password) (not (s/blank? password)))
               (s/replace text password "***")
               text)]
    (-> text
        (s/replace #"(?i)(password\s*=\s*)[^;&\s]*" "$1***")
        (s/replace #"(//[^:/@\s]+:)[^@/\s]*@" "$1***@"))))

(defn- exception-message [^Throwable e]
  (or (.getMessage e) (str (class e))))

;;; Arguments

(defn- integral?
  "Check if the JSON number `x` is an integer. JSON has one number type, so
  a client may send 5 as `5.0`."
  [x]
  (or (integer? x)
      (and (double? x)
           (not (Double/isInfinite x))
           (== x (Math/floor x)))))

(defn- valid-argument?
  "Check `value` against its property schema, as far as the tools use JSON
  Schema: the type, `minLength`, `minimum`."
  [{:keys [type minLength minimum]} value]
  (case type
    "string" (and (string? value)
                  (>= (count value) (or minLength 0)))
    "integer" (and (integral? value)
                   (>= value (or minimum Long/MIN_VALUE)))))

(defn- expected-argument
  "Describe the values a property schema accepts, for an error message."
  [{:keys [type minLength minimum]}]
  (case type
    "string" (if minLength "a non-empty string" "a string")
    "integer" (if minimum
                (format "an integer >= %d" minimum)
                "an integer")))

(defn argument-error
  "Describe what's wrong with the tool `args` (string keys) according to
  the tool's input `schema`, nil if they match it."
  [schema args]
  (let [properties (:properties schema)]
    (or (some (fn [k]
                (when-not (contains? args k)
                  (format "missing required argument %s." k)))
              (:required schema))
        (some (fn [[k v]]
                (let [property (get properties (keyword k))]
                  (cond
                    (nil? property)
                    (format "unknown argument %s." k)
                    (not (valid-argument? property v))
                    (format "argument %s must be %s."
                            k (expected-argument property)))))
              args))))

(defn- name-error
  "The error for an owner or table name in `args` not matching `name-re`,
  nil if they match it."
  [args]
  (some (fn [k]
          (when-let [v (get args k)]
            (when-not (re-matches name-re v)
              (format (str "Error: argument %s is not a valid name: only "
                           "letters, digits, _, $, # and @ are accepted.")
                      k))))
        ["owner" "table"]))

(defn- limit
  "`value` or `default` if it's nil, at most `max`. Return [value clamped?]."
  [value default max]
  (if (and value (> value max))
    [max true]
    [(long (or value default)) false]))

;;; SQL

(defn get-connection
  "Open a new JDBC connection to `db`."
  ^Connection [db]
  (j/get-connection db))

(defn- db-type
  "The `structure/get-db-type` of `db`, nil if it can't tell."
  [db]
  (try
    (structure/get-db-type db)
    (catch Exception _ nil)))

(def read-only-switches
  "Statements making a connection read-only, by `structure/get-db-type`.
  A `:session` one is run in autocommit mode and holds for the transactions
  to come, a `:transaction` one has to be the first statement of a
  transaction. Other databases have none, `.setReadOnly` is only a hint."
  {:postgresql [:session "SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY"]
   :mysql [:session "SET SESSION TRANSACTION READ ONLY"]
   :mariadb [:session "SET SESSION TRANSACTION READ ONLY"]
   :sqlite [:session "PRAGMA query_only = ON"]
   :oracle [:transaction "SET TRANSACTION READ ONLY"]})

(defn- run-switch
  "Run the read-only switch `sql` on `conn`. Throw if it fails, since the
  connection is not read-only then."
  [^Connection conn sql]
  (try
    (with-open [statement (.createStatement conn)]
      (.execute statement sql))
    (catch SQLException e
      (throw (ex-info (str "can't make the connection read-only: "
                           (.getMessage e))
                      {} e)))))

(defn with-read-only-connection
  "Call `f` with a new connection to `db`, read-only as far as the driver and
  the database allow, then roll back whatever `f` did. The connection is
  read-only in layers, none of them is trusted alone: `.setReadOnly`, the
  database's `read-only-switches`, the rollback."
  [db f]
  (with-open [conn (get-connection db)]
    (let [[mode sql] (read-only-switches (db-type db))]
      (when (= mode :session)
        (run-switch conn sql))
      (.setAutoCommit conn false)
      (try
        (.setReadOnly conn true)
        ;; sqlite-jdbc refuses to change it on an open connection.
        (catch SQLException _))
      (try
        (when (= mode :transaction)
          (run-switch conn sql))
        (f conn)
        (finally
          (.rollback conn))))))

(defn statements
  "Split `sql` into statements on every `separator`, the ones in string
  literals and comments too, dropping the parts that hold only comments.
  Unlike the quote-aware split of `eval-user-sql`, it can't be fooled by a
  quote or a comment marker the database reads differently, e.g. `'--'`, so
  no second statement slips through as a part of the first one."
  [sql separator]
  (->> (s/split sql (re-pattern (Pattern/quote separator)))
       (remove #(s/blank? (s/replace % lib/comments-re "")))
       (map s/trim)))

(defn- split-delimiter
  "Return [sql separator]: a leading `delimiter X` command removed and its X,
  or `sql` and `;`."
  [sql]
  (let [sql (s/trim sql)]
    (if (s/starts-with? (s/lower-case sql) "delimiter")
      [(s/trim (s/replace-first sql c/delimiter-re ""))
       (some-> (re-find c/delimiter-re sql) second s/trim not-empty)]
      [sql ";"])))

(defn- single-statement
  "Return [statement nil] for `parts` holding one statement, [nil error] else.
  `parts` are split on `separator`, `hint` tells how to send a statement
  holding it."
  [parts separator hint]
  (case (count parts)
    0 [nil "Error: there is no SQL statement."]
    1 [(first parts) nil]
    [nil (format (str "Error: one statement per call, this SQL has %d parts "
                      "separated by '%s'; one in a string literal or a "
                      "comment separates too. Send the statements one by "
                      "one. %s")
                 (count parts) separator hint)]))

(def tsql-write-words
  "Words of the T-SQL statements a read-only query must not hold: the ones
  that write, run code (`OPENQUERY` and the like too, the text they send to
  another server is not seen), control transactions or change the session.
  SQL Server needs no `;` between statements, so `SELECT 1 DROP TABLE t
  COMMIT` is three of them, and the first word tells nothing about the other
  two. Most of these words are reserved: a SELECT holds them only quoted or
  in comments, which `tsql-words` skips."
  #{"ALTER" "BACKUP" "BEGIN" "BULK" "CHECKPOINT" "COMMIT" "CONVERSATION"
    "CREATE" "DBCC" "DECLARE" "DELETE" "DENY" "DROP" "DUMP" "EXEC" "EXECUTE"
    "GRANT" "INSERT" "INTO" "KILL" "LOAD" "MERGE" "OPENDATASOURCE"
    "OPENQUERY" "OPENROWSET" "RAISERROR" "RECEIVE" "RECONFIGURE" "RESTORE"
    "REVERT" "REVOKE" "ROLLBACK" "SAVE" "SET" "SETUSER" "SHUTDOWN" "TRIGGER"
    "TRUNCATE" "UPDATE" "UPDATETEXT" "USE" "WAITFOR" "WRITETEXT"})

(defn- closing-quote
  "The index after the `close` char ending the quoted text which starts at
  `i` of `sql`, a doubled `close` being a part of the text. nil if the text
  is not closed."
  [^String sql i close]
  (loop [j (inc i)]
    (let [k (.indexOf sql (int close) (int j))]
      (cond
        (neg? k) nil
        (= close (get sql (inc k))) (recur (+ k 2))
        :else (inc k)))))

(defn- comment-end
  "The index after the block comment which starts at `i` of `sql`, the nested
  ones T-SQL allows counted. nil if the comment is not closed."
  [^String sql i]
  (loop [j (+ i 2)
         depth 1]
    (cond
      (zero? depth) j
      (>= j (count sql)) nil
      (.startsWith sql "/*" (int j)) (recur (+ j 2) (inc depth))
      (.startsWith sql "*/" (int j)) (recur (+ j 2) (dec depth))
      :else (recur (inc j) depth))))

(defn- word-char?
  "Check if `c` is a part of a T-SQL word: besides letters, digits and `_`,
  `@` and `#` start variables and temporary tables, `$` occurs in names."
  [c]
  (or (Character/isLetterOrDigit (char c)) (contains? #{\_ \@ \# \$} c)))

(defn tsql-words
  "The words of the T-SQL `sql` upper-cased, string literals, quoted names
  and comments skipped. nil if one of these is not closed. A variable keeps
  its `@`, a temporary table its `#`: `@SET` is not `SET`."
  [^String sql]
  (let [n (long (count sql))]
    (loop [i 0
           words []]
      (if (>= i n)
        words
        (let [c (.charAt sql i)]
          (cond
            (#{\' \" \[} c)
            (when-let [j (closing-quote sql i (if (= c \[) \] c))]
              (recur (long j) words))
            (.startsWith sql "--" i)
            (let [j (.indexOf sql "\n" i)]
              (recur (if (neg? j) n (long j)) words))
            (.startsWith sql "/*" i)
            (when-let [j (comment-end sql i)]
              (recur (long j) words))
            (word-char? c)
            (let [j (loop [j (inc i)]
                      (if (and (< j n) (word-char? (.charAt sql j)))
                        (recur (inc j))
                        j))]
              (recur (long j) (conj words (s/upper-case (subs sql i j)))))
            :else
            (recur (inc i) words)))))))

(defn- tsql?
  "Check if `db` speaks T-SQL: SQL Server or Sybase."
  [db]
  (boolean (some->> (db-type db) name (re-find #"sqlserver|sybase"))))

(defn tsql-error
  "The error for the read-only statement `sql` of T-SQL if it may do more:
  it holds a word of `tsql-write-words`, or `NEXT VALUE FOR`, which moves a
  sequence on, or a quote or a comment which is not closed. nil if it can't."
  [sql]
  (if-let [words (tsql-words sql)]
    (when-let [word (or (some tsql-write-words words)
                        (when (some #{["NEXT" "VALUE"]} (partition 2 1 words))
                          "NEXT VALUE FOR"))]
      (format (str "Error: SQL Server runs statements with no ';' between "
                   "them, so query rejects a word of another statement "
                   "anywhere, and this SQL holds %s outside string "
                   "literals, quoted names and comments. Use execute to "
                   "change data.")
              word))
    (str "Error: this SQL has a string literal, a quoted name or a comment "
         "which is not closed.")))

(defn- log-sql
  "Write `sql` to the query log, marked as the agent's."
  [connection-name sql]
  (o/log-sql (str "-- [mcp " (s/replace connection-name #"[\r\n]+" " ") "]\n"
                  sql "\n")))

(defn- elapsed-ms [start]
  (quot (- (System/nanoTime) start) 1000000))

(defn render-rows
  "Print `rows`, `[[column...] row...]`, as `o/print-table` does with the outside
  borders, with a footer: rows, `elapsed` ms and `notes`. Up to `max-rows`
  rows are printed, one more tells the result was truncated."
  [rows max-rows elapsed notes]
  (let [[header & data] rows
        truncated? (> (count data) max-rows)
        data (take max-rows data)
        table (with-out-str
                (binding [o/*add-outside-borders* true]
                  (o/print-table (cons header data) 0)))]
    (str table
         "\n"
         (format "%d row%s, %d ms" (count data) (if (= 1 (count data)) "" "s")
                 elapsed)
         (when truncated?
           (format ", truncated at max_rows %d" max-rows))
         (apply str (map #(str "; " %) notes)))))

(defn- timed-out?
  "Check if `e` means the statement was cancelled by its `timeout`, in s.
  Not every driver tells it, hence the `elapsed` ms too."
  [^SQLException e timeout elapsed]
  (or (instance? SQLTimeoutException e)
      ;; query_canceled: PostgreSQL, H2.
      (= "57014" (.getSQLState e))
      (>= elapsed (* 1000 timeout))))

(defn- sql-error
  "The tool error for `e`, thrown by a statement on `db`, secrets masked."
  [db ^SQLException e timeout elapsed]
  (error-result
   (mask-secrets
    db
    (if (timed-out? e timeout elapsed)
      (format "Error: cancelled by the timeout of %d s (timeout_s): %s"
              timeout (.getMessage e))
      (str "Error: " (.getMessage e))))))

(defn- notes
  "The footer notes telling the agent its limits were clamped."
  [max-rows-clamped? timeout-clamped?]
  (cond-> []
    max-rows-clamped? (conj (format "max_rows clamped to %d" max-rows-limit))
    timeout-clamped? (conj (format "timeout_s clamped to %d" timeout-limit))))

(defn- query-tool
  "Run the `query` tool: one read-only statement on a read-only connection
  of its own to `db`, rolled back."
  [db connection-name {:strs [sql max_rows timeout_s]}]
  (let [[sql error] (single-statement
                     (statements sql ";") ";"
                     (str "query takes no ';' inside a statement: leave "
                          "out the comments holding it, build a string "
                          "holding it with a function, e.g. CHR(59)."))
        error (or error
                  (when-not (lib/read-only? sql)
                    (format (str "Error: query runs only read-only "
                                 "statements (%s), not %s. Use execute to "
                                 "change data.")
                            (s/join ", " lib/read-only-words)
                            (or (lib/first-keyword sql) "this one")))
                  (when (tsql? db)
                    (tsql-error sql)))]
    (if error
      (error-result error)
      (let [[max-rows max-rows-clamped?] (limit max_rows default-max-rows
                                                max-rows-limit)
            [timeout timeout-clamped?] (limit timeout_s default-timeout
                                              timeout-limit)
            _ (log-sql connection-name sql)
            start (System/nanoTime)]
        (try
          (binding [lib/*max-column-width* @o/column-width-limit]
            (let [rows (with-read-only-connection
                         db
                         (fn [conn]
                           (c/fetch-rows db conn sql
                                         {:fetch-size (or @o/fetch-size 0)
                                          :max-rows (inc max-rows)
                                          :timeout timeout})))]
              (text-result
               (render-rows rows max-rows (elapsed-ms start)
                            (notes max-rows-clamped? timeout-clamped?)))))
          (catch SQLException e
            (sql-error db e timeout (elapsed-ms start))))))))

(defn- execute-tool
  "Run the `execute` tool: one statement on a connection of its own to `db`,
  in autocommit mode, if `config` makes the connection writable."
  [db connection-name config {:strs [sql timeout_s]}]
  (let [[sql separator] (split-delimiter sql)
        [sql error] (if separator
                      (single-statement
                       (statements sql separator) separator
                       (str "To send a statement holding it, start the SQL "
                            "with a line \"delimiter X\", where X doesn't "
                            "occur in the statement."))
                      [nil "Error: delimiter without a separator."])]
    (cond
      (not (contains? (:writable config) connection-name))
      (error-result (format "Error: writes to %s are not allowed; ask the user."
                            connection-name))
      error
      (error-result error)
      :else
      (let [[timeout timeout-clamped?] (limit timeout_s default-timeout
                                              timeout-limit)
            _ (log-sql connection-name sql)
            start (System/nanoTime)]
        (try
          (with-open [conn (get-connection db)
                      statement (.createStatement conn)]
            (.setAutoCommit conn true)
            (.setQueryTimeout statement timeout)
            (.setMaxRows statement (inc default-max-rows))
            (let [result-set? (.execute statement sql)
                  text (if result-set?
                         (binding [lib/*max-column-width* @o/column-width-limit]
                           (render-rows
                            (c/read-lobs
                             (j/result-set-seq (.getResultSet statement)
                                               {:as-arrays? true}))
                            default-max-rows
                            (elapsed-ms start)
                            (notes false timeout-clamped?)))
                         (str (c/execution-message statement false)
                              (when timeout-clamped?
                                (format "\n(timeout_s clamped to %d)"
                                        timeout-limit))))]
              (when (lib/ddl? sql)
                (cache/invalidate-cache db))
              (text-result text)))
          (catch SQLException e
            (sql-error db e timeout (elapsed-ms start))))))))

(defn- describe-table-tool
  "Run the `describe_table` tool: what `describe-table` shows in Emacs, over
  a read-only connection of its own to `db`, with a timeout on every query."
  [db {:strs [table owner]}]
  (let [success (atom true)
        ;; java.jdbc passes the keys of a db-spec map on to
        ;; `prepare-statement`, `:timeout` too.
        meta-db (fn [conn] {:connection conn :timeout metadata-timeout})
        text (binding [lib/*max-column-width* @o/column-width-limit
                       o/*add-outside-borders* true]
               (with-out-str
                 (with-read-only-connection
                   db
                   (fn [conn]
                     (structure/print-table-description
                      db table
                      :owner owner
                      :table-meta-fn (fn [_ table-name]
                                       (let [result (c/query-meta
                                                     (meta-db conn)
                                                     (str "SELECT * FROM "
                                                          table-name))]
                                         (reset! success (:success result))
                                         result))
                      :query-fn (fn [_ sql]
                                  (try
                                    (c/fetch-rows (meta-db conn) conn sql
                                                  {:timeout metadata-timeout})
                                    (catch SQLException e
                                      (str "Error: " (.getMessage e)))))
                      :row-limit 0)))))]
    (text-result (mask-secrets db text) (not @success))))

(defn- cache-error
  "The exception the table list query of `db` failed with, if it did."
  [db owner]
  (let [p (get-in @cache/cache-creation-promises
                  [db (if owner (keyword (str "tables-" owner)) :all-tables)])]
    (when (and p (realized? p) (instance? Throwable @p))
      @p)))

(defn- list-tables-tool
  "Run the `list_tables` tool on the `structure` cache of `db`, the one
  completion uses, waiting at most `metadata-timeout` for it to load."
  [db connection-name {:strs [owner pattern]}]
  (let [tables (deref (future
                        (if owner
                          (structure/get-tables db owner true)
                          (structure/get-all-tables db)))
                      (* 1000 metadata-timeout)
                      ::timeout)]
    (cond
      (= tables ::timeout)
      (error-result
       (format (str "Error: the table list of %s is still loading after %d s. "
                    "It keeps loading in the background, call list_tables "
                    "again in a while.")
               connection-name metadata-timeout))
      (and (empty? tables) (cache-error db owner))
      (error-result
       (mask-secrets db (str "Error: " (exception-message
                                        (cache-error db owner)))))
      :else
      (let [tables (cond->> (sort (distinct (map str tables)))
                     pattern (filter #(s/includes? (s/lower-case %)
                                                   (s/lower-case pattern))))]
        (text-result
         (if (seq tables)
           (str (count tables) (if (= 1 (count tables)) " table:\n" " tables:\n")
                (s/join "\n" tables))
           "No tables found."))))))

(defn- host-and-port
  "Host and port of `db`, from its keys or from the `//host:port` part of
  its URL."
  [db]
  (if (:host db)
    [(:host db) (:port db)]
    (when-let [[_ host port] (some
                              #(when (string? %)
                                 (re-find
                                  #"^(?:jdbc:[^/]*:)?//(?:[^@/]*@)?([^:/;?@]+)(?::(\d+))?"
                                  %))
                              [(:subname db) (:connection-uri db)])]
      [host port])))

(defn- connection-line
  "Describe the connection `db` for `list_connections`: enough to choose
  it, nothing about the user and the password."
  [connection-name db writable?]
  (let [safe (fn [f] (try (f) (catch Exception _ nil)))
        type (safe #(some-> (structure/get-db-type db) name))
        [host port] (safe #(host-and-port db))
        dbname (safe #(some-> (structure/get-db-name db)
                              (s/split #"[?;&]")
                              first
                              not-empty))]
    (str "- " connection-name ": "
         (s/join ", " (remove nil? [type
                                    (when host (str "host " host))
                                    (when port (str "port " port))
                                    (when dbname (str "database " dbname))
                                    (if writable?
                                      "writable (execute allowed)"
                                      "read-only")])))))

(defn- list-connections-tool
  "Run the `list_connections` tool. `config` tells the writable ones."
  [config]
  (let [registered (connections)]
    (text-result
     (if (seq registered)
       (str "Connections:\n"
            (s/join "\n"
                    (for [connection-name (connection-names)
                          :let [db (registered connection-name)]]
                      (mask-secrets db (connection-line
                                        connection-name db
                                        (contains? (:writable config)
                                                   connection-name))))))
       (str "No connections. Ask the user to connect to the database "
            "with M-x ejc-connect in Emacs.")))))

(defn- unknown-connection
  "The error for a connection not registered, telling the agent what to do."
  [connection-name]
  (let [names (connection-names)]
    (error-result
     (format (str "Error: unknown connection %s. Connected: %s. If the "
                  "database is missing, ask the user to connect to it with "
                  "M-x ejc-connect in Emacs.")
             connection-name
             (if (seq names) (s/join ", " names) "none")))))

(defn call-tool
  "Run the tool `tool-name` with `args` (string keys). `config` is the
  parsed `config-file`. Return the `tools/call` result."
  [tool-name args config]
  (let [tool (tool-by-name tool-name)]
    (if-let [problem (argument-error (:inputSchema tool) args)]
      (error-result (str "Error: " problem))
      (if (= tool-name "list_connections")
        (list-connections-tool config)
        (let [connection-name (get args "connection")
              db (get (connections) connection-name)]
          (cond
            (name-error args) (error-result (name-error args))
            (nil? db) (unknown-connection connection-name)
            :else
            (try
              (case tool-name
                "list_tables" (list-tables-tool db connection-name args)
                "describe_table" (describe-table-tool db args)
                "query" (query-tool db connection-name args)
                "execute" (execute-tool db connection-name config args))
              (catch Exception e
                (error-result
                 (mask-secrets db (str "Error: " (exception-message e))))))))))))

;;; Protocol

(def protocol-version
  "The revision served without the `initialize` handshake."
  "2026-07-28")

(def legacy-protocol-versions
  "The revisions served after the `initialize` handshake, newest first."
  ["2025-11-25" "2025-06-18" "2025-03-26"])

(def supported-protocol-versions
  "All the revisions served, as `server/discover` lists them."
  (into [protocol-version] legacy-protocol-versions))

(def tools-ttl-ms
  "How long a client may cache the tools: they never change within a JVM."
  3600000)

(def instructions
  "What the server tells a client about using it."
  (str "Databases the user connected to in Emacs (ejc-sql). Call "
       "list_connections first. query runs one read-only statement per call; "
       "execute runs one statement that changes data or schema, only on the "
       "connections the user made writable, and the user approves every "
       "call. If a database is missing, ask the user to connect to it with "
       "M-x ejc-connect in Emacs; never reach databases by other means."))

(defn server-info
  "Name and version of the server."
  []
  {:name "ejc-sql"
   :version (or (System/getProperty "ejc-sql.version") "unknown")})

(defn rpc-error
  "A JSON-RPC error response."
  [id code message & [data]]
  {:jsonrpc "2.0"
   :id id
   :error (cond-> {:code code :message message}
            data (assoc :data data))})

(defn- rpc-result [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- response [status body]
  {:status status :body body})

(defn- call-tool-request
  "Answer the `tools/call` request `id`. Unlike a tool error, which goes to
  the model, invalid params are a JSON-RPC error. `modern?` adds the
  `resultType` the `protocol-version` requires."
  [id params config modern?]
  (let [tool-name (get params "name")
        args (get params "arguments")]
    (cond
      (not (string? tool-name))
      (response 200 (rpc-error id -32602
                               "Invalid params: name must be a string."))
      (not (or (nil? args) (map? args)))
      (response 200 (rpc-error id -32602
                               "Invalid params: arguments must be an object."))
      (not (tool-by-name tool-name))
      (response 200 (rpc-error id -32602 (str "Unknown tool: " tool-name)))
      :else
      (response 200 (rpc-result id
                                (cond-> (call-tool tool-name (or args {}) config)
                                  modern? (assoc :resultType "complete")))))))

(defn- decode-header
  "Decode a `=?base64?...?=` header value, nil if it's not valid base64."
  [value]
  (if-let [[_ encoded] (re-matches #"=\?base64\?(.*)\?=" value)]
    (try
      (String. (.decode (Base64/getDecoder) ^String encoded) "UTF-8")
      (catch IllegalArgumentException _ nil))
    value))

(defn- header-mismatch
  "Describe how the `headers` of a modern request contradict its body."
  [headers method params]
  (let [check (fn [header expected]
                (let [raw (get headers (s/lower-case header))]
                  (cond
                    (nil? raw)
                    (format "%s header is missing" header)
                    (not= expected (decode-header raw))
                    (format "%s header value '%s' does not match body value '%s'"
                            header raw expected))))
        version (get headers "mcp-protocol-version")
        meta-version (get-in params ["_meta"
                                     "io.modelcontextprotocol/protocolVersion"])]
    (or (check "Mcp-Method" method)
        (when (and (= method "tools/call") (string? (get params "name")))
          (check "Mcp-Name" (get params "name")))
        (when (not= version meta-version)
          (format (str "MCP-Protocol-Version header value '%s' does not match "
                       "params._meta io.modelcontextprotocol/protocolVersion '%s'")
                  version meta-version)))))

(defn- modern-request
  "Answer the request `id` by the rules of `protocol-version`: the headers
  must match the body, `server/discover` replaces `initialize`, there is no
  `ping`."
  [headers id method params config]
  (if-let [mismatch (header-mismatch headers method params)]
    (response 400 (rpc-error id -32020 (str "Header mismatch: " mismatch)))
    (case method
      "server/discover"
      (response 200 (rpc-result
                     id
                     {:resultType "complete"
                      :supportedVersions supported-protocol-versions
                      :capabilities {:tools {}}
                      :instructions instructions
                      :ttlMs tools-ttl-ms
                      :cacheScope "public"
                      :_meta {"io.modelcontextprotocol/serverInfo"
                              (server-info)}}))
      "tools/list"
      (response 200 (rpc-result id {:resultType "complete"
                                    :tools tools
                                    :ttlMs tools-ttl-ms
                                    :cacheScope "public"}))
      "tools/call"
      (call-tool-request id params config true)
      (response 404 (rpc-error id -32601 (str "Method not found: " method))))))

(defn- legacy-request
  "Answer the request `id` by the rules of `legacy-protocol-versions`, where
  a session starts with `initialize`."
  [id method params config]
  (case method
    "initialize"
    (let [requested (get params "protocolVersion")]
      (response 200 (rpc-result
                     id
                     {:protocolVersion (if (some #{requested}
                                                 legacy-protocol-versions)
                                         requested
                                         (first legacy-protocol-versions))
                      :capabilities {:tools {:listChanged false}}
                      :serverInfo (server-info)
                      :instructions instructions})))
    "ping"
    (response 200 (rpc-result id {}))
    "tools/list"
    (response 200 (rpc-result id {:tools tools}))
    "tools/call"
    (call-tool-request id params config false)
    (response 200 (rpc-error id -32601 (str "Method not found: " method)))))

(defn- message-kind
  "`:request`, `:notification` or `:response` (a client's one), nil if
  `message` is not valid JSON-RPC 2.0."
  [message]
  (when (and (map? message) (= "2.0" (get message "jsonrpc")))
    (let [id (get message "id")
          method (get message "method")
          id? (contains? message "id")]
      (cond
        (and (string? method) (not id?))
        :notification
        (and (string? method) (or (string? id) (number? id)))
        :request
        (and (nil? method) id? (or (nil? id) (string? id) (number? id))
             (or (contains? message "result") (contains? message "error")))
        :response))))

(defn handle-rpc
  "Handle the JSON-RPC `message`, parsed from JSON with string keys, which
  came with the HTTP `headers` (lower-case names). `config` is the parsed
  `config-file`. Return {:status http-status, :body response-or-nil}.

  The revision is chosen per request: `initialize` and a request with a
  legacy `MCP-Protocol-Version`, or with none, are served by the legacy
  rules, a request with `protocol-version` by the modern ones."
  [headers message config]
  (let [version (get headers "mcp-protocol-version")
        kind (message-kind message)
        id (when (map? message) (get message "id"))
        method (get message "method")
        params (or (get message "params") {})]
    (cond
      (nil? kind)
      (response 400 (rpc-error (when (or (string? id) (number? id)) id)
                               -32600 "Invalid Request"))
      (and version (not (some #{version} supported-protocol-versions)))
      (response 400 (rpc-error (when (= kind :request) id)
                               -32022 "Unsupported protocol version"
                               {:supported supported-protocol-versions
                                :requested version}))
      ;; Nothing to answer: a notification or a client's response.
      (not= kind :request)
      (response 202 nil)
      (not (map? params))
      (response 400 (rpc-error id -32602 "Invalid params: must be an object."))
      (and (= version protocol-version) (not= method "initialize"))
      (modern-request headers id method params config)
      :else
      (legacy-request id method params config))))
