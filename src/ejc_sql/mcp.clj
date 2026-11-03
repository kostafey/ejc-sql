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

;;; The Model Context Protocol (MCP) server core: the connections, the ones
;;; of `databases.edn' and the ones the user connected to in Emacs, the
;;; configuration file, the tools and the JSON-RPC dispatcher. The transport
;;; is `ejc-sql.mcp-http'.
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
            [ejc-sql.deps-resolver :as deps]
            [ejc-sql.lib :as lib]
            [ejc-sql.output :as o]
            [ejc-sql.structure :as structure])
  (:import [clojure.lang Compiler RT]
           [java.io File]
           [java.sql Connection DatabaseMetaData SQLException
            SQLTimeoutException]
           [java.util Base64]
           [java.util.concurrent ExecutionException Executor]
           [java.util.regex Pattern]))

;;; Registry

(defonce ^{:doc "The connections the user connected to in Emacs:
  {name db-spec}."}
  registry
  (atom {}))

(defn register-db!
  "Make the db-spec `db` available to the agents under the Emacs connection
  `name`. A spec registered under this name before is replaced."
  [name db]
  (swap! registry assoc name db))

;;; Configuration

(def config-file
  "The MCP server configuration. The server runs only while the file exists.
  Emacs sets it with `set-config-file!` to `ejc-mcp-config-file`, since its
  `~` can differ from `user.home`: on Windows it may be `%AppData%`, while
  `user.home` is always `%UserProfile%`."
  (io/file (System/getProperty "user.home") ".ejc-sql" "mcp.edn"))

(defn set-config-file!
  "Read the MCP server configuration from `path` from now on."
  [path]
  (alter-var-root #'config-file (constantly (io/file path))))

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

;;; Databases file

(defn databases-file
  "The connections the agents may use without the user connecting to them
  in Emacs, {name db-spec}, written by the user: `databases.edn` next to
  `config-file`, so that Emacs sets its directory too."
  ^File []
  (io/file (.getParentFile (.getAbsoluteFile (io/file config-file)))
           "databases.edn"))

(defn- databases-problem
  "Describe what's wrong with the parsed `databases`, nil if nothing."
  [databases]
  (if (map? databases)
    (some (fn [[name db]]
            (cond
              (not (and (string? name) (not (s/blank? name))))
              (str (pr-str name) ": a connection name must be a non-blank string")
              (not (map? db))
              (str name ": not a map")
              (not (some db [:dbtype :subprotocol :connection-uri]))
              (str name ": needs :dbtype, :subprotocol or :connection-uri")
              (not (or (nil? (:description db)) (string? (:description db))))
              (str name ": :description must be a string")))
          databases)
    "not a map"))

(defn read-databases
  "Read `databases-file`: {} if there is no such file, its map otherwise.
  Throw `ex-info` if the file is malformed."
  []
  (let [file (databases-file)]
    (if (.exists file)
      (let [databases (try
                        (or (edn/read-string (slurp file)) {})
                        (catch Exception e
                          (throw (ex-info (format "Malformed %s: %s"
                                                  file (.getMessage e))
                                          {}))))]
        (when-let [problem (databases-problem databases)]
          (throw (ex-info (format "Malformed %s: %s" file problem) {})))
        databases)
      {})))

;;; Drivers

(def driver-wait-ms
  "How long a tool waits for the JDBC driver of a connection from
  `databases-file`, which may be downloaded, before it gives up: with the
  longest `timeout-limit` of a statement, within the 60 s a client waits."
  10000)

(defonce ^{:doc "The class loader the JDBC drivers of `databases-file` are
  added to, see `remember-class-loader!`."
           :private true}
  driver-class-loader
  (atom nil))

(defn remember-class-loader!
  "Load the JDBC drivers of `databases-file` into the class loader of the
  calling thread from now on: the nREPL session of Emacs, or the main
  thread of the standalone server, which ejc-sql was loaded by. A driver is
  of use only in a class loader `clojure.java.jdbc` was loaded below, see
  `deps/load-driver!`, and the threads the drivers load in have none of
  their own. Only the first call counts."
  []
  (compare-and-set! driver-class-loader nil (RT/baseLoader)))

(defonce ^{:doc "The JDBC drivers of `databases-file`, loaded or loading:
  {db-spec (delay (future nil))}."
           :private true}
  drivers
  (atom {}))

(defn- driver
  "Start loading the JDBC driver of the db-spec `db`, unless it's loaded or
  loading. Return {db-spec (delay (future nil))} with this one in it."
  [db]
  (let [loading (delay
                  (future
                    (with-bindings {Compiler/LOADER (or @driver-class-loader
                                                        (RT/baseLoader))}
                      (deps/load-driver! db))))
        loaded (swap! drivers #(if (contains? % db) % (assoc % db loading)))]
    (force (loaded db))
    loaded))

(defn await-driver
  "Wait for the JDBC driver of the db-spec `db` to load, `timeout-ms` at
  most, forever without it. Throw `ex-info` if it doesn't load in time or
  fails; a failed one is loaded anew by the next call."
  [db & [timeout-ms]]
  (let [loading (get (driver db) db)
        result (try
                 (if timeout-ms
                   (deref @loading timeout-ms ::timeout)
                   @@loading)
                 (catch ExecutionException e
                   (swap! drivers #(if (identical? (% db) loading)
                                     (dissoc % db)
                                     %))
                   (throw (ex-info (str "can't load the JDBC driver: "
                                        (.getCause e))
                                   {} (.getCause e)))))]
    (when (= result ::timeout)
      (throw (ex-info (format (str "the JDBC driver is still loading after "
                                   "%d s, its dependencies may be being "
                                   "downloaded. Try again later.")
                              (quot timeout-ms 1000))
                      {})))))

(defonce ^{:doc "The last `databases-file` read: {:stamp, :databases}."
           :private true}
  databases-cache
  (atom nil))

(defn file-connections
  "The connections of `databases-file`, read anew only when its path, size
  or modification time changes, their JDBC drivers loading. Throw `ex-info`
  if the file is malformed."
  []
  (let [file (databases-file)
        stamp [file (.lastModified file) (.length file)]
        cached @databases-cache]
    (if (= stamp (:stamp cached))
      (:databases cached)
      (let [databases (read-databases)]
        (run! driver (vals databases))
        (reset! databases-cache {:stamp stamp :databases databases})
        databases))))

(defn connections
  "The connections the agents may use, {name db-spec}: the ones of
  `databases-file` and the ones connected to in Emacs, which win on the
  same name. Throw `ex-info` if the file is malformed."
  []
  (merge (file-connections) @registry))

(defn connection-names
  "The names of the `connections`, sorted."
  []
  (sort (keys (connections))))

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

(def ^:private owner-property
  {:type "string"
   :description "Owner or schema name."})

(defn- max-column-width-property
  "The `max_column_width` argument, its `default` told."
  [default]
  {:type "integer"
   :minimum 0
   :description (str "Cut longer text values to this many characters, "
                     "followed by ...[+N], N the number of the characters "
                     "left out. " default " 0 for no limit. Numbers and "
                     "dates are never cut.")})

(def find-columns-limit
  "The most columns `find_columns` returns."
  200)

(def ^:private timeout-property
  {:type "integer"
   :minimum 1
   :description (format (str "Seconds before the statement is cancelled, %d "
                             "by default, at most %d.")
                        default-timeout
                        timeout-limit)})

(def ^:private expected-rows-property
  {:type "integer"
   :minimum 0
   :description (str "The number of rows the statement inserts, updates or "
                     "deletes, counted with query before. The statement "
                     "runs in a transaction then, rolled back if it changes "
                     "another number of rows.")})

(def tools
  "The tools, in the order `tools/list` returns them."
  [{:name "list_connections"
    :title "List database connections"
    :description (str "List the database connections the user made "
                      "available in ejc-sql: name, database type, host, "
                      "port, database, whether execute may write to it and "
                      "the user's description of it, if any, which tells "
                      "e.g. where a tunnel to 127.0.0.1 leads. Call it "
                      "first. If the database you need is missing, "
                      "ask the user to add it to databases.edn of ejc-sql "
                      "or to connect to it with M-x ejc-connect in Emacs.")
    :inputSchema {:type "object"
                  :properties {}
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "connection_info"
    :title "Show database version"
    :description (str "Show the user's description of the connection, the "
                      "database product and version, the JDBC driver, the "
                      "current catalog and schema, and how the database "
                      "keeps unquoted names: upper-cased, lower-cased or as "
                      "written.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property}
                  :required ["connection"]
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
                               :owner owner-property
                               :pattern {:type "string"
                                         :description (str "Return only names "
                                                           "containing this "
                                                           "substring, "
                                                           "ignoring case.")}}
                  :required ["connection"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "find_columns"
    :title "Find columns"
    :description (format (str "Find the columns, of tables and views, whose "
                              "names contain a substring, as given, "
                              "upper-cased or lower-cased, in one owner "
                              "(schema) or, if none is given, in the "
                              "current schema first, then in all of them, "
                              "of the tables whose names contain another "
                              "one, if given. At most %d columns are "
                              "returned.")
                         find-columns-limit)
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :column {:type "string"
                                        :description (str "Part of the "
                                                          "column name.")}
                               :table {:type "string"
                                       :description (str "Part of the table "
                                                         "or view name.")}
                               :owner owner-property}
                  :required ["connection" "column"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "describe_table"
    :title "Describe table"
    :description (str "Show the columns of a table: types with sizes, "
                      "NOT NULL, defaults, comments; its primary key, its "
                      "indexes, its foreign keys, the foreign keys "
                      "referencing it, and the constraints the database "
                      "lists.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :table {:type "string"
                                       :description "Table name, without the owner."}
                               :owner owner-property
                               :max_column_width (max-column-width-property
                                                  (str "30 by default, as "
                                                       "set in Emacs."))}
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
                      "max_rows, the run time by timeout_s, the values "
                      "shown by max_column_width.")
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
                               :timeout_s timeout-property
                               :max_column_width (max-column-width-property
                                                  (str "By default 30, as set "
                                                       "in Emacs, but for a "
                                                       "single row, which is "
                                                       "not cut. A single "
                                                       "value is never cut."))}
                  :required ["connection" "sql"]
                  :additionalProperties false}
    :annotations {:readOnlyHint true
                  :openWorldHint false}}
   {:name "execute"
    :title "Run SQL that changes data"
    :description (str "Run SQL that changes data or schema (INSERT, UPDATE, "
                      "DELETE, CREATE, ALTER, DROP, CALL...) and commit it. "
                      "The user is asked to approve every call. It works "
                      "only on the connections the user made writable; on "
                      "any other it fails, then ask the user instead of "
                      "retrying. Send one statement as sql, or several as "
                      "statements: they run in this order in one "
                      "transaction, committed if all of them succeed, "
                      "rolled back at the first failure. MySQL, MariaDB, "
                      "Oracle and H2 commit the transaction a DDL statement "
                      "(CREATE, ALTER, DROP, TRUNCATE, GRANT...) runs in, "
                      "so there statements holding one are rejected unless "
                      "transaction is false; a statement starting or ending "
                      "a transaction (BEGIN, COMMIT...) is rejected in "
                      "statements everywhere. Before an UPDATE or a DELETE, "
                      "count the rows it changes with query and pass the "
                      "count as expected_rows: the user sees it when "
                      "approving the call, and the statement is rolled back "
                      "if it changes another number of rows. To send a "
                      "statement containing ';', e.g. a procedure body, "
                      "start its SQL with a line \"delimiter X\", where X is "
                      "a separator that doesn't occur in the statement, not "
                      "even in a string literal or a comment.")
    :inputSchema {:type "object"
                  :properties {:connection connection-property
                               :sql {:type "string"
                                     :minLength 1
                                     :description "One SQL statement."}
                               :expected_rows expected-rows-property
                               :statements
                               {:type "array"
                                :minItems 1
                                :description (str "Several SQL statements, "
                                                  "instead of sql.")
                                :items {:type "object"
                                        :properties
                                        {:sql {:type "string"
                                               :minLength 1
                                               :description "One SQL statement."}
                                         :expected_rows expected-rows-property}
                                        :required ["sql"]
                                        :additionalProperties false}}
                               :transaction
                               {:type "boolean"
                                :description (str "For statements: false runs "
                                                  "them one by one, each "
                                                  "committed at once, or "
                                                  "rolled back if it has "
                                                  "expected_rows and changes "
                                                  "another number of rows, "
                                                  "stopping at the first "
                                                  "failure. true by "
                                                  "default.")}
                               :timeout_s (assoc timeout-property
                                                 :description
                                                 (format
                                                  (str "Seconds before the "
                                                       "statements, all of "
                                                       "them together, are "
                                                       "cancelled, %d by "
                                                       "default, at most %d.")
                                                  default-timeout
                                                  timeout-limit))}
                  :required ["connection"]
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

(def ^:private oracle-credentials-re
  "Regex to find the credentials of an Oracle URL, `user/password@` after
  `thin:` or `oci:`, with that prefix as group 1. A quoted password may hold
  `@` and spaces."
  #"(?i)\b((?:thin|oci8?):)(?:\"[^\"]*\"|[^\s\"@])++@")

(defn mask-secrets
  "Replace the password of the db-spec `db`, and whatever looks like a
  password in a JDBC URL, by *** in `text`. The user of an Oracle URL,
  `user/password@`, goes too."
  [db text]
  (let [password (:password db)
        text (str text)
        text (if (and (string? password) (not (s/blank? password)))
               (s/replace text password "***")
               text)]
    (-> text
        (s/replace oracle-credentials-re "$1***@")
        (s/replace #"(?i)(password\s*=\s*)[^;&\s]*" "$1***")
        (s/replace #"(//[^:/@\s]+:)[^@/\s]*@" "$1***@"))))

(defn- mask-all-secrets
  "`mask-secrets` of every connection, for a `text` which may come from any
  of them. The ones of `databases-file` as last read, since it may be
  malformed now."
  [text]
  (reduce #(mask-secrets %2 %1)
          (mask-secrets {} text)
          (vals (merge (:databases @databases-cache) @registry))))

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
  Schema: the type, `minLength`, `minimum`, `minItems`. The items of an
  array and the properties of an object are left to `property-error`."
  [{:keys [type minLength minimum minItems]} value]
  (case type
    "string" (and (string? value)
                  (>= (count value) (or minLength 0)))
    "integer" (and (integral? value)
                   (>= value (or minimum Long/MIN_VALUE)))
    "boolean" (boolean? value)
    "array" (and (sequential? value)
                 (>= (count value) (or minItems 0)))
    "object" (map? value)))

(defn- expected-argument
  "Describe the values a property schema accepts, for an error message."
  [{:keys [type minLength minimum minItems]}]
  (case type
    "string" (if minLength "a non-empty string" "a string")
    "integer" (if minimum
                (format "an integer >= %d" minimum)
                "an integer")
    "boolean" "true or false"
    "array" (if minItems "a non-empty array" "an array")
    "object" "an object"))

(declare argument-error)

(defn- property-error
  "Describe what's wrong with the argument `value` named `path` according
  to its `property` schema, its items or properties too, nil if nothing."
  [{:keys [type items] :as property} value path]
  (if (valid-argument? property value)
    (case type
      "array" (some (fn [[i item]]
                      (property-error items item (format "%s[%d]" path i)))
                    (map-indexed vector value))
      "object" (argument-error property value path)
      nil)
    (format "argument %s must be %s." path (expected-argument property))))

(defn argument-error
  "Describe what's wrong with the tool `args` (string keys) according to
  the tool's input `schema`, nil if they match it. `path` names `args` in
  the message, when they are the properties of an argument. An unknown
  argument is told first, with the known ones: an agent may take the name
  of a similar one of another tool, the `pattern` of `list_tables` for the
  `column` of `find_columns`, which is missing then."
  [schema args & [path]]
  (let [properties (:properties schema)
        full-name #(if path (str path "." %) %)]
    (or (some (fn [[k v]]
                (if-let [property (get properties (keyword k))]
                  (property-error property v (full-name k))
                  (format "unknown argument %s, the arguments%s are: %s."
                          (full-name k)
                          (if path (str " of " path) "")
                          (s/join ", " (map name (keys properties))))))
              args)
        (some (fn [k]
                (when-not (contains? args k)
                  (format "missing required argument %s." (full-name k))))
              (:required schema)))))

(defn- name-error
  "The error for an owner, table or column name in `args` not matching
  `name-re`, nil if they match it."
  [args]
  (some (fn [k]
          (when-let [v (get args k)]
            (when-not (re-matches name-re v)
              (format (str "Error: argument %s is not a valid name: only "
                           "letters, digits, _, $, # and @ are accepted.")
                      k))))
        ["owner" "table" "column"]))

(defn- limit
  "`value` or `default` if it's nil, at most `max`. Return [value clamped?]."
  [value default max]
  (if (and value (> value max))
    [max true]
    [(long (or value default)) false]))

(defn- column-width
  "The `lib/*max-column-width*` for the `max_column_width` argument `value`:
  the limit set in Emacs for nil, none, nil, for 0."
  [value]
  (let [width (if (nil? value) @o/column-width-limit value)]
    (when (and width (pos? width))
      (long width))))

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
  `parts` are split on `separator`, `hint` tells how to send several
  statements or one holding it."
  [parts separator hint]
  (case (count parts)
    0 [nil "Error: there is no SQL statement."]
    1 [(first parts) nil]
    [nil (format (str "Error: one statement only, this SQL has %d parts "
                      "separated by '%s'; one in a string literal or a "
                      "comment separates too. %s")
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

(defn- word-end
  "The index after the T-SQL word which starts at `i` of `sql`."
  [^String sql i]
  (loop [j (inc i)]
    (if (and (< j (count sql)) (word-char? (.charAt sql j)))
      (recur (inc j))
      j)))

(defn tsql-first-word
  "The first word of the T-SQL `sql` upper-cased, whitespace, `(` and
  comments skipped, as the server reads it: with its `@`, `#` and `$`,
  letters of any alphabet included, so `SELECT$X` is a word of its own.
  nil if `sql` starts with anything else, a quoted name among them."
  [^String sql]
  (let [n (count sql)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt sql i)]
          (cond
            (#{\space \tab \newline \return \formfeed \(} c)
            (recur (inc i))
            (.startsWith sql "--" i)
            (let [j (.indexOf sql "\n" i)]
              (when-not (neg? j)
                (recur (long j))))
            (.startsWith sql "/*" i)
            (when-let [j (comment-end sql i)]
              (recur (long j)))
            (word-char? c)
            (s/upper-case (subs sql i (word-end sql i)))))))))

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
            (let [j (long (word-end sql i))]
              (recur j (conj words (s/upper-case (subs sql i j)))))
            :else
            (recur (inc i) words)))))))

(defn- tsql?
  "Check if `db` speaks T-SQL: SQL Server or Sybase."
  [db]
  (boolean (some->> (db-type db) name (re-find #"sqlserver|sybase"))))

(defn- hash-comments?
  "Check if `#` starts a line comment on `db`: MySQL and MariaDB. Elsewhere
  it may start a word, on SQL Server the name of a temporary procedure."
  [db]
  (contains? #{:mysql :mariadb} (db-type db)))

(def tsql-read-words
  "The first words of the read-only T-SQL statements. SQL Server and Sybase
  take any other word starting a batch, or a quoted name, for the name of a
  procedure to call, `EXEC` left out: `SHOW` or `[SELECT]` may run one."
  #{"SELECT" "WITH"})

(defn tsql-error
  "The error for the read-only statement `sql` of T-SQL if it may do more:
  its `tsql-first-word` is not one of `tsql-read-words`, it holds a word of
  `tsql-write-words`, or `NEXT VALUE FOR`, which moves a sequence on, or a
  quote or a comment which is not closed. nil if it can't."
  [sql]
  (let [first-word (tsql-first-word sql)
        words (tsql-words sql)]
    (cond
      (not (tsql-read-words first-word))
      (format (str "Error: SQL Server calls the procedure named by the first "
                   "word of a batch, EXEC left out, so query takes only the "
                   "statements starting with %s there, not %s.")
              (s/join " or " (sort tsql-read-words))
              (or first-word "this one"))
      (nil? words)
      (str "Error: this SQL has a string literal, a quoted name or a comment "
           "which is not closed.")
      :else
      (when-let [word (or (some tsql-write-words words)
                          (when (some #{["NEXT" "VALUE"]}
                                      (partition 2 1 words))
                            "NEXT VALUE FOR"))]
        (format (str "Error: SQL Server runs statements with no ';' between "
                     "them, so query rejects a word of another statement "
                     "anywhere, and this SQL holds %s outside string "
                     "literals, quoted names and comments. Use execute to "
                     "change data.")
                word)))))

(defn- log-sql
  "Write `sql` to the query log, marked as the agent's."
  [connection-name sql]
  (o/log-sql (str "-- [mcp " (s/replace connection-name #"[\r\n]+" " ") "]\n"
                  sql "\n")))

(defn- elapsed-ms [start]
  (quot (- (System/nanoTime) start) 1000000))

;; The first characters of a CLOB longer than the width of its column, read
;; by `read-result`, and its `length`: only that much of a CLOB is read.
(defrecord Cut [text length])

(defn- cut-clob
  "`clob` as a `Cut` of its first `width` characters if it's longer, as is
  otherwise."
  [^java.sql.Clob clob width]
  (let [length (.length clob)]
    (if (> length width)
      (->Cut (.getSubString clob 1 (int width)) length)
      clob)))

(defn- read-result
  "Read the result set `rows`, `[[column...] row...]`, while it's open, for
  `render-rows`, which prints up to `max-rows` rows of it: {:rows, :width}.
  The values are cut to the width `value`, the `max_column_width`
  argument, if given, else the limit set in Emacs, but for a single row,
  which `render-rows` prints as a line per column, with room for long
  values. A single value is never cut: it is what the agent asked for, a
  `max_column_width` given with it is more likely the one of the call
  before. A CLOB longer than the width becomes a `Cut`, the other LOBs
  text, as `c/read-lobs` makes them."
  [rows max-rows value]
  (let [single? (= 1 (min max-rows (count (rest rows))))
        width (when-not (and single? (= 1 (count (first rows))))
                (column-width (if (and (nil? value) single?) 0 value)))]
    {:width width
     :rows (mapv (fn [row]
                   (lib/lob-to-string-row
                    (mapv #(if (and width (lib/is-clob? %))
                             (cut-clob % width)
                             %)
                          row)
                    true))
                 rows)}))

(defn- cell
  "The value `value` for the agent and whether it is cut: [value cut?].
  nil and a number are left to `o/print-table`, which shows nil as NULL and
  aligns the numbers. The text of any other value is cut to its first
  `width` characters, followed by `o/cut-mark`, so that a cut value tells
  it itself and how much of it is left out; a `Cut` is a CLOB
  `read-result` has cut already. Its line breaks and tabs are written as
  `\\r`, `\\n` and `\\t`, so that nothing of them is lost and a row stays
  a line. A date is never cut: it's short, and of no use cut."
  [value width]
  (if (or (nil? value) (number? value))
    [value false]
    (let [text (if (instance? Cut value) (:text value) (str value))
          left (cond
                 (instance? Cut value)
                 (- (:length value) (count text))
                 (and width
                      (> (count text) width)
                      (not (instance? java.util.Date value))
                      (not (instance? java.time.temporal.Temporal value)))
                 (- (count text) width))
          text (s/escape (if (and left (not (instance? Cut value)))
                           (subs text 0 width)
                           text)
                         {\return "\\r" \newline "\\n" \tab "\\t"})]
      (if left
        [(str text (o/cut-mark left)) true]
        [text false]))))

(defn render-rows
  "Print `rows`, `[[column...] row...]`, of `read-result` for the agent,
  with a footer: rows, `elapsed` ms, `notes`. Up to `max-rows` rows are
  printed, one more tells the result was truncated. The values are
  `cell`s, cut to `width`; the footer tells it, followed by `cut-hint`.
  `o/print-table` draws them with no outside borders, and so a single row
  as a line per column, `column | value`: no counting of separators in a
  long line to find the column of a value."
  [rows max-rows width elapsed notes & [cut-hint]]
  (let [[header & data] rows
        truncated? (> (count data) max-rows)
        data (take max-rows data)
        names (map #(if (keyword? %) (name %) (str %)) header)
        cells (map (fn [row] (map #(cell % width) row)) data)
        table (with-out-str
                (binding [o/*add-outside-borders* false
                          o/*null-text* "NULL"
                          ;; Cut by `cell`, dates not.
                          lib/*max-column-width* nil]
                  (o/print-table (cons names (map #(map first %) cells))
                                 0)))]
    (str table
         "\n"
         (format "%d row%s, %d ms" (count data) (if (= 1 (count data)) "" "s")
                 elapsed)
         (when truncated?
           (format ", truncated at max_rows %d" max-rows))
         (apply str (map #(str "; " %)
                         (cond-> notes
                           (some second (apply concat cells))
                           (conj (str (format "values cut to %d characters"
                                              width)
                                      cut-hint))))))))

(defn- timed-out?
  "Check if `e` means the statement was cancelled by its `timeout`, in s.
  Not every driver tells it, hence the `elapsed` ms too."
  [^SQLException e timeout elapsed]
  (or (instance? SQLTimeoutException e)
      ;; query_canceled: PostgreSQL, H2.
      (= "57014" (.getSQLState e))
      (>= elapsed (* 1000 timeout))))

(defn- sql-error-text
  "The error text for `e`, thrown by a statement on `db`, secrets masked."
  [db ^SQLException e timeout elapsed]
  (mask-secrets
   db
   (if (timed-out? e timeout elapsed)
     (format "Error: cancelled by the timeout of %d s (timeout_s): %s"
             timeout (.getMessage e))
     (str "Error: " (.getMessage e)))))

(defn- sql-error
  "The tool error for `e`, thrown by a statement on `db`, secrets masked."
  [db ^SQLException e timeout elapsed]
  (error-result (sql-error-text db e timeout elapsed)))

(defn- notes
  "The footer notes telling the agent its limits were clamped."
  [max-rows-clamped? timeout-clamped?]
  (cond-> []
    max-rows-clamped? (conj (format "max_rows clamped to %d" max-rows-limit))
    timeout-clamped? (conj (format "timeout_s clamped to %d" timeout-limit))))

(defn- query-tool
  "Run the `query` tool: one read-only statement on a read-only connection
  of its own to `db`, rolled back."
  [db connection-name {:strs [sql max_rows timeout_s max_column_width]}]
  (let [[sql error] (single-statement
                     (statements sql ";") ";"
                     (str "Send the statements one by one. query takes no "
                          "';' inside a statement: leave out the comments "
                          "holding it, build a string holding it with a "
                          "function, e.g. CHR(59)."))
        error (or error
                  (when-not (lib/read-only? sql (hash-comments? db))
                    (format (str "Error: query runs only read-only "
                                 "statements (%s), not %s. Use execute to "
                                 "change data.")
                            (s/join ", " lib/read-only-words)
                            (or (lib/first-keyword sql (hash-comments? db))
                                "this one")))
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
          (let [{:keys [rows width]}
                (with-read-only-connection
                  db
                  (fn [conn]
                    (c/fetch-rows db conn sql
                                  {:fetch-size (or @o/fetch-size 0)
                                   :max-rows (inc max-rows)
                                   :timeout timeout
                                   :result-set-fn #(read-result
                                                    % max-rows
                                                    max_column_width)})))]
            (text-result
             (render-rows rows max-rows width (elapsed-ms start)
                          (notes max-rows-clamped? timeout-clamped?)
                          ", see max_column_width")))
          (catch SQLException e
            (sql-error db e timeout (elapsed-ms start))))))))

(def ddl-commit-dbs
  "The databases committing the transaction a DDL statement runs in, by
  `structure/get-db-type`."
  #{:mysql :mariadb :oracle :h2})

(def ddl-commit-words
  "The first words of the statements `ddl-commit-dbs` commit the
  transaction with: DDL, grants, table maintenance and locks."
  #{"ALTER" "ANALYZE" "AUDIT" "CACHE" "CHECK" "COMMENT" "CREATE" "DROP"
    "FLASHBACK" "FLUSH" "GRANT" "INSTALL" "LOCK" "NOAUDIT" "OPTIMIZE" "PURGE"
    "RENAME" "REPAIR" "RESET" "REVOKE" "TRUNCATE" "UNINSTALL" "UNLOCK"})

(def transaction-words
  "The first words of the statements which may end the transaction they run
  in or start another one, on any database: `BEGIN` commits the open one on
  MySQL, `END` commits on PostgreSQL."
  #{"BEGIN" "COMMIT" "END" "ROLLBACK" "START"})

(defn- commit-problem
  "Describe how the statement `sql` may commit the transaction it runs in on
  `db`, nil if it can't. A statement starting with no word, e.g. with an
  executable comment of MySQL, may be any."
  [db sql]
  (let [word (lib/first-keyword sql (hash-comments? db))
        type (db-type db)]
    (cond
      (nil? word)
      (str "ejc-sql can't tell what it starts with, it may commit the "
           "transaction")
      (transaction-words word)
      (format "%s may end the transaction or start another one" word)
      (and (ddl-commit-dbs type) (ddl-commit-words word))
      (format "%s commits the transaction on %s" word (name type)))))

(defn- one-statement
  "Return [statement nil] for the `sql` of a statement to execute, a
  leading `delimiter X` line removed, [nil error] for no statement or
  several."
  [sql]
  (let [[sql separator] (split-delimiter sql)]
    (if separator
      (single-statement
       (statements sql separator) separator
       (str "Send them one by one, or as statements. To send a statement "
            "holding it, start its SQL with a line \"delimiter X\", where X "
            "doesn't occur in the statement."))
      [nil "Error: delimiter without a separator."])))

(defn- of-statement
  "`error` of the statement number `n` of `statements`, nil for `sql`."
  [n error]
  (if n
    (s/replace-first error "Error: " (format "Error: statement %d: " n))
    error))

(defn- execute-plan
  "Plan the run of the `execute` tool `args` on `db`: [groups nil], or
  [nil error] if they are rejected. The groups run in order, as `run-group`
  runs them, {:transaction?, :steps}; a step is a statement, {:n, :sql,
  :expected}, `:n` its number in `statements`, nil for `sql`, its
  `delimiter X` line removed. Several `statements` run in one transaction
  unless `transaction` is false, a statement with `expected_rows` in one
  of its own then. A statement which may commit the transaction it runs in,
  ending it before the next one runs, is rejected in one with others, and
  with `expected_rows`, which needs it rolled back."
  [db {:strs [sql statements expected_rows transaction]}]
  (let [together? (and (next statements) (not (false? transaction)))
        steps (for [[i {:strs [sql expected_rows]}]
                    (if statements
                      (map-indexed vector statements)
                      [[nil {"sql" sql "expected_rows" expected_rows}]])
                    :let [n (some-> i inc)
                          [sql error] (one-statement sql)
                          problem (when (and sql (or together? expected_rows))
                                    (commit-problem db sql))]]
                {:n n
                 :sql sql
                 ;; JSON may send 1 as 1.0, which is not = 1.
                 :expected (some-> expected_rows long)
                 :error (or error
                            (when problem
                              (format (if together?
                                        (str "Error: %s, so the statements "
                                             "can't run in one transaction. "
                                             "Send it in a call of its own, "
                                             "or pass transaction false to "
                                             "run the statements one by one, "
                                             "each committed at once.")
                                        (str "Error: %s, so it can't be "
                                             "rolled back as expected_rows "
                                             "needs."))
                                      problem)))})]
    (cond
      (= (nil? sql) (nil? statements))
      [nil "Error: send either sql or statements."]
      (and statements (some? expected_rows))
      [nil (str "Error: with statements, give the expected_rows of each of "
                "them in its item.")]
      (and sql (some? transaction))
      [nil (str "Error: transaction is for statements; sql runs in a "
                "transaction of its own with expected_rows, is committed at "
                "once without.")]
      :else
      (if-let [{:keys [n error]} (first (filter :error steps))]
        [nil (of-statement n error)]
        [(if together?
           [{:transaction? true :steps (vec steps)}]
           (for [step steps]
             {:transaction? (some? (:expected step)) :steps [step]}))
         nil]))))

(defn- run-statement
  "Run the statement `step` of an `execute` plan on `conn`, within what's
  left of `timeout` s since `start`: {:n, :text, :error?}. A result set is
  printed as `query` prints one. With an `:expected` count of rows, another
  update count is an error."
  [db connection-name ^Connection conn {:keys [n sql expected]} timeout start]
  (let [left (- timeout (quot (elapsed-ms start) 1000))]
    (if (< left 1)
      {:n n
       :error? true
       :text (format "Error: not run, the timeout of %d s (timeout_s) is over."
                     timeout)}
      (try
        (log-sql connection-name sql)
        (with-open [statement (.createStatement conn)]
          (.setQueryTimeout statement (int left))
          (.setMaxRows statement (inc default-max-rows))
          (let [result-set? (.execute statement sql)
                ;; Read once, see `c/execution-message`.
                result (when-not result-set?
                         (.getUpdateCount statement))]
            (cond
              (and expected result-set?)
              {:n n
               :error? true
               :text (format (str "Error: it returned rows, not a number of "
                                  "rows changed, as expected_rows %d needs.")
                             expected)}
              (and expected (not= expected (max result 0)))
              {:n n
               :error? true
               :text (format "Error: %d row%s affected, not expected_rows %d."
                             (max result 0) (if (= 1 result) "" "s") expected)}
              result-set?
              {:n n
               :text (let [{:keys [rows width]}
                           (read-result (j/result-set-seq (.getResultSet statement)
                                                          {:as-arrays? true})
                                        default-max-rows nil)]
                       (render-rows rows default-max-rows width
                                    (elapsed-ms start) []))}
              :else
              {:n n :text (c/count-message result)})))
        (catch SQLException e
          {:n n
           :error? true
           :text (sql-error-text db e timeout (elapsed-ms start))})))))

(defn- run-group
  "Run the statements of `group` of an `execute` plan on `conn`, up to the
  first failure: their outcomes, as `run-statement` returns them, and one
  more, `:commit?`, if the commit fails. A `:transaction?` group runs in a
  transaction, committed if all of them succeed, rolled back otherwise,
  whatever is thrown: it's never left open, Oracle commits it when the
  connection is closed. Any other runs in autocommit mode."
  [db connection-name ^Connection conn {:keys [transaction? steps]} timeout
   start]
  (let [run (fn []
              (reduce (fn [outcomes step]
                        (let [outcome (run-statement db connection-name conn
                                                     step timeout start)
                              outcomes (conj outcomes outcome)]
                          (if (:error? outcome)
                            (reduced outcomes)
                            outcomes)))
                      []
                      steps))]
    (.setAutoCommit conn (not transaction?))
    (if-not transaction?
      (run)
      (let [committed? (volatile! false)]
        (try
          (let [outcomes (run)]
            (if (some :error? outcomes)
              outcomes
              (try
                (.commit conn)
                (vreset! committed? true)
                outcomes
                (catch SQLException e
                  (conj outcomes
                        {:n (:n (peek outcomes))
                         :commit? true
                         :error? true
                         :text (mask-secrets
                                db (str "Error: the commit failed: "
                                        (.getMessage e)))})))))
          (finally
            (when-not @committed?
              (try
                (.rollback conn)
                (catch SQLException _)))))))))

(defn- batch-report
  "The `execute` result text of `statements`: the `outcomes` of its `steps`,
  a line per statement, numbered as in `statements`, the ones not run too,
  then how it ended, all of them in `one-transaction?` or not."
  [steps outcomes one-transaction?]
  (let [by-n (into {} (map (juxt :n identity) (remove :commit? outcomes)))
        failed (first (filter :error? outcomes))]
    (s/join "\n"
            (concat
             (for [{:keys [n]} steps]
               (if-let [{:keys [text error?]} (by-n n)]
                 ;; A table under the number, its columns aligned.
                 (str n "." (if (and (not error?) (s/includes? text "\n"))
                              "\n"
                              " ")
                      text)
                 (str n ". Not run.")))
             (keep #(when (:commit? %) (:text %)) outcomes)
             [(cond
                (nil? failed) "Committed."
                one-transaction? "Rolled back."
                (= 1 (:n failed)) "Stopped at statement 1, nothing is committed."
                :else (format (str "Stopped at statement %d, the ones before "
                                   "it are committed.")
                              (:n failed)))]))))

(defn- execute-tool
  "Run the `execute` tool: the statements of its `execute-plan` on a
  connection of its own to `db`, if `config` makes the connection writable.
  The completion cache of `db` is dropped after any statement: not only DDL
  changes the schema, a procedure or `SELECT INTO` of SQL Server may too."
  [db connection-name config {:strs [statements timeout_s] :as args}]
  (let [[groups error] (execute-plan db args)]
    (cond
      (not (contains? (:writable config) connection-name))
      (error-result (format "Error: writes to %s are not allowed; ask the user."
                            connection-name))
      error
      (error-result error)
      :else
      (let [[timeout timeout-clamped?] (limit timeout_s default-timeout
                                              timeout-limit)
            start (System/nanoTime)]
        (try
          (let [outcomes (with-open [conn (get-connection db)]
                           (reduce (fn [outcomes group]
                                     (let [outcomes (into outcomes
                                                          (run-group
                                                           db connection-name
                                                           conn group timeout
                                                           start))]
                                       (if (some :error? outcomes)
                                         (reduced outcomes)
                                         outcomes)))
                                   []
                                   groups))
                one-transaction? (and (= 1 (count groups))
                                      (:transaction? (first groups)))
                error? (boolean (some :error? outcomes))]
            (text-result
             (str (if statements
                    (batch-report (mapcat :steps groups) outcomes
                                  one-transaction?)
                    (s/join "\n" (cond-> (mapv :text outcomes)
                                   (and error? one-transaction?)
                                   (conj "Rolled back."))))
                  (when timeout-clamped?
                    (format "\n(timeout_s clamped to %d)" timeout-limit)))
             error?))
          (catch SQLException e
            (sql-error db e timeout (elapsed-ms start)))
          (finally
            (cache/invalidate-cache db)))))))

(def ^:private calling-thread
  "An executor running a task in the calling thread, for `.setNetworkTimeout`."
  (reify Executor
    (execute [_ task]
      (.run ^Runnable task))))

(defn- limit-network-wait
  "Make the calls on `conn` fail after `metadata-timeout` with no answer from
  the database, since the metadata calls of a driver take no query timeout.
  A driver not supporting it waits as long as it takes."
  [^Connection conn]
  (try
    (.setNetworkTimeout conn calling-thread (* 1000 metadata-timeout))
    (catch SQLException _)
    (catch AbstractMethodError _)))

(defn- describe-table-tool
  "Run the `describe_table` tool: what `describe-table` shows in Emacs, over
  a read-only connection of its own to `db`, with a timeout on every query.
  An error of the constraints query is shown after the columns, which are
  still of use, and makes the result an error too. Only the errors are
  masked: the password of `db` may be part of a name or a value too."
  [db {:strs [table owner max_column_width]}]
  (let [success (atom true)
        ;; java.jdbc passes the keys of a db-spec map on to
        ;; `prepare-statement`, `:timeout` too.
        meta-db (fn [conn] {:connection conn :timeout metadata-timeout})
        text (binding [lib/*max-column-width* (column-width max_column_width)
                       o/*mark-cuts* true
                       o/*add-outside-borders* false]
               (with-out-str
                 (with-read-only-connection
                   db
                   (fn [conn]
                     (limit-network-wait conn)
                     (structure/print-table-description
                      db table
                      :owner owner
                      :details-fn (fn [_ owner table]
                                    (structure/table-details conn owner table))
                      :table-meta-fn (fn [_ table-name]
                                       (let [result (c/query-meta
                                                     (meta-db conn)
                                                     (str "SELECT * FROM "
                                                          table-name))]
                                         (if (:success result)
                                           result
                                           (do (reset! success false)
                                               (update result :result
                                                       #(mask-secrets db %))))))
                      :query-fn (fn [_ sql]
                                  (try
                                    (c/fetch-rows (meta-db conn) conn sql
                                                  {:timeout metadata-timeout})
                                    (catch SQLException e
                                      (reset! success false)
                                      (mask-secrets
                                       db (str "Error: " (.getMessage e))))))
                      :row-limit 0)))))]
    (text-result text (not @success))))

(defn- list-tables-tool
  "Run the `list_tables` tool: the table list query of completion, on a
  read-only connection of its own to `db`, with a timeout. Not through the
  completion cache: it takes an error for no tables and may be stale."
  [db {:strs [owner pattern]}]
  (if-let [sql (if owner
                 (structure/select-db-meta-script db :tables :owner owner)
                 (or (structure/select-db-meta-script db :all-tables)
                     (structure/select-db-meta-script db :tables)))]
    (let [rows (with-read-only-connection
                 db
                 (fn [conn]
                   (limit-network-wait conn)
                   (j/query {:connection conn :timeout metadata-timeout}
                            [sql] {:as-arrays? true})))
          ;; The name comes first in the list of an owner, last in the
          ;; one of all owners, as `structure/get-tables` takes it.
          tables (cond->> (sort (distinct (map (comp str (if owner first last))
                                               (rest rows))))
                   pattern (filter #(s/includes? (s/lower-case %)
                                                 (s/lower-case pattern))))]
      (text-result
       (if (seq tables)
         (str (count tables) (if (= 1 (count tables)) " table:\n" " tables:\n")
              (s/join "\n" tables))
         "No tables found.")))
    (error-result "Error: ejc-sql can't list the tables of this database.")))

(defn- find-columns-tool
  "Run the `find_columns` tool: `structure/find-columns` on a read-only
  connection of its own to `db`, with the network wait limited, since the
  metadata calls take no query timeout."
  [db {:strs [column owner table]}]
  (let [found (with-read-only-connection
                db
                (fn [conn]
                  (limit-network-wait conn)
                  (structure/find-columns conn owner table column
                                          (inc find-columns-limit))))
        shown (take find-columns-limit found)]
    (text-result
     (if (seq shown)
       (str (with-out-str
              (binding [o/*add-outside-borders* false]
                (o/print-maps [:owner :table :column :type] shown true)))
            "\n"
            (count shown) (if (= 1 (count shown)) " column" " columns")
            (when (> (count found) find-columns-limit)
              (format (str ", truncated at %d; give the owner, the table or "
                           "a longer part of the name")
                      find-columns-limit)))
       "No columns found."))))

(defn- identifier-case
  "How the database of `meta` keeps unquoted names, for `connection_info`."
  [^DatabaseMetaData meta]
  (cond
    (.storesUpperCaseIdentifiers meta) "upper-cased"
    (.storesLowerCaseIdentifiers meta) "lower-cased"
    :else "as written"))

(defn- description
  "The `:description` of the connection `db` the user wrote, on one line,
  nil if there's none."
  [db]
  (some-> (:description db) (s/replace #"\s*[\r\n]+\s*" " ") s/trim
          not-empty))

(defn- connection-info-tool
  "Run the `connection_info` tool: the `description` of `db` and what the
  JDBC metadata of a read-only connection of its own to `db` tells of the
  database, nothing of the user or the URL."
  [db]
  (text-result
   (with-read-only-connection
     db
     (fn [^Connection conn]
       (limit-network-wait conn)
       (let [meta (.getMetaData conn)
             quote (s/trim (str (.getIdentifierQuoteString meta)))]
         (s/join "\n"
                 (remove nil?
                         [(some->> (description db) (str "Description: "))
                          (str "Database: " (.getDatabaseProductName meta) " "
                               (.getDatabaseProductVersion meta))
                          (str "Driver: " (.getDriverName meta) " "
                               (.getDriverVersion meta))
                          (some->> (.getCatalog conn) not-empty
                                   (str "Catalog: "))
                          (some->> (structure/current-schema conn) not-empty
                                   (str "Schema: "))
                          (str "Unquoted names are kept "
                               (identifier-case meta)
                               (when (seq quote)
                                 (str "; quote names with " quote)))])))))))

(defn- host-and-port
  "Host and port of `db`, from its keys or from the `//host:port` part of
  its URL."
  [db]
  (if (:host db)
    [(:host db) (:port db)]
    (when-let [[_ host port] (some
                              #(when (string? %)
                                 (re-find
                                  #"^(?:jdbc:[^/]*:)?//(?:[^@/;?]*@)?([^:/;?@]+)(?::(\d+))?"
                                  %))
                              [(:subname db) (:connection-uri db)])]
      [host port])))

(defn- without-credentials
  "`db` with the user and the password stripped from its URL, so that no
  part of the address parsed from it shows them: `//user:password@`,
  `password=...` and the Oracle `user/password@`."
  [db]
  (let [strip #(-> %
                   (s/replace oracle-credentials-re "$1@")
                   (s/replace #"(?i)(password\s*=\s*)[^;&\s]*" "$1")
                   ;; Not past `;` or `?`: `;user=alice@corp.com` of
                   ;; SQL Server is a property.
                   (s/replace #"//[^/@\s;?]*@" "//"))]
    (cond-> db
      (string? (:subname db)) (update :subname strip)
      (string? (:connection-uri db)) (update :connection-uri strip))))

(defn- oracle-address
  "Host, port and database of the Oracle URL of `db`: `@host:port:SID` or
  `@[//]host[:port]/service`. nil for a TNS alias or a `(DESCRIPTION=...)`,
  which `structure/get-db-name` takes for the database whole."
  [db]
  (some->> [(:subname db) (:connection-uri db)]
           (some #(when (string? %) %))
           (re-find
            #"@(?:\w+://|//)?([^:/@()\s]+)(?::(\d+))?[:/]([^:/?@()\s]+)")
           rest
           vec))

(defn- address
  "Host, port and database of `db` for `list_connections`, as far as its
  keys or its URL tell them, the unknown ones nil. The credentials are
  stripped from the URL first."
  [db]
  (let [safe (fn [f] (try (f) (catch Exception _ nil)))
        db (without-credentials db)]
    (if (and (= :oracle (db-type db)) (not (:host db)))
      (safe #(oracle-address db))
      (let [[host port] (safe #(host-and-port db))]
        [host port (safe #(some-> (structure/get-db-name db)
                                  (s/split #"[?;&]")
                                  first
                                  not-empty))]))))

(defn- connection-line
  "Describe the connection `db` for `list_connections`: enough to choose
  it, nothing about the user and the password, its `description` last."
  [connection-name db writable?]
  (let [type (some-> (db-type db) name)
        [host port dbname] (address db)]
    (str "- " connection-name ": "
         (s/join ", " (remove nil? [type
                                    (when host (str "host " host))
                                    (when port (str "port " port))
                                    (when dbname (str "database " dbname))
                                    (if writable?
                                      "writable (execute allowed)"
                                      "read-only")]))
         (some->> (description db) (str " -- ")))))

(defn- list-connections-tool
  "Run the `list_connections` tool. `config` tells the writable ones. The
  credentials are stripped from a URL before its address is parsed, and
  what looks like them in a URL is masked still, but not the password
  wherever it occurs: a dev database is often named as its password, the
  connection too."
  [config]
  (let [registered (connections)]
    (text-result
     (if (seq registered)
       (str "Connections:\n"
            (s/join "\n"
                    (for [connection-name (sort (keys registered))
                          :let [db (registered connection-name)]]
                      (mask-secrets {} (connection-line
                                        connection-name db
                                        (contains? (:writable config)
                                                   connection-name))))))
       (format (str "No connections. Ask the user to add the database to "
                    "%s or to connect to it with M-x ejc-connect in Emacs.")
               (databases-file))))))

(defn- unknown-connection
  "The error for a connection not registered, telling the agent what to do.
  `names` are the ones there are."
  [connection-name names]
  (error-result
   (format (str "Error: unknown connection %s. Connected: %s. If the "
                "database is missing, ask the user to add it to %s or to "
                "connect to it with M-x ejc-connect in Emacs.")
           connection-name
           (if (seq names) (s/join ", " names) "none")
           (databases-file))))

(defn call-tool
  "Run the tool `tool-name` with `args` (string keys). `config` is the
  parsed `config-file`. Return the `tools/call` result."
  [tool-name args config]
  (let [tool (tool-by-name tool-name)]
    (if-let [problem (argument-error (:inputSchema tool) args)]
      (error-result (str "Error: " problem))
      (try
        (if (= tool-name "list_connections")
          (list-connections-tool config)
          (let [connection-name (get args "connection")
                connected (connections)
                db (get connected connection-name)]
            (cond
              (name-error args) (error-result (name-error args))
              (nil? db) (unknown-connection connection-name
                                            (sort (keys connected)))
              :else
              (try
                ;; Emacs loaded the driver of its connection itself.
                (when-not (contains? @registry connection-name)
                  (await-driver db driver-wait-ms))
                ;; The tables printed by `output.clj` for Emacs too.
                (binding [o/*ascii-borders* true]
                  (case tool-name
                    "connection_info" (connection-info-tool db)
                    "list_tables" (list-tables-tool db args)
                    "find_columns" (find-columns-tool db args)
                    "describe_table" (describe-table-tool db args)
                    "query" (query-tool db connection-name args)
                    "execute" (execute-tool db connection-name config args)))
                (catch Exception e
                  (error-result
                   (mask-secrets db (str "Error: " (exception-message e)))))))))
        ;; A malformed `databases-file`.
        (catch clojure.lang.ExceptionInfo e
          (error-result (str "Error: " (.getMessage e))))))))

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
  (str "Databases the user made available in ejc-sql. Call "
       "list_connections first. query runs one read-only statement per call; "
       "execute runs statements that change data or schema, only on the "
       "connections the user made writable, and the user approves every "
       "call. If a database is missing, ask the user to add it to "
       "databases.edn of ejc-sql or to connect to it with M-x ejc-connect in "
       "Emacs; never reach databases by other means. If the server stops "
       "answering, ask the user to start it, with M-x ejc-connect to any "
       "database in Emacs or with java -jar ejc-sql.jar, then to reconnect "
       "the client to it, e.g. with /mcp in Claude Code."))

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
  rules, a request with `protocol-version` by the modern ones. An exception
  no tool caught is the JSON-RPC error -32603, its text masked."
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
      :else
      (try
        (if (and (= version protocol-version) (not= method "initialize"))
          (modern-request headers id method params config)
          (legacy-request id method params config))
        (catch Throwable e
          (response 200 (rpc-error id -32603
                                   (str "Internal error: "
                                        (mask-all-secrets (str e))))))))))
