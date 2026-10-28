(ns ejc-sql.mcp-test
  (:require [clojure.java.jdbc :as j]
            [clojure.string :as s]
            [clojure.test :refer :all]
            [ejc-sql.cache :as cache]
            [ejc-sql.connect :as c]
            [ejc-sql.lib :as lib]
            [ejc-sql.mcp :as mcp]
            [ejc-sql.output :as o]))

(def h2-db
  {:classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "mem:ejc_mcp_test;DB_CLOSE_DELAY=-1"
   :user "a_user"
   :password "s3cret"})

(def config {:writable #{"h2-rw"}})

(def logged
  "SQL written to the query log by the test."
  (atom []))

(def opened
  "Connections opened by the MCP tools in the test."
  (atom 0))

(defn- fixture
  "Run the test `f` on a fresh H2 database registered twice, as read-only
  \"h2\" and writable \"h2-rw\", recording the query log and counting the
  connections the tools open."
  [f]
  (j/db-do-commands h2-db ["DROP ALL OBJECTS"
                           (str "CREATE TABLE users (id INT PRIMARY KEY, "
                                "login VARCHAR(20))")
                           (str "INSERT INTO users VALUES "
                                "(1, 'admin'), (2, 'neo'), (3, 'trinity')")
                           ;; SLEEP(nanoseconds)
                           (str "CREATE ALIAS SLEEP FOR "
                                "\"java.util.concurrent.locks.LockSupport.parkNanos\"")])
  (cache/invalidate-cache h2-db)
  (reset! logged [])
  (reset! opened 0)
  (let [get-connection mcp/get-connection]
    (with-redefs [mcp/registry (atom {"h2" h2-db "h2-rw" h2-db})
                  o/log-sql #(swap! logged conj %)
                  mcp/get-connection (fn [db]
                                       (swap! opened inc)
                                       (get-connection db))]
      (f))))

(use-fixtures :each fixture)

(defn- call [tool args]
  (mcp/call-tool tool args config))

(defn- text [result]
  (get-in result [:content 0 :text]))

(defn- logins []
  (map :login (j/query h2-db ["SELECT login FROM users ORDER BY id"])))

;;; Protocol

(def meta-version {"io.modelcontextprotocol/protocolVersion" "2026-07-28"
                   "io.modelcontextprotocol/clientCapabilities" {}})

(defn- request [method & [params]]
  (cond-> {"jsonrpc" "2.0" "id" 1 "method" method}
    params (assoc "params" params)))

(defn- legacy [method & [params headers]]
  (mcp/handle-rpc (or headers {}) (request method params) config))

(defn- modern-headers [method & [tool]]
  (cond-> {"mcp-protocol-version" "2026-07-28" "mcp-method" method}
    tool (assoc "mcp-name" tool)))

(defn- modern
  "Send a request by the rules of `mcp/protocol-version`, with the headers
  matching it unless `headers` are given."
  [method & [params headers]]
  (mcp/handle-rpc (or headers (modern-headers method (get params "name")))
                  (request method (assoc params "_meta" meta-version))
                  config))

(deftest legacy-protocol-test
  (testing "initialize echoes a legacy revision, offers the newest otherwise."
    (doseq [version mcp/legacy-protocol-versions]
      (let [{:keys [status body]} (legacy "initialize" {"protocolVersion" version})]
        (is (= 200 status))
        (is (= version (get-in body [:result :protocolVersion])))))
    (let [result (:result (:body (legacy "initialize"
                                         {"protocolVersion" "2026-07-28"})))]
      (is (= "2025-11-25" (:protocolVersion result)))
      (is (= {:tools {:listChanged false}} (:capabilities result)))
      (is (= "ejc-sql" (get-in result [:serverInfo :name])))
      (is (string? (get-in result [:serverInfo :version])))
      (is (s/includes? (:instructions result) "M-x ejc-connect"))
      (is (not (contains? result :resultType)))))
  (testing "ping, tools/list, unknown method."
    (is (= {:status 200 :body {:jsonrpc "2.0" :id 1 :result {}}}
           (legacy "ping")))
    (is (= {:tools mcp/tools}
           (:result (:body (legacy "tools/list" nil
                                   {"mcp-protocol-version" "2025-06-18"})))))
    (let [{:keys [status body]} (legacy "resources/list")]
      (is (= 200 status))
      (is (= -32601 (get-in body [:error :code])))))
  (testing "tools/call."
    (let [result (:result (:body (legacy "tools/call"
                                         {"name" "list_connections"})))]
      (is (false? (:isError result)))
      (is (not (contains? result :resultType))))))

(deftest modern-protocol-test
  (testing "server/discover."
    (let [{:keys [status body]} (modern "server/discover")
          result (:result body)]
      (is (= 200 status))
      (is (= "complete" (:resultType result)))
      (is (= mcp/supported-protocol-versions (:supportedVersions result)))
      (is (= {:tools {}} (:capabilities result)))
      (is (s/includes? (:instructions result) "list_connections"))
      (is (pos? (:ttlMs result)))
      (is (= "public" (:cacheScope result)))
      (is (= "ejc-sql" (get-in result [:_meta "io.modelcontextprotocol/serverInfo"
                                       :name])))))
  (testing "tools/list."
    (let [result (:result (:body (modern "tools/list")))]
      (is (= {:resultType "complete"
              :tools mcp/tools
              :ttlMs mcp/tools-ttl-ms
              :cacheScope "public"}
             result))))
  (testing "tools/call."
    (let [result (:result (:body (modern "tools/call"
                                         {"name" "list_connections"})))]
      (is (= "complete" (:resultType result)))
      (is (false? (:isError result))))
    (testing "Mcp-Name may be base64-encoded."
      (is (= 200 (:status (modern "tools/call" {"name" "list_connections"}
                                  (assoc (modern-headers "tools/call")
                                         "mcp-name"
                                         "=?base64?bGlzdF9jb25uZWN0aW9ucw==?=")))))))
  (testing "Headers must match the body."
    (doseq [[params headers] [[nil (dissoc (modern-headers "tools/list")
                                           "mcp-method")]
                              [nil (modern-headers "tools/call")]
                              [{"name" "query"} (modern-headers "tools/call")]
                              [{"name" "query"} (modern-headers "tools/call"
                                                                "execute")]
                              [{"name" "query"} (modern-headers "tools/call"
                                                                "=?base64?!?=")]]]
      (let [{:keys [status body]} (modern (if params "tools/call" "tools/list")
                                          params headers)]
        (is (= 400 status) (pr-str headers))
        (is (= -32020 (get-in body [:error :code])) (pr-str headers))))
    (let [{:keys [status body]} (mcp/handle-rpc (modern-headers "tools/list")
                                                (request "tools/list")
                                                config)]
      (is (= 400 status))
      (is (= -32020 (get-in body [:error :code])))))
  (testing "Removed and unknown methods."
    (doseq [method ["ping" "resources/list"]]
      (let [{:keys [status body]} (modern method)]
        (is (= 404 status))
        (is (= -32601 (get-in body [:error :code])))))
    (testing "initialize selects the legacy rules."
      (let [{:keys [status body]} (modern "initialize"
                                          {"protocolVersion" "2025-06-18"})]
        (is (= 200 status))
        (is (= "2025-06-18" (get-in body [:result :protocolVersion])))))))

(deftest protocol-test
  (testing "Unsupported MCP-Protocol-Version."
    (let [{:keys [status body]} (legacy "tools/list" nil
                                        {"mcp-protocol-version" "1900-01-01"})]
      (is (= 400 status))
      (is (= -32022 (get-in body [:error :code])))
      (is (= {:supported mcp/supported-protocol-versions
              :requested "1900-01-01"}
             (get-in body [:error :data])))))
  (testing "Notifications and client responses get no answer."
    (is (= {:status 202 :body nil}
           (mcp/handle-rpc {} {"jsonrpc" "2.0"
                               "method" "notifications/initialized"}
                           config)))
    (is (= {:status 202 :body nil}
           (mcp/handle-rpc {} {"jsonrpc" "2.0" "id" 5 "result" {}} config))))
  (testing "Invalid requests."
    (doseq [message [{"id" 1 "method" "ping"}
                     {"jsonrpc" "2.0" "id" nil "method" "ping"}
                     {"jsonrpc" "2.0" "id" {} "method" "ping"}
                     {"jsonrpc" "2.0" "id" 1}
                     "ping"]]
      (let [{:keys [status body]} (mcp/handle-rpc {} message config)]
        (is (= 400 status) (pr-str message))
        (is (= -32600 (get-in body [:error :code])) (pr-str message)))))
  (testing "tools/call protocol errors."
    (doseq [params [{"name" "no_such_tool"}
                    {"arguments" {}}
                    {"name" "query" "arguments" []}]]
      (let [{:keys [status body]} (legacy "tools/call" params)]
        (is (= 200 status))
        (is (= -32602 (get-in body [:error :code])) (pr-str params)))))
  (testing "Arguments failing the schema are tool errors."
    (doseq [[args problem] [[{"connection" "h2"} "missing required argument sql"]
                            [{"connection" "h2" "sql" ""} "argument sql must be"]
                            [{"connection" "h2" "sql" "select 1" "max_rows" "x"}
                             "argument max_rows must be an integer"]
                            [{"connection" "h2" "sql" "select 1" "max_rows" 0}
                             "argument max_rows must be an integer >= 1"]
                            [{"connection" "h2" "sql" "select 1" "limit" 5}
                             "unknown argument limit"]]]
      (let [result (:result (:body (legacy "tools/call" {"name" "query"
                                                         "arguments" args})))]
        (is (:isError result) (pr-str args))
        (is (s/includes? (text result) problem) (pr-str args))))))

(deftest tools-test
  (testing "tools/list shape."
    (is (= ["list_connections" "list_tables" "describe_table" "query" "execute"]
           (map :name mcp/tools)))
    (doseq [{:keys [title description inputSchema annotations] :as tool} mcp/tools]
      (is (string? title))
      (is (string? description))
      (is (= "object" (:type inputSchema)))
      (is (false? (:additionalProperties inputSchema)))
      (is (every? (set (map name (keys (:properties inputSchema))))
                  (:required inputSchema)))
      (doseq [[_ property] (:properties inputSchema)]
        (is (string? (:description property))))
      (is (false? (:openWorldHint annotations)))
      (if (= "execute" (:name tool))
        (do (is (false? (:readOnlyHint annotations)))
            (is (true? (:destructiveHint annotations)))
            (is (= {"anthropic/requiresUserInteraction" true} (:_meta tool))))
        (do (is (true? (:readOnlyHint annotations)))
            (is (nil? (:_meta tool))))))
    (is (= {"list_connections" nil
            "list_tables" ["connection"]
            "describe_table" ["connection" "table"]
            "query" ["connection" "sql"]
            "execute" ["connection" "sql"]}
           (into {} (map (juxt :name #(get-in % [:inputSchema :required]))
                         mcp/tools))))))

;;; Tools

(deftest read-only-statements-test
  (testing "Single statement."
    (is (= ["select 1"] (mcp/statements "select 1;" ";")))
    (is (= ["-- one\nselect 1"] (mcp/statements "-- one\nselect 1; -- two" ";")))
    (is (= ["select 1 --"] (mcp/statements "select 1 -- ;" ";")))
    (is (= 2 (count (mcp/statements "select 1; select 2" ";"))))
    (is (= 2 (count (mcp/statements "select 1; drop table t" ";"))))
    (is (= 2 (count (mcp/statements "a $$ b" "$$")))))
  (testing "A separator in a string literal or a comment separates too."
    (doseq [sql ["select ';' as x"
                 "select '--'; drop table t"
                 "select 1 /* ' */; drop table t /* ' */"
                 "select 1 /* ; */"]]
      (is (= 2 (count (mcp/statements sql ";"))) sql)))
  (testing "query rejects before JDBC."
    (doseq [sql ["insert into users values (4, 'x')"
                 "update users set login = 'x'"
                 "delete from users"
                 "drop table users"
                 "call sleep(1000)"
                 "select 1; select 2"
                 "select 1; drop table users"
                 "select '--'; drop table users"
                 "select 1 /* ' */; drop table users /* ' */"
                 "/* /* */ select 1 */ drop table users"
                 "/*! drop table users */ select 1"
                 "delimiter /\nselect 1"
                 "-- nothing"]]
      (let [result (call "query" {"connection" "h2" "sql" sql})]
        (is (:isError result) sql)
        (is (s/starts-with? (text result) "Error: ") sql)))
    (is (= 0 @opened))
    (is (= [] @logged))
    (is (= ["admin" "neo" "trinity"] (logins))))
  (testing "The separator is explained."
    (is (s/includes? (text (call "query" {"connection" "h2"
                                          "sql" "select ';' as x"}))
                     "one in a string literal or a comment separates too")))
  (testing "The keyword is named."
    (is (s/includes? (text (call "query" {"connection" "h2"
                                          "sql" "delete from users"}))
                     "not DELETE"))))

(deftest tsql-test
  (testing "Words outside string literals, quoted names and comments."
    (is (= ["SELECT" "N" "@SET" "#T" "FROM" "T"]
           (mcp/tsql-words (str "select N'it''s DROP' /* a /* nested */ DROP */ "
                                "@set, [up]]date], \"de\"\"lete\" -- commit\n"
                                "#t from t"))))
    (doseq [sql ["select 'abc" "select [abc" "select \"abc" "select 1 /* a /* b */"]]
      (is (nil? (mcp/tsql-words sql)) sql)))
  (testing "Read-only statements pass."
    (doseq [sql ["SELECT TOP 10 * FROM users WITH (NOLOCK) ORDER BY id"
                 "SELECT id FROM users ORDER BY id OFFSET 0 ROWS FETCH NEXT 5 ROWS ONLY"
                 "SELECT [update], 'DROP' AS x, \"delete\" FROM t -- commit"
                 "SELECT 1 /* /* */ DROP TABLE users */"
                 "WITH x AS (SELECT 1 AS a) SELECT a FROM x FOR JSON PATH"
                 "SELECT @@TRANCOUNT"]]
      (is (nil? (mcp/tsql-error sql)) sql)))
  (testing "Other statements, writes and unclosed quotes are rejected."
    (doseq [[sql word] [["SELECT 1 DROP TABLE users COMMIT" "holds DROP"]
                        ["SELECT * INTO users2 FROM users" "holds INTO"]
                        ["SELECT 1 EXEC('DROP TABLE users')" "holds EXEC"]
                        ["SELECT 1 /* */ */ DELETE FROM users" "holds DELETE"]
                        ["SELECT NEXT VALUE FOR dbo.seq" "holds NEXT VALUE FOR"]
                        ["SELECT 'abc" "not closed"]]]
      (is (s/includes? (str (mcp/tsql-error sql)) word) sql)))
  (testing "query checks it on SQL Server and Sybase only, before JDBC."
    (with-redefs [mcp/registry (atom {"mssql" {:subprotocol "sqlserver"
                                               :subname "//nowhere:1433"}
                                      "sybase" {:subprotocol "jtds:sybase"
                                                :subname "//nowhere:5000"}})]
      (doseq [connection ["mssql" "sybase"]]
        (let [result (call "query" {"connection" connection
                                    "sql" "SELECT 1 DROP TABLE users COMMIT"})]
          (is (:isError result) connection)
          (is (s/includes? (text result) "holds DROP") connection))))
    (is (= 0 @opened))
    (is (not (:isError (call "query" {"connection" "h2"
                                      "sql" (str "SELECT login FROM users "
                                                 "WHERE id = 1 FOR UPDATE")}))))))

(deftest names-test
  (doseq [bad ["users'" "users;" "us ers" "users\\" "public.users" "users--"]]
    (doseq [[tool args] [["describe_table" {"connection" "h2" "table" bad}]
                         ["describe_table" {"connection" "h2" "table" "users"
                                            "owner" bad}]
                         ["list_tables" {"connection" "h2" "owner" bad}]]]
      (let [result (call tool args)]
        (is (:isError result) (pr-str args))
        (is (s/includes? (text result) "is not a valid name") (pr-str args)))))
  (is (= 0 @opened)))

(deftest query-test
  (testing "Rows and footer."
    (let [result (call "query" {"connection" "h2"
                                "sql" "SELECT id, login FROM users ORDER BY id"})]
      (is (not (:isError result)))
      (is (re-matches
           #"(?s)\| id \| login +\|\n\|----\+-+\|\n\| 1  \| admin +\|\n.*\| 3  \| trinity \|\n\n3 rows, \d+ ms"
           (text result))
          (text result)))
    (is (= ["-- [mcp h2]\nSELECT id, login FROM users ORDER BY id\n"] @logged)))
  (testing "Truncation and clamping."
    (is (re-find #"\n2 rows, \d+ ms, truncated at max_rows 2$"
                 (text (call "query" {"connection" "h2"
                                      "sql" "SELECT * FROM users"
                                      "max_rows" 2}))))
    (is (re-find #"\n3 rows, \d+ ms$"
                 (text (call "query" {"connection" "h2"
                                      "sql" "SELECT * FROM users"
                                      "max_rows" 3}))))
    (is (re-find #"\n3 rows, \d+ ms; max_rows clamped to 1000; timeout_s clamped to 50$"
                 (text (call "query" {"connection" "h2"
                                      "sql" "SELECT * FROM users"
                                      "max_rows" 5000
                                      "timeout_s" 500})))))
  (testing "SQL errors."
    (let [result (call "query" {"connection" "h2"
                                "sql" "SELECT * FROM no_such_table"})]
      (is (:isError result))
      (is (s/starts-with? (text result) "Error: "))))
  (testing "Timeout."
    (let [result (call "query" {"connection" "h2"
                                ;; H2 checks the timeout every 128 rows.
                                "sql" (str "SELECT COUNT(SLEEP(2000000)) "
                                           "FROM SYSTEM_RANGE(1, 2000)")
                                "timeout_s" 1})]
      (is (:isError result))
      (is (s/includes? (text result) "timeout of 1 s"))))
  (testing "Unknown connection."
    (let [result (call "query" {"connection" "prod" "sql" "SELECT 1"})]
      (is (:isError result))
      (is (s/includes? (text result) "Connected: h2, h2-rw"))
      (is (s/includes? (text result) "M-x ejc-connect")))))

(defn- recording-connection
  "Wrap `conn` to record [method-name & args] of the calls of it and of
  its statements into the `calls` atom."
  [conn calls]
  (letfn [(wrap [object ^Class interface]
            (java.lang.reflect.Proxy/newProxyInstance
             (.getClassLoader interface)
             (into-array Class [interface])
             (reify java.lang.reflect.InvocationHandler
               (invoke [_ _ method args]
                 (swap! calls conj (into [(.getName method)] args))
                 (let [result (try
                                (.invoke method object args)
                                (catch java.lang.reflect.InvocationTargetException e
                                  (throw (.getCause e))))]
                   (cond
                     (instance? java.sql.PreparedStatement result)
                     result
                     (instance? java.sql.Statement result)
                     (wrap result java.sql.Statement)
                     :else
                     result))))))]
    (wrap conn java.sql.Connection)))

(deftest query-rollback-test
  (testing "A write past the predicate is rolled back."
    (with-redefs [c/fetch-rows (fn [_ ^java.sql.Connection conn _ _]
                                 (with-open [st (.createStatement conn)]
                                   (.executeUpdate
                                    st "INSERT INTO users VALUES (4, 'smith')"))
                                 [[:x] [1]])]
      (is (not (:isError (call "query" {"connection" "h2"
                                        "sql" "SELECT 1"})))))
    (is (= ["admin" "neo" "trinity"] (logins))))
  (testing "Every read-only layer runs, in order."
    (doseq [[mode switch] [[:session "SET LOCK_TIMEOUT 1000"]
                           [:transaction "SET LOCK_TIMEOUT 2000"]]]
      (let [calls (atom [])
            get-connection mcp/get-connection]
        (with-redefs [mcp/read-only-switches {:h2 [mode switch]}
                      mcp/get-connection #(recording-connection
                                           (get-connection %) calls)]
          (is (not (:isError (call "query" {"connection" "h2"
                                            "sql" "SELECT 1"})))))
        (is (= (concat (when (= mode :session) [["execute" switch] ["close"]])
                       [["setAutoCommit" false]
                        ["setReadOnly" true]]
                       (when (= mode :transaction) [["execute" switch] ["close"]])
                       [["prepareStatement" "SELECT 1"]
                        ["rollback"]
                        ["close"]])
               (filter (comp #{"execute" "setAutoCommit" "setReadOnly"
                               "prepareStatement" "rollback" "close"}
                             first)
                       @calls))
            (pr-str mode)))))
  (testing "A failing switch fails the query."
    (with-redefs [mcp/read-only-switches {:h2 [:session "NO SUCH COMMAND"]}]
      (let [result (call "query" {"connection" "h2" "sql" "SELECT 1"})]
        (is (:isError result))
        (is (s/includes? (text result) "can't make the connection read-only"))))))

(deftest execute-test
  (testing "Not writable."
    (let [result (call "execute" {"connection" "h2"
                                  "sql" "DELETE FROM users"})]
      (is (:isError result))
      (is (= "Error: writes to h2 are not allowed; ask the user." (text result))))
    (is (= 0 @opened))
    (is (= [] @logged)))
  (testing "Writable."
    (let [result (call "execute" {"connection" "h2-rw"
                                  "sql" "INSERT INTO users VALUES (4, 'smith')"})]
      (is (not (:isError result)))
      (is (= "Records affected: 1" (text result))))
    (is (= ["admin" "neo" "trinity" "smith"] (logins)))
    (is (= ["-- [mcp h2-rw]\nINSERT INTO users VALUES (4, 'smith')\n"] @logged)))
  (testing "One statement, unless a delimiter says otherwise."
    (let [sql (str "INSERT INTO users VALUES (5, 'a');"
                   "INSERT INTO users VALUES (6, 'b')")]
      (is (:isError (call "execute" {"connection" "h2-rw" "sql" sql})))
      (is (not (:isError (call "execute" {"connection" "h2-rw"
                                          "sql" (str "delimiter /\n" sql)}))))
      (is (s/includes? (text (call "execute"
                                   {"connection" "h2-rw"
                                    "sql" "INSERT INTO users VALUES (7, ';')"}))
                       "delimiter X")))
    (is (= ["admin" "neo" "trinity" "smith" "a" "b"] (logins))))
  (testing "Result sets."
    (is (re-find #"\| login +\|[\s\S]*\n6 rows, \d+ ms$"
                 (text (call "execute" {"connection" "h2-rw"
                                        "sql" "SELECT login FROM users"})))))
  (testing "DDL invalidates the cache."
    (swap! cache/cache assoc-in [h2-db :all-tables] (future ["USERS"]))
    (is (= "Executed" (text (call "execute" {"connection" "h2-rw"
                                             "sql" "CREATE TABLE t2 (id INT)"}))))
    (is (nil? (get @cache/cache h2-db))))
  (testing "SQL errors."
    (let [result (call "execute" {"connection" "h2-rw"
                                  "sql" "INSERT INTO nope VALUES (1)"})]
      (is (:isError result))
      (is (s/starts-with? (text result) "Error: ")))))

(deftest metadata-test
  (testing "list_tables."
    (is (= "1 table:\nUSERS" (text (call "list_tables" {"connection" "h2"}))))
    (is (= "1 table:\nUSERS" (text (call "list_tables" {"connection" "h2"
                                                         "owner" "PUBLIC"
                                                         "pattern" "sE"}))))
    (is (= "No tables found." (text (call "list_tables" {"connection" "h2"
                                                          "pattern" "zzz"})))))
  (testing "describe_table."
    (let [result (call "describe_table" {"connection" "h2" "table" "users"})]
      (is (not (:isError result)))
      (is (s/starts-with? (text result) "Table \"PUBLIC.USERS\" description:"))
      (is (re-find #"\| ID +\| INTEGER +\| not null +\|" (text result)))
      (is (re-find #"\| LOGIN +\| CHARACTER VARYING\(20\) +\| +\|" (text result)))
      (is (re-find #"\nPrimary key \w+: ID\n" (text result))))
    (let [result (call "describe_table" {"connection" "h2" "table" "nope"})]
      (is (:isError result))
      (is (s/starts-with? (text result) "Error: ")))))

(deftest shared-state-test
  (testing "The tools don't touch the state shared with Emacs."
    (let [db-before (reset! c/db {:subprotocol "emacs"})
          query-before (reset! c/current-query {:start-time 42})]
      (doseq [[tool args] [["list_connections" {}]
                           ["list_tables" {"connection" "h2"}]
                           ["describe_table" {"connection" "h2" "table" "users"}]
                           ["query" {"connection" "h2" "sql" "SELECT 1"}]
                           ["execute" {"connection" "h2-rw"
                                       "sql" "UPDATE users SET login = login"}]]]
        (is (not (:isError (call tool args))) tool)
        (is (= db-before @c/db) tool)
        (is (= query-before @c/current-query) tool)))))

(deftest redaction-test
  (let [db {:subprotocol "postgresql"
            :subname "//dbhost:5432/shop?password=pw1"
            :user "u1"
            :password "pw2"
            :connection-uri "jdbc:mysql://u1:pw3@h/db"}]
    (testing "list_connections."
      (with-redefs [mcp/registry (atom {"pg" db "h2" h2-db})]
        (let [listing (text (call "list_connections" {}))]
          (is (= (str "Connections:\n"
                      "- h2: h2, database ejc_mcp_test, read-only\n"
                      "- pg: postgresql, host dbhost, port 5432, "
                      "database shop, read-only")
                 listing))
          (doseq [secret ["pw1" "pw2" "pw3" "u1" "a_user" "s3cret"
                          "DB_CLOSE_DELAY"]]
            (is (not (s/includes? listing secret)) secret)))))
    (testing "Error texts."
      (is (= "Login failed for *** at jdbc:x://u:***@h;password=***"
             (mcp/mask-secrets db (str "Login failed for pw2 at "
                                       "jdbc:x://u:pw3@h;password=pw1")))))
    (testing "Writable."
      (is (s/includes? (text (call "list_connections" {}))
                       "- h2-rw: h2, database ejc_mcp_test, writable")))))
