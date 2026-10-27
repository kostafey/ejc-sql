(ns ejc-sql.mcp-http-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer :all]
            [ejc-sql.connect :as c]
            [ejc-sql.mcp :as mcp]
            [ejc-sql.mcp-http :as http])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

(def config-file
  (doto (java.io.File/createTempFile "ejc-sql-mcp" ".edn")
    (.deleteOnExit)))

(def h2-db
  {:classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "mem:ejc_mcp_http_test;DB_CLOSE_DELAY=-1"})

(def reports
  "Messages `http/report!` was asked to show in Emacs."
  (atom []))

(defn- fixture
  "Run the test `f` with a config file of its own, one H2 connection
  registered and the messages to Emacs recorded. Stop the server after it."
  [f]
  (spit config-file "{:port 0}")
  (reset! reports [])
  (with-redefs [mcp/config-file config-file
                mcp/registry (atom {"h2" h2-db})
                http/report! #(swap! reports conj %)]
    (try
      (f)
      (finally
        (http/stop!)))))

(use-fixtures :each fixture)

(def json-headers {"content-type" "application/json"})

(defn- post
  "POST `message`, a map or a raw JSON string, to `http/handle-request`."
  [message & [headers]]
  (http/handle-request {:method "POST"
                        :path "/mcp"
                        :headers (merge json-headers headers)
                        :body (if (string? message)
                                message
                                (json/write-str message))}))

(defn- parse [response]
  (json/read-str (:body response) :key-fn keyword))

(def initialize
  {:jsonrpc "2.0" :id 1 :method "initialize"
   :params {:protocolVersion "2025-06-18"
            :capabilities {}
            :clientInfo {:name "test" :version "0"}}})

(defn- execute [sql]
  {:jsonrpc "2.0" :id 2 :method "tools/call"
   :params {:name "execute" :arguments {:connection "h2" :sql sql}}})

(deftest handle-request-test
  (testing "A JSON-RPC round trip."
    (let [response (post initialize)]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (is (= "2025-06-18" (get-in (parse response) [:result :protocolVersion])))))
  (testing "Notifications."
    (is (= {:status 202 :headers {} :body nil}
           (post {:jsonrpc "2.0" :method "notifications/initialized"}))))
  (testing "Transport errors."
    (is (= 404 (:status (http/handle-request {:method "POST" :path "/other"
                                              :headers json-headers
                                              :body "{}"}))))
    (doseq [method ["GET" "DELETE"]]
      (let [response (http/handle-request {:method method :path "/mcp"
                                           :headers {} :body ""})]
        (is (= 405 (:status response)))
        (is (= "POST" (get-in response [:headers "Allow"])))))
    (is (= 415 (:status (post initialize {"content-type" "text/plain"}))))
    (is (= 200 (:status (post initialize {"content-type"
                                          "application/json; charset=utf-8"}))))
    (is (= 413 (:status (http/handle-request {:method "POST" :path "/mcp"
                                              :headers json-headers
                                              :body nil}))))
    (let [response (post "{\"jsonrpc\": ")]
      (is (= 400 (:status response)))
      (is (= -32700 (get-in (parse response) [:error :code]))))
    (let [response (post [initialize])]
      (is (= 400 (:status response)))
      (is (= -32600 (get-in (parse response) [:error :code])))))
  (testing "Origin."
    (doseq [origin ["http://evil.example" "http://localhost.evil.example"
                    "null" "file://localhost"]]
      (is (= 403 (:status (post initialize {"origin" origin}))) origin))
    (doseq [origin ["http://localhost:3000" "https://127.0.0.1" "http://[::1]:80"]]
      (is (= 200 (:status (post initialize {"origin" origin}))) origin))))

(deftest config-test
  (testing "The file gone - 503, the kill switch."
    (.delete config-file)
    (is (= 503 (:status (post initialize)))))
  (testing "Malformed - 500."
    (doseq [text ["{:port" "[1 2]" "{:token 42}" "{:writable \"h2\"}"
                  "{:port 99999}"]]
      (spit config-file text)
      (let [response (post initialize)]
        (is (= 500 (:status response)) text)
        (is (s/starts-with? (:body response) "Malformed") text))))
  (testing "Empty means defaults."
    (spit config-file "")
    (is (= 200 (:status (post initialize)))))
  (testing ":token is read on every request."
    (spit config-file "{:token \"abc\"}")
    (is (= 401 (:status (post initialize))))
    (is (= "Bearer" (get-in (post initialize) [:headers "WWW-Authenticate"])))
    (is (= 401 (:status (post initialize {"authorization" "Bearer abd"}))))
    (is (= 401 (:status (post initialize {"authorization" "Basic abc"}))))
    (is (= 200 (:status (post initialize {"authorization" "Bearer abc"}))))
    (spit config-file "{:token \"xyz\"}")
    (is (= 401 (:status (post initialize {"authorization" "Bearer abc"})))))
  (testing ":writable is read on every request."
    (spit config-file "{:writable #{\"h2\"}}")
    (is (false? (get-in (parse (post (execute "CREATE TABLE t (id INT)")))
                        [:result :isError])))
    (spit config-file "{:writable #{}}")
    (is (true? (get-in (parse (post (execute "DROP TABLE t")))
                       [:result :isError])))))

(defn- send-request
  "Send an HTTP request to the server listening on `port`."
  [port method & {:keys [path body headers]
                  :or {path "/mcp"}}]
  (let [builder (reduce (fn [b [k v]] (.header b k v))
                        (-> (HttpRequest/newBuilder
                             (URI. (str "http://127.0.0.1:" port path)))
                            (.method method
                                     (if body
                                       (HttpRequest$BodyPublishers/ofString body)
                                       (HttpRequest$BodyPublishers/noBody))))
                        headers)
        response (.send (HttpClient/newHttpClient) (.build builder)
                        (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response)
     :body (.body response)}))

(deftest http-server-test
  (let [port (http/start! {:port 0})
        mcp-headers {"Content-Type" "application/json"
                     "Accept" "application/json, text/event-stream"}]
    (testing "start! is idempotent."
      (is (http/running?))
      (is (= port (http/start! {:port 0}))))
    (testing "A POST round trip."
      (let [{:keys [status body]} (send-request port "POST"
                                                :body (json/write-str initialize)
                                                :headers mcp-headers)]
        (is (= 200 status))
        (is (= "ejc-sql" (get-in (json/read-str body)
                                 ["result" "serverInfo" "name"])))))
    (testing "Modern revision, non-ASCII text."
      (let [{:keys [status body]}
            (send-request
             port "POST"
             :body (json/write-str
                    {:jsonrpc "2.0" :id 3 :method "tools/call"
                     :params {:name "query"
                              :arguments {:connection "h2"
                                          :sql "SELECT 'Привет' AS x"}
                              :_meta {"io.modelcontextprotocol/protocolVersion"
                                      "2026-07-28"}}})
             :headers (assoc mcp-headers
                             "MCP-Protocol-Version" "2026-07-28"
                             "Mcp-Method" "tools/call"
                             "Mcp-Name" "query"))]
        (is (= 200 status))
        (is (s/includes? (get-in (json/read-str body)
                                 ["result" "content" 0 "text"])
                         "| Привет |"))))
    (testing "Statuses."
      (is (= 202 (:status (send-request
                           port "POST"
                           :body (json/write-str
                                  {:jsonrpc "2.0"
                                   :method "notifications/initialized"})
                           :headers mcp-headers))))
      (is (= 405 (:status (send-request port "GET"))))
      (is (= 405 (:status (send-request port "DELETE"))))
      (is (= 403 (:status (send-request port "POST"
                                        :body (json/write-str initialize)
                                        :headers (assoc mcp-headers "Origin"
                                                        "http://evil.example")))))
      (is (= 415 (:status (send-request port "POST"
                                        :body (json/write-str initialize)
                                        :headers {"Content-Type" "text/plain"}))))
      (is (= 404 (:status (send-request port "POST" :path "/other"
                                        :body (json/write-str initialize)
                                        :headers mcp-headers))))
      (is (= 400 (:status (send-request port "POST"
                                        :body (json/write-str initialize)
                                        :headers (assoc mcp-headers
                                                        "MCP-Protocol-Version"
                                                        "1900-01-01")))))
      (spit config-file "{:token \"abc\"}")
      (is (= 401 (:status (send-request port "POST"
                                        :body (json/write-str initialize)
                                        :headers mcp-headers))))
      (is (= 200 (:status (send-request port "POST"
                                        :body (json/write-str initialize)
                                        :headers (assoc mcp-headers
                                                        "Authorization"
                                                        "Bearer abc"))))))
    (testing "status."
      (is (= {:running true :port port :connections ["h2"]}
             (dissoc (http/status) :requests)))
      (is (pos? (:requests (http/status)))))
    (testing "stop!."
      (http/stop!)
      (is (not (http/running?))))))

(deftest set-db-test
  (testing "Without a name nothing is registered or started."
    (is (= h2-db (c/set-db h2-db)))
    (is (= ["h2"] (mcp/connection-names)))
    (is (not (http/running?))))
  (testing "With a name the connection is registered, the server started."
    (let [other (assoc h2-db :subname "mem:other")]
      (is (= other (c/set-db other "other")))
      (is (= other @c/db))
      (is (= ["h2" "other"] (mcp/connection-names)))
      (is (= other (get (mcp/connections) "other"))))
    (is (http/running?))
    (is (= 1 (count @reports)))
    (is (re-matches #"ejc-sql MCP server: http://127\.0\.0\.1:\d+/mcp"
                    (first @reports))))
  (testing "Once."
    (c/set-db h2-db "h2")
    (is (= 1 (count @reports))))
  (testing "Re-registering replaces the spec."
    (c/set-db (assoc h2-db :user "u") "h2")
    (is (= "u" (:user (get (mcp/connections) "h2")))))
  (testing "No file - registered, not started."
    (http/stop!)
    (.delete config-file)
    (c/set-db h2-db "third")
    (is (= ["h2" "other" "third"] (mcp/connection-names)))
    (is (not (http/running?))))
  (testing "A failing start doesn't fail set-db."
    (spit config-file "{:port")
    (is (= h2-db (c/set-db h2-db "fourth")))
    (is (s/starts-with? (last @reports) "ejc-sql MCP server is not started: Malformed"))
    (spit config-file "{}")
    (with-redefs [http/start! (fn [_] (throw (java.net.BindException.
                                              "Address already in use")))]
      (is (= h2-db (c/set-db h2-db "fifth"))))
    (is (= (format "ejc-sql MCP server can't listen on 127.0.0.1:%d: Address already in use"
                   http/default-port)
           (last @reports)))
    (let [err (java.io.StringWriter.)]
      (with-redefs [http/connected! (fn [& _] (throw (Exception. "boom")))]
        (binding [*err* err]
          (is (= h2-db (c/set-db h2-db "sixth")))))
      (is (s/starts-with? (str err) "ejc-sql MCP server: ")))))

(def original-report!
  "`http/report!`, which the fixture replaces."
  http/report!)

(deftest report-test
  (testing "Quotes and backslashes don't reach Elisp."
    (let [sent (promise)]
      (with-redefs [http/emacs-message (fn [& args] (deliver sent args))]
        (original-report! "a \"b\" \\c %s")
        (is (= ["%s" "a b c %s"] (deref sent 1000 :timeout)))))))
