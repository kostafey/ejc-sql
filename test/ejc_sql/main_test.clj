(ns ejc-sql.main-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer :all])
  (:import [java.io File]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(def classpath
  "The classpath of the tests."
  (s/split (System/getProperty "java.class.path")
           (re-pattern File/pathSeparator)))

(def h2-jar
  "The H2 jar of the `:test` profile."
  (first (filter #(re-find #"[/\\]h2-[^/\\]+\.jar$" %) classpath)))

(defn- start-standalone
  "Run `ejc-sql.main` with `config-file` in a JVM of its own, on the
  classpath of the tests but the H2 jar, which it has to load as a driver.
  Return [process output], `output` a promise of the lines it printed up to
  the server URL, all of them if it exited before."
  [config-file]
  (let [log-dir (System/getProperty "ejc-sql.log-dir")
        process (-> (ProcessBuilder.
                     (concat
                      [(str (io/file (System/getProperty "java.home") "bin" "java"))
                       "-cp" (s/join File/pathSeparator (remove #{h2-jar} classpath))]
                      ;; Keep the queries of the test off the user's log.
                      (when log-dir
                        [(str "-Dejc-sql.log-dir=" log-dir)])
                      ["clojure.main" "-m" "ejc-sql.main" (str config-file)]))
                    (.redirectErrorStream true)
                    (.start))
        output (promise)]
    (future
      (with-open [reader (io/reader (.getInputStream process))]
        (loop [lines []]
          (if-let [line (.readLine reader)]
            (let [lines (conj lines line)]
              (if (s/starts-with? line "ejc-sql MCP server: http")
                (deliver output lines)
                (recur lines)))
            (deliver output lines)))))
    [process output]))

(defn- call-tool
  "Call the MCP tool `tool-name` with `arguments` at `url`, return the text
  of the result."
  [url tool-name arguments]
  (let [request (-> (HttpRequest/newBuilder (URI. url))
                    (.header "Content-Type" "application/json")
                    (.header "Accept" "application/json, text/event-stream")
                    (.POST (HttpRequest$BodyPublishers/ofString
                            (json/write-str
                             {:jsonrpc "2.0" :id 1 :method "tools/call"
                              :params {:name tool-name
                                       :arguments arguments}})))
                    (.build))
        response (.send (HttpClient/newHttpClient) request
                        (HttpResponse$BodyHandlers/ofString))]
    (get-in (json/read-str (.body response))
            ["result" "content" 0 "text"])))

(deftest standalone-test
  (is h2-jar)
  (let [dir (.toFile (Files/createTempDirectory "ejc-sql-main"
                                                (make-array FileAttribute 0)))
        config-file (io/file dir "mcp.edn")]
    (spit config-file "{:port 0}")
    (spit (io/file dir "databases.edn")
          (pr-str {"h2" {:classpath h2-jar
                         :subprotocol "h2"
                         :subname "mem:ejc_main_test;DB_CLOSE_DELAY=-1"}
                   "broken" {:classname "no.such.Driver"
                             :subprotocol "h2"
                             :subname "mem:ejc_main_test_broken"}}))
    (let [[process output] (start-standalone config-file)]
      (try
        (let [lines (deref output 120000 ::timeout)
              url (some->> (if (vector? lines) (peek lines) "")
                           (re-find #"http://\S+"))]
          (testing "The drivers load before the server starts."
            (is (vector? lines))
            (is (some #{(str "broken: can't load the JDBC driver: "
                             "java.lang.ClassNotFoundException: no.such.Driver")}
                      lines)
                lines)
            (is url lines))
          (when url
            (testing "A driver added to the classpath at run time works."
              (is (s/includes? (call-tool url "list_connections" {})
                               "- h2: h2, database ejc_main_test, read-only"))
              (is (s/starts-with? (call-tool url "query" {"connection" "h2"
                                                          "sql" "SELECT 1 AS x"})
                                  "x\n-\n1\n\n1 row")))))
        (finally
          (.destroy process)
          (.waitFor process 10 TimeUnit/SECONDS))))))

(deftest usage-test
  (testing "No config file - the server is off."
    (let [dir (.toFile (Files/createTempDirectory "ejc-sql-main"
                                                  (make-array FileAttribute 0)))
          [process output] (start-standalone (io/file dir "mcp.edn"))]
      (is (some #{(format "No %s, the MCP server is turned off."
                          (io/file dir "mcp.edn"))}
                (deref output 120000 ::timeout)))
      (is (.waitFor process 10 TimeUnit/SECONDS))
      (is (= 1 (.exitValue process))))))
