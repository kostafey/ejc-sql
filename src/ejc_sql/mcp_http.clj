;;; mcp_http.clj -- MCP Streamable HTTP transport of ejc-sql.

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

;;; Serve `ejc-sql.mcp' over MCP Streamable HTTP on 127.0.0.1: stateless,
;;; JSON responses only, no SSE streams, no sessions. The server is started
;;; by `ejc-sql.connect/set-db' when `ejc-sql.mcp/config-file' exists and
;;; dies with the JVM.

(ns ejc-sql.mcp-http
  (:require [clojure.data.json :as json]
            [clojure.string :as s]
            [clomacs :refer [clomacs-defn]]
            [ejc-sql.mcp :as mcp])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io InputStream]
           [java.net BindException InetAddress InetSocketAddress URI]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util.concurrent ExecutorService Executors ThreadFactory]))

(def host
  "The only address the server listens on."
  "127.0.0.1")

(def endpoint
  "The MCP endpoint path."
  "/mcp")

(def default-port 8765)

(def max-body-size
  "The largest request body accepted, in bytes."
  (* 16 1024 1024))

(def local-hosts #{"localhost" "127.0.0.1" "[::1]"})

(defn- local-origin?
  "Check the `Origin` header value against DNS rebinding: only a page served
  from this machine may call the server."
  [origin]
  (try
    (let [uri (URI. origin)]
      (boolean (and (#{"http" "https"} (.getScheme uri))
                    (local-hosts (.getHost uri)))))
    (catch Exception _ false)))

(defn- authorized?
  "Check the `Authorization` header value against the configured `token`."
  [token authorization]
  (or (nil? token)
      (when-let [[_ given] (and authorization
                                (re-matches #"(?i)bearer\s+(.+)" authorization))]
        (MessageDigest/isEqual (.getBytes ^String token StandardCharsets/UTF_8)
                               (.getBytes ^String given StandardCharsets/UTF_8)))))

(defn- json-content-type?
  "Check if the `Content-Type` header value is JSON, parameters allowed."
  [content-type]
  (boolean (and content-type
                (re-matches #"(?i)application/json\s*(;.*)?" content-type))))

(defn- text-response [status text]
  {:status status
   :headers {"Content-Type" "text/plain; charset=utf-8"}
   :body text})

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/write-str body :escape-slash false :escape-unicode false)})

(defn- rpc-response
  "Answer the JSON-RPC message in the request `body` with `mcp/handle-rpc`.
  What it doesn't take, invalid JSON and batches, is rejected here."
  [headers body config]
  (let [message (try
                  (json/read-str body)
                  (catch Exception _ ::parse-error))]
    (cond
      (= message ::parse-error)
      (json-response 400 (mcp/rpc-error nil -32700 "Parse error"))
      (sequential? message)
      (json-response 400 (mcp/rpc-error nil -32600
                                        "Invalid Request: batches are not supported"))
      :else
      (let [{:keys [status body]} (mcp/handle-rpc headers message config)]
        (if body
          (json-response status body)
          {:status status :headers {} :body nil})))))

(defn handle-request
  "Answer the HTTP request {:method, :path, :headers (lower-case names),
  :body string} with {:status, :headers, :body string-or-nil}.
  `mcp/config-file` is read on every request, so a change of `:token` or
  `:writable` takes effect at once, and with the file gone the server
  answers 503 to everything."
  [{:keys [method path headers body]}]
  (let [origin (get headers "origin")]
    (cond
      (and origin (not (local-origin? origin)))
      (text-response 403 "Forbidden: the Origin is not local.")
      (not= path endpoint)
      (text-response 404 "Not found.")
      :else
      (let [config (try
                     (mcp/read-config)
                     (catch clojure.lang.ExceptionInfo e e))]
        (cond
          (nil? config)
          (text-response 503 "The ejc-sql MCP server is turned off by the user.")
          (instance? Exception config)
          (text-response 500 (.getMessage ^Exception config))
          (not (authorized? (:token config) (get headers "authorization")))
          (assoc-in (text-response 401 "Unauthorized.")
                    [:headers "WWW-Authenticate"] "Bearer")
          (not= method "POST")
          (assoc-in (text-response 405 "Method not allowed: no SSE streams, POST only.")
                    [:headers "Allow"] "POST")
          (not (json-content-type? (get headers "content-type")))
          (text-response 415 "Content-Type must be application/json.")
          (nil? body)
          (text-response 413 (format "The request body exceeds %d bytes."
                                     max-body-size))
          :else
          (rpc-response headers body config))))))

(defn- read-body
  "The request body as a string, nil if it exceeds `max-body-size`."
  [^HttpExchange exchange]
  (with-open [^InputStream in (.getRequestBody exchange)]
    (let [bytes (.readNBytes in (inc max-body-size))]
      (when (<= (alength bytes) max-body-size)
        (String. bytes StandardCharsets/UTF_8)))))

(defn- exchange->request
  "The request of `exchange`, as `handle-request` takes it."
  [^HttpExchange exchange]
  {:method (.getRequestMethod exchange)
   :path (.getPath (.getRequestURI exchange))
   :headers (into {}
                  (for [[k vs] (.getRequestHeaders exchange)]
                    [(s/lower-case k) (first vs)]))
   :body (read-body exchange)})

(defn- respond!
  "Send the response, as `handle-request` returns it, through `exchange`."
  [^HttpExchange exchange {:keys [status headers body]}]
  (let [^bytes bytes (when (seq body)
                       (.getBytes ^String body StandardCharsets/UTF_8))
        response-headers (.getResponseHeaders exchange)]
    (doseq [[k v] headers]
      (.set response-headers k v))
    (.sendResponseHeaders exchange status (if bytes (alength bytes) -1))
    (when bytes
      (with-open [out (.getResponseBody exchange)]
        (.write out bytes)))))

(def request-count
  "Requests served since the JVM started."
  (atom 0))

(defn- handler
  "The server's only handler: `handle-request` answers every path, an
  unexpected exception is answered with 500."
  []
  (reify HttpHandler
    (handle [_ exchange]
      (swap! request-count inc)
      (try
        (respond! exchange (handle-request (exchange->request exchange)))
        (catch Throwable e
          (try
            (respond! exchange (text-response 500 (str e)))
            (catch Throwable _)))
        (finally
          (.close exchange))))))

(defn- daemon-thread-factory
  "Threads for the server, which don't keep the JVM from exiting."
  []
  (let [n (atom 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable (str "ejc-sql-mcp-" (swap! n inc)))
          (.setDaemon true))))))

(defonce ^{:doc "The running server: {:server, :executor, :port}, or nil."
           :private true}
  state
  (atom nil))

(defn running?
  "Check if the MCP server is running."
  []
  (boolean @state))

(defn start!
  "Start the MCP server on `host`:`port` (0 - any free port) unless it is
  running already. Return the port it listens on."
  [{:keys [port]}]
  (locking state
    (if-let [running @state]
      (:port running)
      (let [^ExecutorService executor (Executors/newFixedThreadPool
                                       4 (daemon-thread-factory))
            ^HttpServer server
            (try
              (HttpServer/create
               (InetSocketAddress. (InetAddress/getByName host)
                                   (int (or port default-port)))
               0)
              (catch Throwable e
                (.shutdownNow executor)
                (throw e)))]
        (.createContext server "/" (handler))
        (.setExecutor server executor)
        (.start server)
        (let [port (.getPort (.getAddress server))]
          (reset! state {:server server :executor executor :port port})
          port)))))

(defn stop!
  "Stop the MCP server if it's running."
  []
  (locking state
    (when-let [{:keys [^HttpServer server ^ExecutorService executor]} @state]
      (.stop server 0)
      (.shutdownNow executor)
      (reset! state nil))))

(defn status
  "The MCP server state, for the REPL."
  []
  {:running (running?)
   :port (:port @state)
   :connections (vec (mcp/connection-names))
   :requests @request-count})

(clomacs-defn emacs-message message
              :doc "Call Emacs `message`.")

(defn report!
  "Show `text` in Emacs' echo area and *Messages*, best effort, in the
  background."
  [text]
  (future
    (try
      ;; clomacs passes strings to Elisp as is, unescaped.
      (emacs-message "%s" (s/replace text #"[\"\\]" ""))
      (catch Throwable _))))

(defn connected!
  "Called by `ejc-sql.connect/set-db` when the user connects to the db-spec
  `db` named `connection-name` in Emacs: register it for the agents and,
  when `mcp/config-file` exists, start the server unless it's running.
  Report the start, or why it failed, to Emacs. Never throw."
  [connection-name db]
  (try
    (mcp/register-db! connection-name db)
    (when-not (running?)
      (when-let [config (mcp/read-config)]
        (let [port (or (:port config) default-port)]
          (try
            (report! (format "ejc-sql MCP server: http://%s:%d%s"
                             host (start! {:port port}) endpoint))
            (catch BindException e
              (report! (format "ejc-sql MCP server can't listen on %s:%d: %s"
                               host port (.getMessage e))))))))
    (catch Throwable e
      (report! (str "ejc-sql MCP server is not started: "
                    (or (.getMessage e) e)))))
  nil)
