(ns ejc-sql.clojure-offline
  (:use [clojure.string :only (join split)])
  (:import (java.io StringWriter File)
           (java.net URL URLClassLoader)
           (java.lang.reflect Method)))

(defn parse-artifact [artifact-name]
  "Parse `artifact-name' to list (`group-id' `artifact-id' `version')
Input format, e.g.:
 [org.clojure/clojure \"1.5.1\"]
Ouptut format, e.g.:
 (\"org.clojure\" \"clojure\" \"1.5.1\")"
  (let [group-and-artifact (split (str (first artifact-name)) #"/")
        group-id (first group-and-artifact)
        artifact-id (if (nil? (second group-and-artifact))
                      (first group-and-artifact)
                      (second group-and-artifact))
        version (second artifact-name)]
    (list group-id artifact-id version)))

(defmacro with-artifact [artifact-name & body]
  "Inject `group-id' `artifact-id' `version' local variables to the `body'
scope."
  `(let [artifact# (parse-artifact ~artifact-name)
         ~(symbol "group-id") (nth artifact# 0)
         ~(symbol "artifact-id") (nth artifact# 1)
         ~(symbol "version") (nth artifact# 2)]
     ~@body))

(defn get-path-tail [path]
  (.getName (File. path)))

(defn get-path-parent [path]
  (.getParent (File. path)))

(defn concat-path [& path-list]
  (let [path-cons (fn [& path-list]
                    (loop [acc (File. (first path-list))
                           pl (rest path-list)]
                      (if (empty? pl)
                        acc
                        (recur (File. acc (first pl)) (rest pl)))
                      ))]
    (.getPath (apply path-cons path-list))))

(def is-windows
  "The value is true if it runs under the os Windows."
  (<= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(def is-linux
  "The value is true if it runs under the os Linux."
  (<= 0 (.indexOf (System/getProperty "os.name") "Linux")))

(defn get-m2-path [artifact-name]
  (with-artifact
    artifact-name
    (let [home (if (= (get-path-tail (System/getenv "HOME")) "Application Data")
                 (get-path-parent (System/getenv "HOME"))
                 (System/getenv "HOME"))
          m2 (concat-path home ".m2" "repository")
          sep (if is-windows "\\\\" "/")]
      (concat-path m2
                   (.replaceAll group-id "\\." sep)
                   artifact-id
                   version "/"))))

(defn get-artifact-file-name [artifact-name extension]
  (with-artifact
    artifact-name
    (str artifact-id "-" version "." extension)))

(defn get-jar-location [artifact-name]
  (str (get-m2-path artifact-name)
       (get-artifact-file-name artifact-name "jar")))

(defn add-to-cp "Since add-classpath is deprecated."
  [#^String jarpath] ; path without "file:///..." prefix.
  (let [#^URL url (.. (File. jarpath) toURI toURL)
        url-ldr-cls (. (URLClassLoader. (into-array URL [])) getClass)
        arr-cls (into-array Class [(. url getClass)])
        arr-obj (into-array Object [url])
        #^Method mthd (. url-ldr-cls getDeclaredMethod "addURL" arr-cls)]
    (doto mthd
      (.setAccessible true)
      (.invoke (ClassLoader/getSystemClassLoader) arr-obj))
    (println (format "Added %s to classpath" jarpath))))

(defn print-cp []
  (doseq [url (seq
               (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))]
    (println (.getFile url))))

(comment
  (get-jar-location '[org.clojure/clojure-contrib "1.2.0"])
  (add-to-cp (get-jar-location '[org.clojure/clojure-contrib "1.2.0"]))

  (print-cp)
  ;; (add-to-cp (.replaceAll (get-jar-location '[org.clojure/clojure-contrib "1.2.0"]) "\\\\" "/"))

  ;; (require 'cemerick.pomegranate :as pom)
  ;; (pom/add-classpath "/home/user/~.m2/....")
  )
