;;; deps_resolver.clj -- Discover required jar artifacts.

;;; Copyright © 2020-2026 - Kostafey <kostafey@gmail.com>

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

(ns ejc-sql.deps-resolver
  (:require
   [cemerick.pomegranate.aether :as aether]
   [clojure.string :as s]
   [ejc-sql.classpath :as classpath]
   [leiningen.core.project :refer [default-repositories]]))

(defn get-hierarchy [artifacts-list]
  (aether/dependency-hierarchy
   artifacts-list
   (aether/resolve-dependencies
    :coordinates artifacts-list
    :repositories default-repositories)))

(def artifacts (atom (list)))

(defn art-wanderer [art]
  (if (and
       (coll? art)
       (vector? art)
       (not (coll? (nth art 0))))
    (swap! artifacts conj art)
    (dorun (map art-wanderer art))))

(defn run-wanderer [art]
  (reset! artifacts (list))
  (dorun (map art-wanderer art))
  @artifacts)

(defn get-dependeces-list
  "Get artifacts dependeces hierarchy as a flatten list."
  [artifacts-list]
  (-> artifacts-list get-hierarchy run-wanderer))

(defn dependency-files
  "Resolve the leiningen dependencies vector `artifacts-list`, e.g.
  '[[com.ibm.informix/jdbc \"4.50.3\"]], and return the paths of all the jar
  files required. The whole dependencies graph is obtained by the single
  `resolve-dependencies` call, so the remote repositories are contacted at
  most once per connection. Throw if it can't be resolved."
  [artifacts-list]
  (doall
   (map
    (fn [^java.io.File dep-file] (.getPath dep-file))
    (aether/dependency-files
     (aether/resolve-dependencies
      :coordinates artifacts-list
      :repositories default-repositories)))))

(defn get-dependeces-files-list
  "`dependency-files`, or nil if they can't be resolved."
  [artifacts-list]
  (try
    (dependency-files artifacts-list)
    (catch Exception _
      nil)))

(defn path-to-artifact
  "The leiningen dependencies vector, e.g. '[[com.h2database/h2 \"2.5.252\"]],
  of the jar file `path` in the local maven repository. nil when `path` is
  outside of it, or too short to be an artifact inside it: the coordinates
  are derived from the ~/.m2/repository directory layout, so there is
  nothing to derive them from in this case."
  [path]
  (when-let [[_ in-repository] (re-find #"\.m2/repository/(.+)"
                                        (s/replace path "\\" "/"))]
    (let [elements (s/split in-repository #"/")]
      ;; The shortest possible layout is group/name/version/file.
      (when (> (count elements) 3)
        (let [[name version] (take-last 3 elements)]
          [[(symbol (s/join "." (drop-last 3 elements)) name) version]])))))

(defn- expand-home
  "Expand the leading `~` of `path`, as written in `databases.edn`, to the
  home directory of Java."
  [path]
  (if (re-find #"^~[/\\]" path)
    (str (System/getProperty "user.home") (subs path 1))
    path))

(defn connection-jars
  "The jar files the JDBC driver of the db-spec `db` needs. Its
  `:dependencies` resolved, or else its `:classpath`, a path or a vector of
  them, together with the dependencies of these jars, when they are in
  ~/.m2/repository and their coordinates can be resolved. Throw if
  `:dependencies` can't be resolved: unlike a `:classpath` jar, which may
  need nothing else, there is nothing to load then."
  [db]
  (if-let [dependencies (:dependencies db)]
    (dependency-files dependencies)
    (let [classpath (:classpath db)
          jars (map expand-home (if (string? classpath)
                                  [classpath]
                                  classpath))]
      (distinct
       (concat (mapcat #(some-> % path-to-artifact get-dependeces-files-list)
                       jars)
               jars)))))

(defn load-driver!
  "Add the `connection-jars` of the db-spec `db` to the classpath and load
  its `:classname`, which registers the driver with `DriverManager`. The
  jars go to the hierarchy of `Compiler/LOADER`: `DriverManager` hands a
  driver only to the code whose class loader can load the driver class, so
  this has to be a class loader `clojure.java.jdbc` was loaded below."
  [db]
  (run! classpath/add-classpath (connection-jars db))
  (when-let [classname (:classname db)]
    (Class/forName classname true (clojure.lang.RT/baseLoader)))
  nil)
