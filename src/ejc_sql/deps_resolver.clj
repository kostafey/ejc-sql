;;; deps_resolver.clj -- Discover required jar artifacts.

;;; Copyright © 2020 - Kostafey <kostafey@gmail.com>

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
  (:use
   [leiningen.core.project :only [default-repositories]])
  (:require
   [cemerick.pomegranate.aether :as aether]))

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

(defn get-dependeces-files-list
  "Resolve dependeces and return a list of all requred jar files paths.
  `artifacts-list` is a leningen dependeces vector.
  E.g. '[[com.ibm.informix/jdbc \"4.50.3\"]]."
  [artifacts-list]
  (try
    (map
     (fn [dep-file] (.getPath dep-file))
     (flatten
      (map
       aether/dependency-files
       (map
        (fn [dep] (get-hierarchy (vector dep)))
        (get-dependeces-list artifacts-list)))))
    (catch Exception e
      nil)))
