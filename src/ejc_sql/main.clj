;;; main.clj -- Run the ejc-sql MCP server without Emacs.

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

;;; The entry point of `lein uberjar': java -jar ejc-sql.jar [mcp.edn].
;;; The only namespace compiled ahead of time, see `-main'.

(ns ejc-sql.main
  (:gen-class))

(defn -main
  "Run the MCP server without Emacs, see `ejc-sql.mcp-http/run-standalone`.
  The JDBC drivers are added to the root class loader of Clojure at run
  time, and `DriverManager` hands them only to the code loaded below it,
  `clojure.java.jdbc` first of all. So the root is set before the rest of
  ejc-sql is loaded, and it is loaded by `requiring-resolve`: required here,
  it would be compiled ahead of time along with this namespace, into the
  classes of the application class loader, which is above the root."
  [& args]
  (if (next args)
    (do
      (binding [*out* *err*]
        (println "Usage: java -jar ejc-sql.jar [mcp.edn]"))
      (System/exit 2))
    (do
      ((requiring-resolve 'ejc-sql.classpath/ensure-compiler-loader))
      (System/exit ((requiring-resolve 'ejc-sql.mcp-http/run-standalone)
                    (first args))))))
