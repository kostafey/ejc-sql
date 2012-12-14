(defproject ejc-sql "0.1.0-SNAPSHOT"
  :description "Simple command-line jdbc client"
  :url "http://example.com/FIXME"
  :license {:name "GPL 2.0+"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [ifxjdbc/ifxjdbcx "1.0"]
                 [ifxjdbc/ifxjdbc "1.0"]])