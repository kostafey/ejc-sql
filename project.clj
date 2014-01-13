(defproject ejc-sql "0.1.0-SNAPSHOT"
  :description "Interact with database via java/clojure libs to run SQL/JPQL scripts"
  :url "https://github.com/kostafey/ejc-sql"
  :license {:name "GPL 2.0+"
            :url "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html"}
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/java.jdbc "0.3.0-beta1"]
                 [org.apache.openjpa/openjpa-all "2.2.2"]
                 [com.cemerick/pomegranate "0.2.0"]])
