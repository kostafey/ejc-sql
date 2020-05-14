(defproject ejc-sql "0.4.0-SNAPSHOT"
  :description "Interact with database via java/clojure libs to run SQL scripts"
  :url "https://github.com/kostafey/ejc-sql"
  :license {:name "GPL 2.0+"
            :url "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/java.jdbc "0.7.10"]
                 [org.hibernate/hibernate-core "5.4.1.Final"]
                 [com.cemerick/pomegranate "1.1.0"]
                 [leiningen-core "2.9.3"]
                 [org.apache.commons/commons-lang3 "3.5"]
                 [org.apache.httpcomponents/httpclient "4.5.5"]
                 [clomacs "0.0.4-SNAPSHOT"]
                 [com.github.kostafey/ascii-graphs_2.13 "0.0.6"]])
