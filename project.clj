(defproject ejc-sql "0.3.3-SNAPSHOT"
  :description "Interact with database via java/clojure libs to run SQL scripts"
  :url "https://github.com/kostafey/ejc-sql"
  :license {:name "GPL 2.0+"
            :url "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojars.kostafey/java.jdbc "0.7.10-SNAPSHOT"]
                 [org.hibernate/hibernate-core "5.4.1.Final"]
                 [com.cemerick/pomegranate "1.1.0"]
                 [org.slf4j/slf4j-simple "1.7.26"]
                 [org.apache.commons/commons-lang3 "3.5"]
                 [org.apache.httpcomponents/httpclient "4.5.5"]
                 [clomacs "0.0.4-SNAPSHOT"]])
