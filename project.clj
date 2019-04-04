(defproject ejc-sql "0.2-SNAPSHOT"
  :description "Interact with database via java/clojure libs to run SQL/JPQL scripts"
  :url "https://github.com/kostafey/ejc-sql"
  :license {:name "GPL 2.0+"
            :url "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/java.jdbc "0.7.9"]
                 [org.hibernate/hibernate-core "5.4.1.Final"]
                 [com.cemerick/pomegranate "0.3.0"]
                 [org.apache.commons/commons-lang3 "3.5"]
                 [org.apache.httpcomponents/httpclient "4.5.5"]
                 [clomacs "0.0.3-SNAPSHOT"]])
