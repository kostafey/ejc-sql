(ns ejc-sql.deps-resolver-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [ejc-sql.deps-resolver :refer :all]))

(deftest get-dependeces-list-test
  (testing "get-dependeces-list fn test."
    (is (= '([org.mongodb/bson "3.8.0"] [com.ibm.informix/jdbc "4.50.3"])
           (get-dependeces-list '[[com.ibm.informix/jdbc "4.50.3"]])))
    (is (= '([org.mongodb/bson "3.8.0"])
           (get-dependeces-list '[[org.mongodb/bson "3.8.0"]])))
    (is (= '([mysql/mysql-connector-java "5.1.44"])
           (get-dependeces-list '[[mysql/mysql-connector-java "5.1.44"]])))))

(defn replace-to-tilda [path]
  (s/replace path (System/getProperty "user.home") "~"))

(defn resolve-to-tildas
  "Resolve ARTIFACTS and return their jar paths as a set of ~-prefixed strings.
  The order of the resolved files is defined by the dependencies graph
  traversal, so it is not a part of the contract."
  [artifacts]
  (set (map replace-to-tilda (get-dependeces-files-list artifacts))))

(deftest get-dependeces-files-list-test
  (testing "get-dependeces-files-list fn test."
    (is (= #{"~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"
             "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"}
           (resolve-to-tildas '[[com.ibm.informix/jdbc "4.50.3"]])))
    (is (= #{"~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"}
           (resolve-to-tildas '[[org.mongodb/bson "3.8.0"]])))
    (is (= #{"~/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"}
           (resolve-to-tildas '[[mysql/mysql-connector-java "5.1.44"]])))))

(deftest get-dependeces-files-list-error-test
  (testing "Unresolvable artifact turns into nil, not into an exception."
    (is (nil? (get-dependeces-files-list
               '[[ejc-sql/no-such-artifact-at-all "0.0.0"]])))))

(deftest path-to-artifact-test
  (testing "Coordinates from the ~/.m2/repository layout."
    (are [artifact path] (= artifact (path-to-artifact path))
      '[[org.xerial/sqlite-jdbc "3.23.1"]]
      "~/.m2/repository/org/xerial/sqlite-jdbc/3.23.1/sqlite-jdbc-3.23.1.jar"
      '[[com.h2database/h2 "1.4.199"]]
      "/home/me/.m2/repository/com/h2database/h2/1.4.199/h2-1.4.199.jar"
      '[[mysql/mysql-connector-java "5.1.44"]]
      "~/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"
      '[[postgresql/postgresql "9.3-1102.jdbc41"]]
      "~/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar"
      '[[com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"]]
      "C:\\Users\\me\\.m2\\repository\\com\\microsoft\\sqlserver\\mssql-jdbc\\6.2.2.jre8\\mssql-jdbc-6.2.2.jre8.jar"
      '[[com.oracle.jdbc/ojdbc8 "12.2.0.1"]]
      "~/.m2/repository/com/oracle/jdbc/ojdbc8/12.2.0.1/ojdbc8-12.2.0.1.jar"
      '[[com.ibm.informix/jdbc "4.50.3"]]
      "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"))
  (testing (str "There is nothing to derive the coordinates from when the jar "
                "file lies outside of the local maven repository, or too "
                "shallow inside it.")
    (is (nil? (path-to-artifact
               "/opt/cloudera-jdbc-drivers/lib/2.6.30.1036/ImpalaJDBC42-2.6.30.1036.jar")))
    (is (nil? (path-to-artifact "~/.m2/repository/ojdbc8.jar")))))

(deftest connection-jars-test
  (let [jar (str (System/getProperty "user.home")
                 "/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar")]
    (testing ":dependencies are resolved."
      (is (= #{"~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"
               "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"}
             (set (map replace-to-tilda
                       (connection-jars
                        {:dependencies '[[com.ibm.informix/jdbc "4.50.3"]]}))))))
    (testing "Unresolvable :dependencies throw: there is nothing to load."
      (is (thrown? Exception
                   (connection-jars
                    {:dependencies '[[ejc-sql/no-such-artifact-at-all "0.0.0"]]}))))
    (testing (str ":classpath, a path or a vector, ~ expanded, with the "
                  "dependencies of the jars in ~/.m2/repository.")
      (is (= [jar] (connection-jars {:classpath jar})))
      (is (= [jar] (connection-jars
                    {:classpath ["~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"]})))
      (is (= #{"~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"
               "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"}
             (set (map replace-to-tilda
                       (connection-jars
                        {:classpath ["~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar"]}))))))
    (testing "A :classpath jar is used as is when nothing can be resolved."
      (is (= ["/opt/drivers/ImpalaJDBC42.jar"
              (str (System/getProperty "user.home")
                   "/.m2/repository/ejc-sql/none/0.0.0/none-0.0.0.jar")]
             (connection-jars
              {:classpath ["/opt/drivers/ImpalaJDBC42.jar"
                           "~/.m2/repository/ejc-sql/none/0.0.0/none-0.0.0.jar"]}))))
    (testing "No jars."
      (is (empty? (connection-jars {:subprotocol "h2"}))))))
