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
