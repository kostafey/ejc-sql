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

(deftest get-dependeces-files-list-test
  (testing "get-dependeces-list fn test."
    (is (= '("~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar"
             "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/jdbc-4.50.3.jar")
           (map replace-to-tilda
                (get-dependeces-files-list
                 '[[com.ibm.informix/jdbc "4.50.3"]]))))
    (is (= '("~/.m2/repository/org/mongodb/bson/3.8.0/bson-3.8.0.jar")
           (map replace-to-tilda
                (get-dependeces-files-list
                 '[[org.mongodb/bson "3.8.0"]]))))
    (is (= '("~/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar")
           (map replace-to-tilda
                (get-dependeces-files-list
                 '[[mysql/mysql-connector-java "5.1.44"]]))))))
