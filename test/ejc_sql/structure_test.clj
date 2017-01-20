(ns ejc-sql.structure-test
  (:require [clojure.test :refer :all]
            [ejc-sql.structure :refer :all]))

(deftest get-db-name-test
  (testing "get-db-name fn test."
    (is (= "my_db_name"
           (let [db {:classname "net.sourceforge.jtds.jdbc.Driver",
                     :connection-uri
                     (str "jdbc:sqlserver://localhost\\instance:1433;"
                          "databaseName=my_db_name;"
                          "user=a_user;"
                          "password=secret;")}]
             (get-db-name (:subname db)
                          (:connection-uri db)))))))
