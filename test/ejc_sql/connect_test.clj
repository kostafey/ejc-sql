(ns ejc-sql.connect-test
  (:require [clojure.test :refer :all]
            [ejc-sql.connect :refer :all]))

(deftest determine-dml-test
  (testing "determine-dml fn test."
    (is (= "SELECT"
           (determine-dml "select * from table")))
    (is (= "SELECT"
           (determine-dml "(select * from table)")))
    (is (= "SELECT"
           (determine-dml "-- comment
                           select * from table")))))
