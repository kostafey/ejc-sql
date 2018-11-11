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

(deftest sql-statement-separators-test
  (testing "get-separator-re fn test."
    (is (= '("SELECT * FROM some_table "
             "SELECT * FROM other_table")
           (seq (.split "SELECT * FROM some_table /SELECT * FROM other_table"
                        (get-separator-re "/")))))
    (is (= '("SELECT * FROM urls WHERE path like '%http://localhost%'")
           (seq (.split (str "SELECT * FROM urls "
                             "WHERE path like '%http://localhost%'")
                        (get-separator-re "/")))))
    (is (= '("text1" "'te/xt2'" "\"te/xt3\"" "text4")
           (seq (.split "text1/'te/xt2'/\"te/xt3\"/text4"
                        (get-separator-re "/")))))))
