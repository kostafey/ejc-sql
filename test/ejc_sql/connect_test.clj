(ns ejc-sql.connect-test
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [ejc-sql.connect :refer :all]
            [ejc-sql.lib :refer :all]))

(deftest determine-dml-test
  (testing "dml? fn test."
    (is (= "SELECT"
           (dml? "select * from table")))
    (is (= "SELECT"
           (dml? "(select * from table)")))
    (is (= "SELECT"
           (dml? "-- comment
                  select * from table")))))

(deftest sql-statement-separators-test
  (testing "get-separator-re fn test."
    (is (= (list
            (str """
select * from table""")
            (str """
select * from urls where id=1""")
            (str """ -- http://localhost:8080
select * from urls"""))
           (seq (s/split
                 (str """
select * from table;
select * from urls where id=1; -- http://localhost:8080
select * from urls;""")
                 (get-separator-re ";")))))

    (is (= (list
            (str """
select * from table; """)
            (str """
select * from urls where id=1; -- http://localhost:8080
select * from urls;"""))
           (seq
            (s/split
             (str """
select * from table; /
select * from urls where id=1; -- http://localhost:8080
select * from urls;""")
             (get-separator-re "/")))))

    (is (= (list
            (str "  -- http://localhost:8080\n"
                 "select * from urls"))
           (seq (s/split (str "  -- http://localhost:8080\n"
                             "select * from urls")
                        (get-separator-re "/")))))
    (is (= (list
            (str "-- http://localhost:8080\n"
                 "select * from urls"))
           (seq (s/split (str "-- http://localhost:8080\n"
                             "select * from urls")
                        (get-separator-re "/")))))
    (is (= '("USE testdb; "
             "CREATE TABLE customer (id INT);")
           (seq (s/split "USE testdb; /CREATE TABLE customer (id INT);"
                        (get-separator-re "/")))))
    (is (= '("USE testdb "
             "CREATE TABLE customer (id INT)")
           (seq (s/split "USE testdb ;CREATE TABLE customer (id INT);"
                        (get-separator-re ";")))))
    (is (= '("SELECT * FROM some_table "
             "SELECT * FROM other_table")
           (seq (s/split "SELECT * FROM some_table ;SELECT * FROM other_table"
                        (get-separator-re ";")))))
    (is (= '("SELECT * FROM some_table "
             "SELECT * FROM other_table")
           (seq (s/split "SELECT * FROM some_table /SELECT * FROM other_table"
                        (get-separator-re "/")))))
    (is (= '("SELECT * FROM urls WHERE path like '%http://localhost%'")
           (seq (s/split (str "SELECT * FROM urls "
                             "WHERE path like '%http://localhost%'")
                        (get-separator-re "/")))))
    (is (= '("text1" "'te/xt2'" "\"te/xt3\"" "text4")
           (seq (s/split "text1/'te/xt2'/\"te/xt3\"/text4"
                        (get-separator-re "/")))))))
