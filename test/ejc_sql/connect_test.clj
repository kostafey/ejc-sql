(ns ejc-sql.connect-test
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [clojure.java.jdbc :as j]
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

(deftest read-only-test
  (testing "read-only? fn test."
    (doseq [sql ["select * from t"
                 "SELECT 1"
                 "-- comment\nselect * from t"
                 "/* comment */ select * from t"
                 "/*+ hint */ select * from t"
                 "/*Main*/ select * from t"
                 "(select 1) union (select 2)"
                 "with x as (select 1) select * from x"
                 "explain select * from t"
                 "show tables"
                 "values (1)"
                 "desc t"
                 "describe t"]]
      (is (read-only? sql) sql))
    (doseq [sql ["insert into t values (1)"
                 "update t set a = 1"
                 "delete from t"
                 "create table t (a int)"
                 "drop table t"
                 "call p()"
                 "exec p"
                 "selectx from t"
                 "delimiter /"
                 "-- select\ndelete from t"
                 "# select\ndelete from t"
                 "/*! insert into t */ select 1"
                 "/*M!100100 insert into t */ select 1"
                 "/* /* */ select 1 */ delete from t"
                 "{call p()}"
                 "[select]"
                 "[select] * from t"
                 "# comment\nselect * from t"
                 ""]]
      (is (not (read-only? sql)) sql))
    (testing "# starts a line comment if hash-comments?, as in MySQL."
      (is (read-only? "# comment\nselect * from t" true))
      (is (not (read-only? "# select\ndelete from t" true)))
      (is (= "SELECT" (first-keyword "#p\n select 1" true)))
      (is (= nil (first-keyword "#p\n select 1"))))
    (is (= "DELETE" (first-keyword "/* select */ delete from t")))
    (is (= nil (first-keyword "  ")))
    (is (= nil (first-keyword "-- select")))))

(def h2-db
  {:classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "mem:ejc_connect_test;DB_CLOSE_DELAY=-1"})

(defn- with-h2-table [f]
  (j/db-do-commands h2-db ["DROP TABLE IF EXISTS t"
                           "CREATE TABLE t (id INT, name VARCHAR(10))"
                           "INSERT INTO t VALUES (1, 'a'), (2, 'b')"])
  (f))

(deftest fetch-rows-test
  (with-h2-table
    (fn []
      (testing "fetch-rows fn test."
        (with-open [conn (j/get-connection h2-db)]
          (let [seen (atom nil)]
            (is (= [[:id :name] [1 "a"] [2 "b"]]
                   (fetch-rows h2-db conn "SELECT id, name FROM t ORDER BY id"
                               {} #(reset! seen %))))
            (is (instance? java.sql.PreparedStatement @seen)))
          (is (= [[:id :name] [1 "a"]]
                 (fetch-rows h2-db conn "SELECT id, name FROM t ORDER BY id"
                             {:max-rows 1})))))
      (testing "execution-message fn test."
        (with-open [conn (j/get-connection h2-db)]
          (let [statement (.createStatement conn)]
            (is (= "Records affected: 2"
                   (execution-message
                    statement (.execute statement "UPDATE t SET name = 'c'"))))
            (is (= "Executed"
                   (execution-message
                    statement (.execute statement "DELETE FROM t WHERE id = 3"))))
            (is (= "Executed"
                   (execution-message
                    statement (.execute statement "CREATE TABLE u (id INT)"))))
            (.execute statement "DROP TABLE u")))))))

(deftest clob-to-string-test
  (testing "clob-to-string fn test."
    (with-open [conn (j/get-connection h2-db)]
      (let [sql (str "SELECT CAST(CASE x WHEN 1 "
                     "THEN 'first line' || CHAR(10) || 'second line' "
                     "ELSE '' END AS CLOB) AS c "
                     "FROM SYSTEM_RANGE(1, %d) ORDER BY x")]
        (is (= [[:c] ["first line\nsecond line"]]
               (fetch-rows h2-db conn (format sql 1) {})))
        (is (= [[:c] ["first line\nsecond line"] [""]]
               (fetch-rows h2-db conn (format sql 2) {})))
        (binding [*max-column-width* 14]
          (is (= [[:c] ["first line\n..."] [""]]
                 (fetch-rows h2-db conn (format sql 2) {}))))))))

(deftest eval-sql-core-test
  (with-h2-table
    (fn []
      (testing "eval-sql-core fn test."
        (is (= [(list :result-set [[:id :name] [1 "a"] [2 "b"]])
                (list :message "Records affected: 1")
                (list :message "Executed")]
               (eval-sql-core :db h2-db
                              :sql-list ["SELECT id, name FROM t ORDER BY id"
                                         "UPDATE t SET name = 'c' WHERE id = 1"
                                         "CREATE TABLE u (id INT)"])))
        (j/db-do-commands h2-db ["DROP TABLE u"])
        (is (= :message
               (ffirst (eval-sql-core :db h2-db
                                      :sql-list ["SELECT * FROM no_such_table"]))))
        (is (s/starts-with?
             (second (first (eval-sql-core :db h2-db
                                           :sql-list ["SELECT * FROM no_such_table"])))
             "Error: "))))))
