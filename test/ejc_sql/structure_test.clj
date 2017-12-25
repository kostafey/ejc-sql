(ns ejc-sql.structure-test
  (:require [clojure.test :refer :all]
            [ejc-sql.structure :refer :all]))

(deftest get-db-name-test
  (testing "get-db-name fn test."
    (is (= "my_db_name"
           (get-db-name
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= "my_db_name"
           (get-db-name
            {:connection-uri
             (str "jdbc:jtds:sqlserver://localhost:1433/my_db_name;"
                  "instance=instance;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= "my_db_name"
           (get-db-name {:database "my_db_name"
                         :subname "//localhost\\instance:1433"})))
    (is (= "my_db_name"
           (get-db-name {:subname "//localhost:3306/my_db_name"})))
    (is (= "my_db_name"
           (get-db-name {:subname "thin:@localhost:1521:my_db_name"})))))

(deftest get-user-test
  (testing "get-user fn test."
    (is (= "a_user"
           (get-user
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))))

(deftest get-db-type-test
  (testing "get-db-type fn test."
    (is (= :sqlserver
           (get-db-type
            {:connection-uri
             (str "jdbc:sqlserver://localhost\\\\instance:1433;"
                  "databaseName=my_db_name;"
                  "user=a_user;"
                  "password=secret;")})))
    (is (= :sqlserver
           (get-db-type
            {:connection-uri
             (str "jdbc:jtds:sqlserver://localhost:1433/dbname;"
                  "instance=instance;"
                  "user=a_user;"
                  "password=secret;")})))))

(deftest get-colomns-candidates-test
  (testing "get-colomns-candidates fn test."
    (with-redefs-fn {#'get-tables (fn [db]
                                    '("users" "products"))
                     #'get-db-type (fn [db] :h2)
                     #'get-all-tables get-tables
                     #'get-colomns (fn [db table force?]
                                     ({"users" '("id" "name")
                                       "products" '("id" "price")}
                                      table))}
      #(is
        (and
         (= '("nil" "id" "name")
            (let [sql "SELECT users. FROM users"]
              (get-colomns-candidates nil sql "users")))
         ;; Table alias
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users AS a"]
              (get-colomns-candidates nil sql "a")))
         ;; Table alias: spaces + tabs near to "AS"
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users  AS 	a"]
              (get-colomns-candidates nil sql "a"))))))))
