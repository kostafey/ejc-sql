(ns ejc-sql.structure-test
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [ejc-sql.output :as o]
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
                     #'get-db-name (fn [db] "mydb")
                     #'get-all-tables get-tables
                     #'get-colomns (fn [db table force?]
                                     ({"users" '("id" "name")
                                       "products" '("id" "price")}
                                      table))}
      #(is
        (and
         (= '("nil" "id" "name")
            (let [sql "SELECT users. FROM users"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "users")))
         ;; Table alias
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users AS a"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "a")))
         ;; Table alias: spaces + tabs near to "AS"
         (= '("nil" "id" "name")
            (let [sql "SELECT a. FROM users  AS     a"]
              (get-colomns-candidates :db nil
                                      :sql sql
                                      :prefix-1 "a"))))))))

(deftest check-if-select-sql-test
  (testing "insert-sql? fn test."
    (is (= "product"
           (select-sql?
            "SELECT FROM product")))
    (is (= "product"
           (select-sql?
            "SELECT name FROM product")))
    (is (= "product"
           (select-sql?
            "SELECT name, price FROM product")))
    (is (= nil
           (select-sql?
            "INSERT INTO product (name, price) VALUES ('socks', 1.25);")))))

(deftest check-if-insert-sql-test
  (testing "insert-sql? fn test."
    (is (= "product"
           (insert-sql?
            "INSERT INTO product (name, price) VALUES ('socks', 1.25);")))
    (is (= nil
           (insert-sql?
            "UPDATE product SET price = 1.26 WHERE product = 'socks'")))
    (is (= nil
           (insert-sql?
            "SELECT * FROM product")))))

(deftest check-if-update-sql-test
  (testing "update-sql? fn test."
    (is (= nil
           (update-sql?
            "INSERT INTO product (name, price) VALUES ('socks', 1.25);")))
    (is (= "product"
           (update-sql?
            "UPDATE product SET price = 1.26 WHERE product = 'socks'")))
    (is (= nil
           (update-sql?
            "SELECT * FROM product")))))

(deftest print-table-description-test
  (testing "print-table-description fn test."
    (let [db {:subprotocol "mysql" :subname "//localhost:3306/shop"}
          columns (fn [_ _] {:success true
                             :result [{:name "ID" :type "INT"}]})
          describe (fn [& {:keys [table-meta-fn query-fn]}]
                     (with-out-str
                       (binding [o/*add-outside-borders* true]
                         (print-table-description db "users"
                                                  :table-meta-fn table-meta-fn
                                                  :query-fn query-fn
                                                  :row-limit 0))))
          sql (atom nil)
          text (describe :table-meta-fn columns
                         :query-fn (fn [_ q]
                                     (reset! sql q)
                                     [[:column_name :constraint_name]
                                      ["ID" "PRIMARY"]]))]
      (is (s/starts-with? text "Table \"users\" description:"))
      (is (s/includes? text "| ID    | INT   |"))
      (is (s/includes? text "Constraints:"))
      (is (s/includes? text "| ID          | PRIMARY         |"))
      (is (s/includes? @sql "table_name = 'users'"))
      ;; A failing constraints query is printed, not thrown.
      (is (s/ends-with? (describe :table-meta-fn columns
                                  :query-fn (fn [_ _] "Error: no access"))
                        "Constraints:\n\nError: no access\n"))
      ;; No columns - no constraints.
      (is (= "Error: no table\n"
             (describe :table-meta-fn (fn [_ _] {:success false
                                                 :result "Error: no table"})
                       :query-fn (fn [_ _] (throw (Exception. "not called")))))))))
