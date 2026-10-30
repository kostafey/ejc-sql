(ns ejc-sql.structure-test
  (:require [clojure.java.jdbc :as j]
            [clojure.string :as s]
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

(deftest sqlserver-constraints-test
  (let [db {:subprotocol "sqlserver"
            :subname "//localhost:1433"
            :database "my_db_name"}]
    (testing "The `%` of the LIKE pattern is not taken by `format`."
      (is (s/includes? (select-db-meta-script db :constraints :table "users")
                       "LIKE '%CONSTRAINT'")))
    (testing "The schema of the table, if known."
      (is (s/includes? (select-db-meta-script db :constraints
                                              :owner "sales" :table "users")
                       "OBJECT_SCHEMA_NAME(parent_object_id)='sales'"))
      (is (not (s/includes? (select-db-meta-script db :constraints
                                                   :table "users")
                            "OBJECT_SCHEMA_NAME"))))))

(deftest h2-tables-test
  (testing "The tables of the owner given, of PUBLIC by default."
    (let [db {:subprotocol "h2" :subname "mem:x"}]
      (is (s/includes? (select-db-meta-script db :tables :owner "OTHER")
                       "table_schema = 'OTHER'"))
      (is (s/includes? (select-db-meta-script db :tables)
                       "table_schema = 'PUBLIC'")))))

(deftest column-type-test
  (testing "The scale of a decimal is shown unless it is 0 or unknown."
    (doseq [[digits type] [[2 "NUMERIC(10,2)"]
                           [-3 "NUMERIC(10,-3)"]
                           [0 "NUMERIC(10)"]
                           [nil "NUMERIC(10)"]]]
      (is (= type (#'ejc-sql.structure/column-type
                   {:type_name "NUMERIC"
                    :data_type java.sql.Types/NUMERIC
                    :column_size 10
                    :decimal_digits digits}))
          (pr-str digits)))))

(def h2-db
  {:subprotocol "h2"
   :subname "mem:ejc_structure_test;DB_CLOSE_DELAY=-1"})

(deftest table-details-test
  (j/db-do-commands h2-db
                    ["DROP ALL OBJECTS"
                     "CREATE TABLE orgs (id INT PRIMARY KEY, name VARCHAR(50))"
                     (str "CREATE TABLE pairs (a INT, b INT, "
                          "CONSTRAINT pk_pairs PRIMARY KEY (a, b))")
                     (str "CREATE TABLE users ("
                          "id INT AUTO_INCREMENT PRIMARY KEY, "
                          "org_id INT NOT NULL, "
                          "login VARCHAR(20) DEFAULT 'guest' NOT NULL, "
                          "balance DECIMAL(10,2), note VARCHAR, x INT, y INT, "
                          "CONSTRAINT fk_org FOREIGN KEY (org_id) "
                          "REFERENCES orgs(id), "
                          "CONSTRAINT fk_pair FOREIGN KEY (x, y) "
                          "REFERENCES pairs(a, b))")
                     "COMMENT ON COLUMN users.note IS 'free text'"
                     "CREATE UNIQUE INDEX users_login ON users (login)"
                     "CREATE INDEX users_balance ON users (balance DESC, note)"
                     (str "CREATE TABLE orders (id INT, "
                          "user_id INT CONSTRAINT fk_user REFERENCES users(id))")
                     "CREATE SCHEMA other"
                     "CREATE TABLE other.users (z INT)"
                     "CREATE TABLE user1s (w INT)"])
  (j/with-db-connection [conn h2-db]
    (let [conn (j/db-connection conn)
          details (fn [owner table] (table-details conn owner table))
          describe (fn [owner table]
                     (with-out-str
                       (binding [o/*add-outside-borders* true]
                         (print-table-description
                          h2-db table
                          :owner owner
                          :details-fn (fn [_ owner table] (details owner table))
                          :table-meta-fn (fn [_ table-name]
                                           {:success false
                                            :result (str "Fallback " table-name)})
                          :query-fn (fn [_ _] (throw (Exception. "not called")))
                          :row-limit 0))))]
      (testing "The name and the owner are tried in the case the database keeps."
        (is (= ["PUBLIC" "USERS"] ((juxt :schema :table) (details nil "users"))))
        (is (= ["OTHER" "USERS"] ((juxt :schema :table) (details "other" "users"))))
        (is (= ["Z"] (map :column_name (:columns (details "other" "users"))))))
      (testing "The `_` of the name is not a wildcard."
        (is (= ["W"] (map :column_name (:columns (details nil "user1s")))))
        (is (nil? (details nil "user_s"))))
      (testing "Unknown tables."
        (is (nil? (details nil "nope")))
        (is (nil? (details "nope" "users")))
        (is (= "Fallback nope\n" (describe nil "nope"))))
      (testing "Columns, keys and references."
        (let [text (describe nil "users")]
          (is (s/starts-with? text "Table \"PUBLIC.USERS\" description:"))
          (is (re-find #"\| ID +\| INTEGER +\| not null +\| auto increment +\| +\|"
                       text))
          (is (re-find #"\| LOGIN +\| CHARACTER VARYING\(20\) +\| not null +\| 'guest' +\|"
                       text))
          (is (re-find #"\| BALANCE +\| DECIMAL\(10,2\) +\| +\| +\| +\|" text))
          (is (re-find #"\| NOTE +\| CHARACTER VARYING +\| +\| +\| free text +\|"
                       text))
          (is (re-find #"\nPrimary key \w+: ID\n" text))
          (is (re-find #"\| FK_ORG +\| ORG_ID +\| ORGS\(ID\) +\|" text))
          (is (re-find #"\| FK_PAIR +\| X, Y +\| PAIRS\(A, B\) +\|" text))
          (is (re-find #"Referenced by:\n\n.*\n.*\n\| FK_USER +\| ORDERS\(USER_ID\) +\| USERS\(ID\) +\|"
                       text))))
      (testing "Indexes, but the one of the primary key."
        (let [text (describe nil "users")]
          (is (re-find #"\nIndexes:\n\n\| name +\| columns +\| unique +\|\n" text))
          (is (re-find #"\| USERS_BALANCE +\| BALANCE DESC, NOTE +\| +\|" text))
          (is (re-find #"\| USERS_LOGIN +\| LOGIN +\| unique +\|" text))
          (is (not (re-find #"PRIMARY_KEY" text))))
        (is (= [["USERS_BALANCE" "BALANCE" "D"] ["USERS_BALANCE" "NOTE" "A"]]
               (->> (:indexes (details nil "users"))
                    (filter #(= "USERS_BALANCE" (:index_name (first %))))
                    first
                    (map (juxt :index_name :column_name :asc_or_desc)))))
        (is (not (re-find #"Indexes:" (describe nil "orgs")))))
      (testing "The optional columns and sections are left out when empty."
        (is (= (str "Table \"OTHER.USERS\" description:\n\n"
                    "| name  | type    |\n"
                    "|-------+---------|\n"
                    "| Z     | INTEGER |\n")
               (describe "OTHER" "users"))))
      (testing "A failing details-fn gives way to table-meta-fn."
        (is (= "Fallback users\n"
               (with-out-str
                 (print-table-description
                  h2-db "users"
                  :details-fn (fn [& _] (throw (java.sql.SQLException. "no")))
                  :table-meta-fn (fn [_ table-name]
                                   {:success false
                                    :result (str "Fallback " table-name)})))))))))

(deftest table-details-constraints-test
  (testing "The constraints are asked for by the names the database keeps."
    (let [sql (atom nil)]
      (with-out-str
        (print-table-description
         {:subprotocol "mysql" :subname "//localhost:3306/shop"} "users"
         :details-fn (fn [& _] {:catalog "shop" :schema nil :table "USERS"
                                :columns [{:column_name "ID"
                                           :type_name "INT"}]})
         :query-fn (fn [_ q] (reset! sql q) "Error: no access")))
      (is (s/includes? @sql "constraint_schema = 'shop'"))
      (is (s/includes? @sql "table_name = 'USERS'")))))
