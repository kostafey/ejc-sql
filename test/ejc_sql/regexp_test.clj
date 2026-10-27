(ns ejc-sql.regexp-test
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest testing is]]
            [ejc-sql.connect :as conn]
            [ejc-sql.lib :as lib]))

(deftest comments-re-test
  (testing "comments-re test."

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
-- Remove before recreate
DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (is (= "
" (s/replace "
-- Remove before recreate DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (is (= "
" (s/replace "
/* Remove before recreate */;
" lib/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */
DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */;
DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */; DROP TABLE IF EXISTS `user`;
" lib/comments-re "")))

    (testing "Line comment is not required to end with a line break."
      (is (= "" (s/replace "-- Remove before recreate"
                           lib/comments-re "")))
      (is (= "" (s/replace "--- Remove before recreate"
                           lib/comments-re "")))
      (is (= "\nDROP TABLE IF EXISTS `user`;\n"
             (s/replace "\nDROP TABLE IF EXISTS `user`;\n-- Done"
                        lib/comments-re ""))))

    (testing "MySQL `#` line comments."
      (is (= "DROP TABLE IF EXISTS `user`;\n"
             (s/replace "# Remove before recreate\nDROP TABLE IF EXISTS `user`;\n"
                        lib/comments-re "")))
      (is (= "" (s/replace "# Remove before recreate"
                           lib/comments-re ""))))

    (testing "`\\r\\n` line breaks are handled as well as `\\n` ones."
      (is (= "DROP TABLE IF EXISTS `user`;\r\n"
             (s/replace "-- Remove before recreate\r\nDROP TABLE IF EXISTS `user`;\r\n"
                        lib/comments-re "")))
      (is (= "DROP TABLE IF EXISTS `user`;\r\n"
             (s/replace "--- Remove before recreate\r\nDROP TABLE IF EXISTS `user`;\r\n"
                        lib/comments-re ""))))))

(deftest separator-re-test
  (testing "get-separator-re test."
    (is (= ["\n/* Remove before recreate */"
            "\nDROP TABLE IF EXISTS `user`"
            "\n"]
           (s/split "
/* Remove before recreate */;
DROP TABLE IF EXISTS `user`;
" (conn/get-separator-re ";"))))))
