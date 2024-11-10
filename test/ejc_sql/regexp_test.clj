(ns ejc-sql.regexp-test
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest testing is]]
            [ejc-sql.connect :as conn]))

(deftest comments-re-test
  (testing "comments-re test."

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
-- Remove before recreate
DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))

    (is (= "
" (s/replace "
-- Remove before recreate DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))

    (is (= "
" (s/replace "
/* Remove before recreate */;
" conn/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */
DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */;
DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))

    (is (= "
DROP TABLE IF EXISTS `user`;
" (s/replace "
/* Remove before recreate */; DROP TABLE IF EXISTS `user`;
" conn/comments-re "")))))

(deftest separator-re-test
  (testing "get-separator-re test."
    (is (= ["\n/* Remove before recreate */"
            "\nDROP TABLE IF EXISTS `user`"
            "\n"]
           (s/split "
/* Remove before recreate */;
DROP TABLE IF EXISTS `user`;
" (conn/get-separator-re ";"))))))
