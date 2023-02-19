(ns ejc-sql.output-test
  (:require [clojure.test :refer :all]
            [ejc-sql.output :refer :all]))

(deftest print-table-test
  (testing "print-table fn test."
    (let [nl (System/getProperty "line.separator")]
      (is (= (str "| a | b |" nl
                  "|---+---|" nl
                  "| 1 | 2 |" nl
                  "| 3 | 4 |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table [["a" "b"] [1 2] [3 4]])
                 (str out)))))
      (is (= (str "| a | b |" nl
                  "|---+---|" nl
                  "| 1 | 2 |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table [["a" "b"] [1 2]])
                 (str out)))))
      (is (= (str "a | b" nl
                  "--+--" nl
                  "1 | 2" nl
                  "3 | 4" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* false]
                 (print-table [["a" "b"] [1 2] [3 4]])
                 (str out)))))
      (is (= (str "a | 1" nl
                  "b | 2" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* false]
                 (print-table [["a" "b"] [1 2]])
                 (str out)))))
      (is (= (str "| description                                  |" nl
                  "|----------------------------------------------|" nl
                  "| Lorem ipsum dolor sit amet, consectetur      |" nl
                  "| adipiscing elit, sed do eiusmod tempor       |" nl
                  "| incididunt ut labore et dolore magna aliqua. |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table
                  [["description"]
                   [(str
                     "Lorem ipsum dolor sit amet, consectetur" nl
                     "adipiscing elit, sed do eiusmod tempor" nl
                     "incididunt ut labore et dolore magna aliqua.")]])
                 (str out)))))
      (is (= (str "description                                 " nl
                  "--------------------------------------------" nl
                  "Lorem ipsum dolor sit amet, consectetur" nl
                  "adipiscing elit, sed do eiusmod tempor" nl
                  "incididunt ut labore et dolore magna aliqua." nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* false]
                 (print-table
                  [["description"]
                   [(str
                     "Lorem ipsum dolor sit amet, consectetur" nl
                     "adipiscing elit, sed do eiusmod tempor" nl
                     "incididunt ut labore et dolore magna aliqua.")]])
                 (str out)))))))
  (testing "print-table fn CJK test."
    (let [nl (System/getProperty "line.separator")]
      (is (= (str "| a  | b  |" nl
                  "|----+----|" nl
                  "| 1  | 2  |" nl
                  "| me | 我 |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table [["a" "b"] [1 2] ["me" "我"]])
                 (str out)))))
      (is (= (str "| a | b  |" nl
                  "|---+----|" nl
                  "| 1 | 我 |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table [["a" "b"] [1 "我"]])
                 (str out)))))
      (is (= (str "a  | b " nl
                  "---+---" nl
                  "1  | 2 " nl
                  "me | 我" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* false]
                 (print-table [["a" "b"] [1 2] ["me" "我"]])
                 (str out)))))
      (is (= (str "a  | 1" nl
                  "我 | 2" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* false]
                 (print-table [["a" "我"] [1 2]])
                 (str out)))))
      (is (= (str "| description                                  |" nl
                  "|----------------------------------------------|" nl
                  "| 天地玄黄 Lorem ipsum dolor sit amet,         |" nl
                  "| consectetur adipiscing 宇宙洪荒 日月盈昃     |" nl
                  "| incididunt ut labore et dolore magna aliqua. |" nl)
             (with-open [out (java.io.StringWriter.)]
               (binding [*out* out
                         *add-outside-borders* true]
                 (print-table
                  [["description"]
                   [(str
                     "天地玄黄 Lorem ipsum dolor sit amet," nl
                     "consectetur adipiscing 宇宙洪荒 日月盈昃" nl
                     "incididunt ut labore et dolore magna aliqua.")]])
                 (str out))))))))
