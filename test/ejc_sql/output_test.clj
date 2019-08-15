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
                 (str out))))))))
