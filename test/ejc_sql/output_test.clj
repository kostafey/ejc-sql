(ns ejc-sql.output-test
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :refer :all]
            [ejc-sql.lib :refer [windows?]]
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

(deftest print-table-numbers-test
  (let [nl (System/getProperty "line.separator")
        table (fn [rows & {:keys [width null-text]}]
                (with-out-str
                  (binding [*add-outside-borders* false
                            ejc-sql.lib/*max-column-width* width
                            *null-text* (or null-text *null-text*)]
                    (print-table rows 0))))]
    (testing "Numbers to the right, their points one under another."
      (is (= (str "    amount |   n | mixed" nl
                  "-----------+-----+------" nl
                  "    123.56 |   1 | a    " nl
                  "      0.13 |     | 2    " nl
                  "2342343.38 | 100 |      " nl)
             (table [[:amount :n :mixed]
                     [123.56M 1 "a"] [0.13M nil 2] [2342343.38M 100 nil]])))
      (is (= (str "     d" nl
                  "------" nl
                  " 1.5  " nl
                  "12.345" nl
                  " 7    " nl)
             (table [[:d] [1.5] [12.345] [7]]))))
    (testing "A column of NULLs only is no column of numbers."
      (is (= (str "x | n" nl
                  "--+--" nl
                  "  | 1" nl
                  "  | 2" nl)
             (table [[:x :n] [nil 1] [nil 2]]))))
    (testing "Numbers are not cut, other values are."
      (is (= (str "                                       n | s         " nl
                  "-----------------------------------------+-----------" nl
                  "1234567890123456789012345678901234567890 | abcdefg..." nl
                  "                                       1 | b         " nl)
             (table [[:n :s]
                     [1234567890123456789012345678901234567890M "abcdefghijkl"]
                     [1 "b"]]
                    :width 10))))
    (testing "NULL is blank as in psql, unless told otherwise."
      (is (= (str "n | s" nl
                  "--+--" nl
                  "  | a" nl
                  "2 |  " nl)
             (table [[:n :s] [nil "a"] [2 nil]])))
      (is (= (str "   n | s   " nl
                  "-----+-----" nl
                  "NULL | a   " nl
                  "   2 | NULL" nl
                  "   3 |     " nl)
             (table [[:n :s] [nil "a"] [2 nil] [3 ""]] :null-text "NULL")))
      (is (= (str "n   " nl
                  "----" nl
                  "NULL" nl)
             (table [[:n] [nil]] :null-text "NULL")))
      (is (= (str "n | NULL" nl
                  "s | a" nl)
             (table [[:n :s] [nil "a"]] :null-text "NULL"))))))

(defn- with-log-dir
  "Call `f` with the `ejc-sql.log-dir` system property set to `dir`, or
  cleared if `dir` is nil, restoring it afterwards."
  [dir f]
  (let [saved (System/getProperty "ejc-sql.log-dir")
        set-dir #(if %
                   (System/setProperty "ejc-sql.log-dir" %)
                   (System/clearProperty "ejc-sql.log-dir"))]
    (try
      (set-dir dir)
      (f)
      (finally
        (set-dir saved)))))

(deftest log-dir-test
  (testing "windows? tells Windows only."
    (is (= (s/starts-with? (System/getProperty "os.name") "Windows")
           windows?)))
  (testing "The log is kept with the user's ejc-sql files."
    (with-log-dir nil
      #(is (= (if windows?
                (io/file (or (System/getenv "AppData")
                             (System/getProperty "user.home"))
                         "ejc-sql")
                (io/file (System/getProperty "user.home") ".ejc-sql" "log"))
              (get-log-dir)))))
  (testing "The tests keep off the user's log."
    (is (= (io/file "ejc-sql") (get-log-dir)))))

(deftest log-sql-test
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "ejc-log-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        blocker (io/file dir "file")]
    (try
      (testing "The query is appended to the log of today."
        (with-log-dir (str (io/file dir "log"))
          (fn []
            (log-sql "SELECT 1")
            (log-sql "SELECT 2")
            (let [text (slurp (get-log-file))]
              (is (s/starts-with? text "-- -*- mode: sql; -*-"))
              (is (re-find #"(?s)SELECT 1\n.*SELECT 2\n$" text))))))
      (testing "Nothing is written with the log off."
        (with-log-dir (str (io/file dir "off"))
          (fn []
            (try
              (set-log-queries nil)
              (is (nil? (log-sql "SELECT 1")))
              (is (not (.exists (io/file dir "off"))))
              (finally
                (set-log-queries true))))))
      (testing "A log that can't be written doesn't fail the query."
        (spit blocker "")
        (with-log-dir (str (io/file blocker "log"))
          (fn []
            (let [err (java.io.StringWriter.)]
              (binding [*err* err]
                (is (nil? (log-sql "SELECT 1"))))
              (is (s/starts-with? (str err)
                                  "ejc-sql: can't write the query log:"))))))
      (finally
        (doseq [f (reverse (file-seq dir))]
          (.delete ^java.io.File f))))))
