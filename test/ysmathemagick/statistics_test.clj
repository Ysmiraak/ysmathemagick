(ns ysmathemagick.statistics-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test :refer :all]
            [ysmathemagick.statistics :refer :all :reload true]))

(instrument)

(deftest table-test
  (is (= (let [a 1 b 2 c 3] (table a b c))
         {:a 1, :b 2, :c 3})))

(deftest float->string-test
  (let [n 0.1234567]
    (is (= (float->string 0 n) "0"))
    (is (= (float->string 1 n) "0.1"))
    (is (= (float->string 4 n) "0.1235"))
    (is (= (float->string 7 n) "0.1234567"))
    (is (= (float->string 8 n) "0.12345670"))
    (is (= (float->string 9 Double/NaN) "NaN"))
    (is (= (float->string 0 Double/NEGATIVE_INFINITY) "-Infinity"))))

(deftest count-choose-test
  (is (= (count-choose 0 0) 1))
  (is (= (count-choose 0 1) 0))
  (is (= (count-choose 1 0) 1))
  (is (= (count-choose 1 1) 1))
  (is (= (count-choose 52 5) 2598960)))

(deftest fake-sample-test
  (is (= (fake-sample 2 0 0) [0.0 0.0]))
  (is (= (fake-sample 3 0 1) [0 1.0 -1.0])))
