(ns task1.core-test
  (:require [clojure.test :refer :all]
            [task1.core :refer :all]))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))
;
;(deftest test-fact
;  (testing "n == 0"
;    (is (= (fact-right 0) 1)))
;  (testing "n == 1"
;    (is (= (fact-right 1) 1))))

(deftest empty-test-task1-permute
  (testing "Empty alphabet"
    (is (= (permute [] 2) nil))))

(deftest zero-test-task1-permute
  (testing "Zero length"
    (is (= (permute ["a" "b" "c"] 0) nil))))

(deftest given-test-task1-permute
  (testing "Тест из задачи"
    (is (= (permute ["a" "b" "c"] 2) ["cb" "ca" "bc" "ba" "ac" "ab"]))))