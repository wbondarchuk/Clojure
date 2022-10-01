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
    (is (= (permute [] 2) ()))))

(deftest zero-test-task1-permute
  (testing "Zero length"
    (is (= (permute ["a" "b" "c"] 0) ()))))

(deftest negative-test-task1-permute
  (testing "Negative length"
    (is (= (permute ["a" "b" "c"] -2) ()))))

(deftest given-test-task1-permute
  (testing "Тест из задачи"
    (is (= (permute '("a" "b" "c") 2) '(["a" "b"] ["a" "c"] ["b" "a"] ["b" "c"] ["c" "a"] ["c" "b"])))))

(deftest test-cons1-task1-permute
  (testing "'(a) 2"
    (is (= (permute '(a) 2) ()))))

(deftest test-cons2-task1-permute
  (testing "'(a a) 2"
    (is (= (permute '(a a) 2) ()))))

(deftest test-cons3-task1-permute
  (testing "'(a b) 2"
    (is (= (permute '(a b) 2) '([a b] [b a])))))

(deftest test-num-vec-task1-permute
  (testing "Вектор с числами"
    (is (= (permute [3 2 1] 3) '([1 3 1] [1 3 2] [1 2 1] [1 2 3] [3 1 3] [3 1 2] [3 2 1] [3 2 3] [2 1 3] [2 1 2] [2 3 1] [2 3 2])))))

(deftest test-number-task1-permute
  (testing "Работа с числами"
    (is (= (permute [32 1] 2) '([1 32] [32 1])))))