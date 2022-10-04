(ns task1.core-test
  (:require [clojure.test :refer :all]
            [task1.core :refer :all]))

;;Тесты для задачи 1.1 и 1.2
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
    (is (= (permute '("a" "b" "c") 2) '("bc" "ac" "cb" "ab" "ca" "ba")))))

(deftest test-cons1-task1-permute
  (testing "'(a) 2"
    (is (= (permute '(a) 2) ()))))

(deftest test-cons2-task1-permute
  (testing "'(a a) 2"
    (is (= (permute '(a a) 2) ()))))

(deftest test-cons3-task1-permute
  (testing "'(a b) 2"
    (is (= (permute '(a b) 2) '("ab" "ba")))))

(deftest test-num-vec-task1-permute
  (testing "Вектор с числами"
    (is (= (permute [3 2 1] 3) '("123" "223" "323" "113" "213" "313" "132" "232" "332" "112" "212" "312" "131" "231" "331" "121" "221" "321")))))

(deftest test-number-task1-permute
  (testing "Работа с числами"
    (is (= (permute [32 1] 2) '("321" "132")))))

;;Тесты для 1.3
(deftest test-range6-task1-my-map
  (testing "range 6"
    (is (= (map (fn [x] (* x x)) (range 6)) (my-map (fn [x] (* x x)) (range 6))))))

(deftest test-inc-task1-my-map
  (testing "inc"
    (is (= (map inc [1 2 3 4 5]) (my-map inc [1 2 3 4 5])))))

(deftest test-range6-even-task1-my-filter
  (testing "Четные числа range 6"
    (is (= (filter even? (range 6)) (my-filter even? (range 6))))))

(deftest test-range121-mod3-task1-my-filter
  (testing "Из лекции"
    (is (= (filter (fn [n] (= 0 (mod n 3))) (range 1 21)) (my-filter (fn [n] (= 0 (mod n 3))) (range 1 21))))))

;Тесты для задачи 1.4
(deftest empty-test-task1-permute2
  (testing "Empty alphabet"
    (is (= (permute [] 2) ()))))

(deftest zero-test-task1-permute2
  (testing "Zero length"
    (is (= (permute2 ["a" "b" "c"] 0) ()))))

(deftest negative-test-task1-permute2
  (testing "Negative length"
    (is (= (permute2 ["a" "b" "c"] -2) ()))))

(deftest given-test-task1-permute2
  (testing "Тест из задачи"
    (is (= (permute2 '("a" "b" "c") 2) '("ac" "bc" "ab" "cb" "ba" "ca")))))

(deftest test-cons1-task1-permute2
  (testing "'(a) 2"
    (is (= (permute2 '(a) 2) ()))))

(deftest test-cons2-task1-permute2
  (testing "'(a a) 2"
    (is (= (permute2 '(a a) 2) ()))))

(deftest test-cons3-task1-permute2
  (testing "'(a b) 2"
    (is (= (permute2 '(a b) 2) '("ab" "ba")))))

(deftest test-num-vec-task1-permute2
  (testing "Вектор с числами"
    (is (= (permute2 [3 2 1] 3) '("313" "213" "323" "123" "312" "212" "232" "132" "321" "121" "231" "131")))))

(deftest test-number-task1-permute2
  (testing "Работа с числами"
    (is (= (permute2 [32 1] 2) '("321" "132")))))

