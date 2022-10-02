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

;;Тесты для 1.3
(deftest test-range6-task1-my-map
  (testing "range 6"
    (is (= (println (map (fn [x] (* x x)) (range 6))) (println (my-map (fn [x] (* x x)) (range 6)))))))

(deftest test-inc-task1-my-map
  (testing "inc"
    (is (= (println (map inc [1 2 3 4 5])) (println (my-map inc [1 2 3 4 5]))))))

(deftest test-range6-even-task1-my-filter
  (testing "Четные числа range 6"
    (is (= (println (filter even? (range 6))) (println (my-filter even? (range 6)))))))

(deftest test-range121-mod3-task1-my-filter
  (testing "Из лекции"
    (is (= (println (filter (fn [n] (= 0 (mod n 3))) (range 1 21))) (println (my-filter (fn [n] (= 0 (mod n 3))) (range 1 21)))))))

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
    (is (= (permute2 '("a" "b" "c") 2) '("ca" "ba" "cb" "ab" "bc" "ac")))))

(deftest test-cons1-task1-permute2
  (testing "'(a) 2"
    (is (= (permute2 '(a) 2) ()))))

(deftest test-cons2-task1-permute2
  (testing "'(a a) 2"
    (is (= (permute2 '(a a) 2) ()))))

(deftest test-cons3-task1-permute2
  (testing "'(a b) 2"
    (is (= (permute2 '(a b) 2) '("ba" "ab")))))

(deftest test-num-vec-task1-permute2
  (testing "Вектор с числами"
    (is (= (permute2 [3 2 1] 3) '("131" "231" "121" "321" "132" "232" "212" "312" "123" "323" "213" "313")))))

(deftest test-number-task1-permute2
  (testing "Работа с числами"
    (is (= (permute2 [32 1] 2) '("132" "321")))))

