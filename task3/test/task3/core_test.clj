(ns task3.core-test
  (:require [clojure.test :refer :all]
            [task3.core :refer :all]))

(deftest equalResultTest
  (testing "Сравнение результатов"
    (is (= (doall (filter heavy-even? (range 100))) (doall (parallelFilter heavy-even? (range 100)))))))

(deftest equalResultTest2
  (testing "Сравнение результатов с заданным числом"
    (is (= (doall (filter heavy-even? (range 100))) (doall (parallelFilter heavy-even? (range 100) 100))))))


(deftest equalResult2Test
  (testing "Сравнение результатов2"
    (is (= (doall (take 30 (filter heavy-even? (iterate inc 0)))) (doall (take 30 (parallelFilterInf heavy-even? (iterate inc 0) 10)))))))

(deftest equalResult2Test2
  (testing "Сравнение результатов2 с заданным числом"
    (is (= (doall (take 30 (filter heavy-even? (iterate inc 0)))) (doall (take 30 (parallelFilterInf heavy-even? (iterate inc 0) 10 10)))))))
