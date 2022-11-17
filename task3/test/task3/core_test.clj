(ns task3.core-test
  (:require [clojure.test :refer :all]
            [task3.core :refer :all]))

(deftest equalResultTest
  (testing "Сравнение результатов"
    (is (= (doall (filter heavy-even? (range 100))) (doall (parallelFilter heavy-even? (range 100)))))))

(deftest equalResultTest2
  (testing "Сравнение результатов с заданным числом"
    (is (= (doall (filter heavy-even? (range 100))) (doall (parallelFilter heavy-even? (range 100) 100))))))
