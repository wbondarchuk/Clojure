(ns task2.core-test
  (:require [clojure.test :refer :all]
            [task2.core :refer :all]))

;; Тесты интеграла
(deftest integrate-test-task2
  (testing "Integrate sq 10"
    (is (= ((integrate sq) 10) '(333.3333333500043)))))
