(ns task2.core-test
  (:require [clojure.test :refer :all]
            [task2.core :refer :all]))

(defn antiderivative-lin [x]
  (/ (* x x) 2))

(defn antiderivative-sq [x]
  (/ (* x x x) 3))

(defn antiderivative-cube [x]
  (/ (* x x x x) 4))


(defn abs [n] (max n (- n)))

(defn are-equal [a b]
  (< (abs (- a b)) 0.1))

(deftest test-lin-integrate
  (testing "Lin integrate"
    (is (are-equal (antiderivative-lin 10) ((integrate lin) 10)))))

(deftest test-lin-memIntegrate
  (testing "Lin memIntegrate"
    (is (are-equal (antiderivative-lin 10) ((memIntegrate lin) 10)))))

(deftest test-lin-seqIntegrate
  (testing "Lin seqIntegrate"
    (is (are-equal (antiderivative-lin 10) ((seqIntegrate lin) 10)))))

(deftest test-sq-integrate
  (testing "Sq integrate"
    (is (are-equal (antiderivative-sq 10) ((integrate sq) 10)))))

(deftest test-sq-memIntegrate
  (testing "Sq memIntegrate"
    (is (are-equal (antiderivative-sq 10) ((memIntegrate sq) 10)))))

(deftest test-sq-seqIntegrate
  (testing "Sq seqIntegrate"
    (is (are-equal (antiderivative-sq 10) ((seqIntegrate sq) 10)))))

(deftest test-cube-integrate
  (testing "Cube integrate"
    (is (are-equal (antiderivative-cube 10) ((integrate cube) 10)))))

(deftest test-cube-memIntegrate
  (testing "Cube memIntegrate"
    (is (are-equal (antiderivative-cube 10) ((memIntegrate cube) 10)))))

(deftest test-cube-seqIntegrate
  (testing "Cube seqIntegrate"
    (is (are-equal (antiderivative-cube 10) ((seqIntegrate cube) 10)))))

