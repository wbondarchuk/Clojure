(ns task4.core-test
  (:require [clojure.test :refer :all]
            [task4.operations :refer :all]))

(deftest testElementaryFuncImpl
  (testing "F = A->B = ~AvB")
  (is (= (doall (disjunction (invert (variable :A)) (variable :B)))
         (doall (elementary-function
                  (implication (variable :A) (variable :B)))))))

(deftest rightDeleteInvert1
  (testing "~(AvB) = ~A&~B")
  (is (= (doall (conjunction (invert (variable :A)) (invert (variable :B))))
         (doall (invertExp (disjunction (variable :A) (variable :B)))))))

(deftest rightDeleteInvert2
  (testing "~(A&B) = ~Av~B")
  (is (= (doall (disjunction (invert (variable :A)) (invert (variable :B))))
         (doall (invertExp (conjunction (variable :A) (variable :B)))))))

(deftest testDistributionLaw1
  (testing "(AvB)&C = (A&C)v(B&C)")
  (is (= (doall (disjunction
                  (conjunction (variable :A) (variable :C))
                  (conjunction (variable :B) (variable :C))))
         (doall (distribution
                  (conjunction
                    (disjunction (variable :A) (variable :B))
                    (variable :C)))))))

(deftest testDistributionLaw2
  (testing "(A&B)vC = (AvC)&(BvC)")
  (is (= (doall (conjunction
                  (disjunction (variable :A) (variable :C))
                  (disjunction (variable :B) (variable :C))))
         (doall (distribution
                  (disjunction
                    (conjunction (variable :A) (variable :B))
                    (variable :C)))))))


(deftest rightDeleteInvertDnf1
  (testing "F = ~(AvB) = ~A&~B")
  (is (= (doall (conjunction (invert (variable :A)) (invert (variable :B))))
         (doall (dnf
                  (invert (disjunction (variable :A) (variable :B))))))))

(deftest rightDeleteInvertDnf2
  (testing "F = ~(A&B) = ~AV~B")
  (is (= (doall (disjunction (invert (variable :A)) (invert (variable :B))))
         (doall (dnf
                  (invert (conjunction (variable :A) (variable :B))))))))

(deftest rightDifficultDnf
  (testing "F = ~((X->Y)v~(Y->Z)) = (X&~Y&~Y)v(X&~Y&Z)")
  (is (= (doall (disjunction
                  (conjunction
                    (conjunction (variable :x) (invert (variable :y)))
                    (invert (variable :y)))
                  (conjunction
                    (conjunction (variable :x) (invert (variable :y)))
                    (variable :z))))
         (doall (dnf
                  (invert
                    (disjunction
                      (implication (variable :x) (variable :y))
                      (invert
                        (implication (variable :y) (variable :z))))))))))

(deftest rightElementaryDnf
  (testing "F = A->B = ~AvB")
  (is (= (doall (disjunction (invert (variable :A)) (variable :B)))
         (doall (dnf
                  (implication (variable :A) (variable :B)))))))

