(ns task4.dnf
  (:require [clojure.test :refer :all]
            [task4.operations :refer :all]))




(defn calculate [expr variables]
  (expr-evaluator expr variables))
(def test-vars-values
  (hash-map
    :x true
    :y false))
(def test-expr
  (conjunction
    (disjunction (variable :x) (constant false))
    (implication (variable :y) (variable :y))))


;(println (calculate test-expr test-vars-values))



(defn dnf
  "Применить ДНФ"
  [expr]
  (cond
    (empty? expr) expr
    (elementary-function? expr) expr
    (almost-elementary? expr) (cons (first expr) (list (dnf (second expr)) (dnf (last expr))))
    (implication? expr) (disjunction (invert (second expr)) (last expr))
    (and (invert? expr) (or (conjunction? (last expr)) (disjunction? (last expr)))) (dnf (de-morgan (last expr)))
    (and (invert? expr) (invert? (last expr))) (delete-invert expr)
    (or (and (conjunction? expr) (or (disjunction? (second expr)) (disjunction? (last expr))))
        (and (disjunction? expr) (or (conjunction? (second expr)) (conjunction? (last expr))))) (recur-distribution expr)
    (reduceable? expr) (reducing expr)
    (or (conjunction? expr) (disjunction? expr)) (dnf (cons (first expr) (list (dnf (second expr)) (dnf (last expr)))))
    :else (dnf (list (first expr) (dnf (last expr))))))

;(dnf (invert (disjunction (implication (variable :x) (variable :y)) (invert (implication (variable :y) (variable :z))))))