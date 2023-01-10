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
  "(X v False) & (X -> Y)"
  (conjunction
    (disjunction (variable :x) (constant false))
    (implication (variable :x) (variable :y))))

(def test-expr2
  (invert (invert (invert (invert (constant false))))))


(println test-expr)
(println test-vars-values)
(println (dnf test-expr))
(println (dnf test-expr2))
(println (calculate test-expr test-vars-values))
(println (calculate (dnf test-expr) test-vars-values))

