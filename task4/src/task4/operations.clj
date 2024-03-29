(ns task4.operations
  (:gen-class)
  (:require clojure.set))

(defn constant
  "Создание константы"
  [num]
  (list ::c num))

(defn constant?
  "Является ли выражение константой?"
  [expr]
  (= (first expr) ::c))

(def c-true
  (constant true))
(def c-false
  (constant false))

(defn variable
  "Создание переменной"
  [name]
  (list ::var name))

(defn variable?
  "Является ли выражение переменной?"
  [expr]
  (= (first expr) ::var))

(defn legal-expr?
  "Проверка типа выражения"
  [expr expr-type]
  (= expr-type (first expr)))

(defn expr-value
  "Получить значение выражения"
  [expr expr-type]
  (if (legal-expr? expr expr-type)
    (second expr)
    (throw (IllegalArgumentException. "Bad type"))))

(defn get-expr-type
  "Получить тип выражения"
  [expr]
  (first expr))

(defn variable-name
  "Получить имя переменной"
  [expr]
  (second expr))

(defn type-equals?
  "Выражения имеют одинаковый тип?"
  [expr1 expr2]
  (= (first expr1) (first expr2)))

(defn same-variables?
  "Совпадают ли две переменные?"
  [var1 var2]
  (and
   (variable? var1)
   (variable? var2)
   (= (variable-name var1) (variable-name var2))))

(defn equals?
  "Выражения абсолютно одинаковые?"
  [expr1 expr2]
  (if (type-equals? expr1 expr2)
    (if (and (or (constant? (first expr1)) (variable? (first expr1)))
             (or (constant? (first expr2)) (variable? (first expr2))))
      (= (second expr1) (second expr2))
      (or (and (equals? (second expr1) (second expr2)) (equals? (last expr1) (last expr2)))
          (and (equals? (second expr1) (last expr2)) (equals? (last expr1) (second expr2)))))
    false))

(defn args
  "Получить аргументы выражения"
  [expr]
  (rest expr))

(defn conjunction
  "Конъюнкция(&)"
  [expr & rest]
  (list ::conj (cons expr rest)))

(defn conjunction?
  "Является ли выражение конъюкцией?"
  [expr]
  (= (first expr) ::conj))

(defn disjunction
  "Дизъюнкция(v)"
  [expr & rest]
  (list ::disj (cons expr rest)))

(defn disjunction?
  "Является ли выражение дизъюкцией?"
  [expr]
  (= (first expr) ::disj))

(defn implication
  "Импликация(->)"
  [expr1 expr2]
  (list ::impl (list expr1 expr2)))

(defn implication?
  "Является ли выражение импликацией?"
  [expr]
  (= (first expr) ::impl))

(defn invert
  "Инверсия(~)"
  [expr]
  (list ::inv expr))

(defn invert?
  "Является ли выражение инверсией?"
  [expr]
  (= (first expr) ::inv))

;(defn elementary-function?
;  "Выражение является элементарной функцией?"
;  [expr]
;  (or
;   (constant? expr)
;   (variable? expr)
;   (and
;    (invert? expr)
;    (or
;     (constant? (last expr))
;     (variable? (last expr))))))
;
;(defn dnf?
;  "Выражение в ДНФ?"
;  [expr]
;  (cond (elementary-function? expr) true
;        (conjunction? expr) (reduce (fn [acc x] (and acc x))
;                                    (map elementary-function? (args expr)))
;        (disjunction? expr) (reduce (fn [acc x] (and acc x))
;                                    (map dnf? (args expr)))
;        :else false))
;;
;(defn de-morgan
;  "Применить закон Де Моргана на выражение"
;  [expr]
;  (if (conjunction? expr)
;    (disjunction (invert (second expr)) (invert (last expr)))
;    (conjunction (invert (second expr)) (invert (last expr)))))
;
;(defn almost-elementary?
;  "Является ли выражение элементарным или простой конъюкцией/дизъюнкцией?"
;  [expr]
;  (or (elementary-function? expr)
;      (and (conjunction? expr) (elementary-function? (second expr)) (elementary-function? (last expr)))
;      (and (disjunction? expr) (elementary-function? (second expr)) (elementary-function? (last expr)))))
;

;(defn distribution
;  "Распределительный закон"
;  [expr]
;  (if (almost-elementary? expr)
;    expr
;    (if (conjunction? expr)
;      (if (disjunction? (last expr))
;        (disjunction (conjunction (second expr) (second (last expr))) (conjunction (second expr) (last (last expr))))
;        (disjunction (conjunction (last expr) (second (second expr))) (conjunction (last expr) (last (second expr)))))
;      (if (conjunction? (last expr))
;         (conjunction (disjunction (second expr) (second (last expr))) (disjunction (second expr) (last (last expr))))
;         (conjunction (disjunction (last expr) (second (second expr))) (disjunction (last expr) (last (second expr))))
;        ))))
;
;(defn recur-distribution
;  "Рекурсивная дистрибутивность"
;  [expr]
;  (if (conjunction? expr)
;    (distribution (conjunction (recur-distribution (second expr)) (recur-distribution (last expr))))
;    (if (disjunction? expr)
;      (disjunction (recur-distribution (second expr)) (recur-distribution (last expr)))
;      expr)))
;
;(defn delete-invert
;  "Применить двойную инверсию"
;  [expr]
;  (last (last expr)))

;(defn reduceable?
;  "Проверяет является ли выражение конъюнкцией дизъюнкции или дизъюнкцией конъюнкции"
;  [expr]
;  (cond (and (conjunction? expr) (variable? (second expr)) (disjunction? (last expr))) true
;        (and (conjunction? expr) (variable? (last expr)) (disjunction? (second expr))) true
;        (and (disjunction? expr) (variable? (last expr)) (conjunction? (second expr))) true
;        (and (disjunction? expr) (variable? (second expr)) (conjunction? (last expr))) true
;        :else false
;        ))

;(defn reducing
;  "Сократить"
;  [expr]
;  (cond (and (conjunction? expr) (variable? (second expr)) (disjunction? (last expr))) (second expr)
;        (and (conjunction? expr) (variable? (last expr)) (disjunction? (second expr))) (last expr)
;        (and (disjunction? expr) (variable? (last expr)) (conjunction? (second expr))) (last expr)
;        (and (disjunction? expr) (variable? (second expr)) (conjunction? (last expr))) (second expr)))


(defmulti expr-evaluator (fn [expr _] (get-expr-type expr)))
(defmethod expr-evaluator ::c [const-expr _] (expr-value const-expr ::c))
(defmethod expr-evaluator ::var [var-expr variables] (get variables (expr-value var-expr ::var)))
(defmethod expr-evaluator ::conj [conj-expr variables]
  (let [[expr1 expr2] (expr-value conj-expr ::conj)]
    (and (expr-evaluator expr1 variables) (expr-evaluator expr2 variables))))
(defmethod expr-evaluator ::disj [disj-expr variables]
  (let [[expr1 expr2] (expr-value disj-expr ::disj)]
    (or (expr-evaluator expr1 variables) (expr-evaluator expr2 variables))))
(defmethod expr-evaluator ::impl [impl-expr variables]
  (let [[expr1 expr2] (expr-value impl-expr ::impl)]
    (or (not (expr-evaluator expr1 variables)) (expr-evaluator expr2 variables))))
(defmethod expr-evaluator ::inv [inv-expr variables]
  (let [expr (expr-value inv-expr ::inv)]
    (not (expr-evaluator expr variables))))

(declare invertExp)

(defmulti elementary-function
          "Избавиться от всех логических операций, содержащихся в формуле, заменив их основными"
          (fn [expr] (get-expr-type expr)))
(defmethod elementary-function ::c [exp] exp)
(defmethod elementary-function ::var [exp] exp)
(defmethod elementary-function ::conj [exp]
  (let [[exp1 exp2] (expr-value exp ::conj)]
    (conjunction (elementary-function exp1) (elementary-function exp2))))
(defmethod elementary-function ::disj [exp]
  (let [[exp1 exp2] (expr-value exp ::disj)]
    (disjunction (elementary-function exp1) (elementary-function exp2))))
(defmethod elementary-function ::impl [exp]
  (let [[exp1 exp2] (expr-value exp ::impl)]
    (disjunction (invert exp1) exp2)))
(defmethod elementary-function ::inv [exp]
  (invertExp (elementary-function (expr-value exp ::inv))))

(defmulti invertExp
          "Заменить знак отрицания, относящийся ко всему выражению, знаками отрицания, относящимися к отдельным переменным высказываниям на основании формул"
          (fn [expr] (get-expr-type expr)))
(defmethod invertExp ::c [exp] (invert exp))
(defmethod invertExp ::var [exp] (invert exp))
(defmethod invertExp ::conj [exp]
  (let [[exp1 exp2] (expr-value exp ::conj)]
    (disjunction (invertExp exp1) (invertExp exp2))))
(defmethod invertExp ::disj [exp]
  (let [[exp1 exp2] (expr-value exp ::disj)]
    (conjunction (invertExp exp1) (invertExp exp2))))
(defmethod invertExp ::inv [exp]
  (elementary-function (expr-value exp ::inv)))

(defmulti distribution
          "Применить свойство дистрибутивности"
          (fn [exp] (get-expr-type exp)))
(defmethod distribution ::c [exp] exp)
(defmethod distribution ::var [exp] exp)
(defmethod distribution ::inv [exp] exp)
(defmethod distribution ::disj [exp]
  (let [[exp1 exp2] (expr-value exp ::disj)
        exp1 (distribution exp1)
        exp2 (distribution exp2)]
    (cond
      (= (get-expr-type exp1) ::conj) (let [[expr1 expr2] (expr-value exp1 ::conj)]
                                        (conjunction
                                          (distribution (disjunction expr1 exp2))
                                          (distribution (disjunction expr2 exp2))))
      (= (get-expr-type exp2) ::conj) (let [[expr1 expr2] (expr-value exp2 ::conj)]
                                        (conjunction
                                          (distribution (disjunction exp1 expr1))
                                          (distribution (disjunction exp1 expr2))))
      :else (disjunction exp1 exp2))))
(defmethod distribution ::conj [exp]
  (let [[exp1 exp2] (expr-value exp ::conj)
        exp1 (distribution exp1)
        exp2 (distribution exp2)]
    (cond
      (= (get-expr-type exp1) ::disj) (let [[expr1 expr2] (expr-value exp1 ::disj)]
                                        (disjunction
                                          (distribution (conjunction expr1 exp2))
                                          (distribution (conjunction expr2 exp2))))
      (= (get-expr-type exp2) ::disj) (let [[expr1 expr2] (expr-value exp2 ::disj)]
                                        (disjunction
                                          (distribution (conjunction exp1 expr1))
                                          (distribution (conjunction exp1 expr2))))
      :else (conjunction exp1 exp2))))


(defn dnf [exp]
  (distribution (elementary-function exp)))






