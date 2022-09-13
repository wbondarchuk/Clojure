(ns task1.core)

;(defn foo
;  "I don't do a whole lot."
;  [x]
;  (println x "Hello, World!"))
;
;(defn square [x]
;  (* x x))
;
;(defn hypot [x y]
;  (Math/sqrt (+ (square x) (square y))))
;
;(def mult2 (fn [x] (+ x x)))
;
;(defn twice [f] (fn [x] (f (f x))))
;;((twice square) 4)
;
;(defn fact [n]
;  (if (zero? n)
;    1
;    (* n (fact (dec n)))))
;
;(defn fact-tail [n acc]
;  (if (zero? n)
;    acc
;    (fact-tail (dec n) (* acc n))))
;
;(defn fact-right
;  ([n] (fact-right n 1))
;  ([n acc]
;   (if (zero? n)
;     acc
;     (fact-tail (dec n) (* acc n)))))
;
;(defn fact2 [n]
;  (let [anon-fact
;        (fn [n acc]
;          (if (zero? n)
;            acc
;            (recur (dec n) (* acc n))))]
;    (anon-fact n 1N)))
;(defn fib [n]
;  (cond
;    (== 0 n) 1
;    (== 1 n) 1
;    :else (+ (fib (- n 1)) (fib (- n 2)))))
;(mapv fib (range 10))

;(def fib
;  (memoize
;    (fn [n]
;      (cond
;        (== 0 n) 1N
;        (== 1 n) 1N
;        :else (+ (fib (- n 1)) (fib (- n 2)))))))
;
;(defn iter-fib' [n a b]
;  (if (== 0 n)
;    a
;    (recur (dec n) b (+ a b))))

;(defn iter-fib [n]
;  (iter-fib' n 1N 1N))

;(defn iter-fib [n]
;  (letfn [
;          (iter-fib- [n a b]
;            (if (== 0 n)
;              a
;              (iter-fib- (dec n) b (+ a b))))
;          ]
;    (iter-fib- n 1N 1N)))


;---- 1.1 ----
;(defn getAlphabet [alphabet n]
;  (if (or (zero? n) (empty? alphabet))
;    (println "No combinations!")
;    (println n alphabet)))
;
;(defn permutations [alphabet]
;  (cond
;    (= (count alphabet) 1) (list alphabet)
;    :else (apply concat
;                 (map (fn [head]
;                      (map (fn [tail]
;                             (cons head tail))
;                           (permutations (remove #{head} alphabet))))
;                      alphabet))))


;(defn permute
;  ([alphabet n] (permute alphabet n nil))
;  ([alphabet n prev]
;   (cond
;     (or (zero? n) (empty? alphabet)) (println "No combinations!")
;     :else (cons (first alphabet) (permute alphabet (dec n) (first alphabet))))
;   ))

;Выше эксперементы, ниже решение

;Вывод в виде строк
(defn permute_add_end [prefix alphabet num_of_items]
  (if (zero? num_of_items)
    nil
    (if (= (nth alphabet (dec num_of_items)) (str (last prefix)))
      (permute_add_end prefix alphabet (dec num_of_items))
      (concat
        (conj
          (permute_add_end prefix alphabet (dec num_of_items))
          (str prefix (nth alphabet (dec num_of_items))))))))

(defn permute_add_list [list alphabet num_of_items counter]
  (if (= counter (count list))
    (if (nil? list)
      (permute_add_end nil alphabet num_of_items)
      nil)
    (concat
      (permute_add_end (nth list counter) alphabet num_of_items)
      (permute_add_list list alphabet num_of_items (inc counter)))))

(defn permute [alphabet length]
  (if (or (zero? length) (empty? alphabet))
    nil
    (permute_add_list (permute alphabet (dec length)) alphabet (count alphabet) 0)))


