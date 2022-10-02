(ns task1.core)

;Использую возведение в степень, чтобы найти всевозможные комбинации
(defn pow [x n]
  (loop [acc 1
         n n]
    (if (<= n 0) acc
                 (recur (* x acc) (dec n)))))

;Получаю индекс следующей буквы
(defn get_letr [letr_count words_count i pos prev_index prev_div]
  (let [first_letr? (= pos 0)
        div (if first_letr? (quot words_count letr_count)
                            (quot prev_div (dec letr_count)))
        rest (if first_letr? letr_count
                             (dec letr_count))
        letr_index (mod (quot i div) rest)
        offset? (<= prev_index letr_index)
        new_index (+ letr_index (if offset? 1 0))]
    (list div new_index)))


(defn permute [alphabet length]
  (let [letters (vec (set alphabet))
        letr_count (count letters)
        init []
        words_count (* letr_count (pow (dec letr_count) (dec length)))]
    (loop [word init
           i (dec words_count)
           pos 0
           res '()
           prev_index letr_count
           prev_div nil]
      (cond
        (or (<= length 0) (neg? i)) res
        (= pos length) (recur init (dec i) 0 (conj res word) letr_count nil)
        :else (let [result (get_letr letr_count words_count i pos prev_index prev_div)
                    div (first result)
                    letr_index (second result)
                    new_word (conj word (get letters letr_index))]
                (recur new_word i (inc pos) res letr_index div))))))

;1.3
(defn my-map [f coll]
  (reduce (fn [acc item]
            (cons item acc)) ; reverse
    '()
    (reduce (fn [acc item]
              (cons (f item) acc))
      '()
            coll)))


(defn my-filter [pred coll]
  (reduce (fn [acc item]
            (cons item acc)) ; reverse
    '()
    (reduce (fn [acc item]
              (if (pred item) (cons item acc)
                           acc))
      '()
            coll)))


;1.4
(defn permute2 [alphabet length]
  (if (<= length 0) ()
                    (let [contains (fn [item l]
                                     (reduce (fn [acc cur]
                                               (if (= item cur) true
                                                                acc))
                                             false l))
                          builder (fn [oldList] (reduce (fn [result item]
                                                          (concat item result))
                                                        ()
                                                        (map (fn [oldWord]
                                                               (reduce (fn [acc letter]
                                                                         (if (= letter (first oldWord)) acc
                                                                                                        (cons (cons letter oldWord) acc)))
                                                                       ()
                                                                       alphabet))
                                                             oldList)))]
                      (map (fn [x]
                             (apply str x)) ;(a b c) => abc
                           (reduce (fn [acc i]
                                     (builder acc))
                                   '(())
                                   (range length))))))