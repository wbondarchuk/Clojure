(ns task1.core)

;1.2
(defn compareAndAdd [inList word outList]
  (if (= (first inList) nil)
    outList
    (if (= (first inList) word)
      (recur (rest inList) word outList)
      (recur (rest inList) word (cons (str (first inList) word) outList)))))

(defn nextWord [inList word outList]
  (if (= (first word) nil)
    outList
    (recur inList (rest word) (concat (compareAndAdd inList (first word) nil) outList))))

(defn permuteAll [inList outList length]
  (if (= length 1)
    outList
    (recur inList (nextWord inList outList '()) (dec length))))

(defn permute [alphabet length]
  (if (<= length 0)
    '()
    (permuteAll alphabet alphabet length)))

;1.3
(defn my-map [f coll]
  (reduce (fn [acc item]
            (concat acc [(f item)]))
          '()
          coll))

(defn my-filter [pred coll]
  (reduce (fn [acc item]
            (concat acc (if (pred item)
                          [item]
                          [])))
          '()
          coll))

(defn my-filter2 [pred coll]
  (reverse (reduce (fn [acc item]
                     (if (pred item)
                       (cons item acc)
                       acc))
                   '()
                   coll)))



;1.4
(defn permute2 [alphabet length]
  (if (<= length 0)
    '()
    (let [builder (fn [oldList]
                    (reduce (fn [result item]
                              (concat item result))
                            '()
                            (my-map
                              (fn [oldWord]
                                (my-map
                                  (fn [symbol]
                                    (cons symbol oldWord))
                                  (my-filter (fn [letter]
                                               (not (= letter (first oldWord))) )
                                     alphabet)))
                              oldList)))]
      (my-map (fn [x]
                (apply str x))
              (reduce (fn [acc i]
                        (builder acc))
                      '(())
                      (range length))))))