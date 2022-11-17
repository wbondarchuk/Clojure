(ns task3.core)

(defn heavy-even? [x]
  (Thread/sleep 100)
  (even? x))

(defn makePartsList [coll n]
  (if (empty? coll) ()
                    (concat (list (take n coll)) (makePartsList (drop n coll) n))))

(defn connectParts [coll]
  (if (empty? coll) ()
                    (concat (first coll) (connectParts (rest coll)))))

(defn parallelFilterChunk [pred coll n]
  (->> (makePartsList coll n)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (connectParts)))

(defn parallelFilterThreads [pred coll threads]
  (let [collSize (count coll)]
    (if (== (rem collSize threads) 0)
      (parallelFilterChunk pred coll (quot collSize threads))
      (parallelFilterChunk pred coll (inc (quot collSize threads))))))

(defn parallelFilter
  ([pred coll] (parallelFilterThreads pred coll (.. Runtime (getRuntime) (availableProcessors))))
  ([pred coll threads] (parallelFilterThreads pred coll threads)))

(defn -main []
  (time (doall (filter even? (range 100))))
  (time (doall (parallelFilter even? (range 100))))
  (time (doall (parallelFilter even? (range 100) 100)))
  (time (doall (filter heavy-even? (range 100))))
  (time (doall (parallelFilter heavy-even? (range 100))))
  (time (doall (parallelFilter heavy-even? (range 100) 100))))

