(ns task2.core)

(defn area [f start end]
  (* (- end start) (* 0.5 (+ (f start) (f end)))))

(defn areaByIndices [f start end delta]
  (* (* (- end start) delta)
     (* 0.5 (+ (f (* delta start)) (f (* delta end))))))


(defn steps [end delta]
  (int (/ end delta)))

(defn remains [end delta]
  (- end (* delta (steps end delta))))

(defn lin [x] x)
(defn sq [x] (* x x))
(defn cube [x] (Math/pow x 3))
(defn sqrt [x] (Math/sqrt x))


;; Без мемоизации
(defn integralSteps [f steps delta]
  "Интегрируем функцию в диапазоне [0; delta * steps]"
  (loop [n steps
         res 0]
    (if (= n 0) 0
                (if (= n 1) (+ res (areaByIndices f 0 1 delta))
                            (recur (dec n) (+ res (areaByIndices f (dec n) n delta)))))))

(defn integrateTo [f integrator end delta]
  "Интегрируем функцию f в диапозоне [0; end] с заданным dx"
  "Разделим диапозон [0; end] на N частей длиной delta, одна часть будет короче"
  (if (neg? end) (println "Incorrect end!")
                 (+ (area f (- end (remains end delta)) end)
                    (integrator f (steps end delta) delta))))

(defn integrate [f]
  (let [delta 1e-5]
    (fn [x]
      (integrateTo f integralSteps x delta))))

;; Memoized version without tailed recursion
;
(def memIntegralStepsRecur
  (memoize (fn [f steps delta res]
             (if (= steps 1)
               (+ res (areaByIndices f 0 1 delta))
               (memIntegralStepsRecur
                 f (dec steps) delta (+ res (areaByIndices f (dec steps) steps delta)))))))

(defn memIntegralSteps  [f steps delta]
  (memIntegralStepsRecur f steps delta 0))

(defn memIntegrate [f]
  (let [delta 0.01]
    (fn [x]
      (integrateTo f memIntegralSteps x delta))))

(defn -main [& args]
  (time ((integrate lin) 10))
  (time ((integrate lin) 10))
  (time ((integrate lin) 10))
  (time ((memIntegrate lin) 10))
  (time ((memIntegrate lin) 10))
  (time ((integrate sq) 10))
  (time ((integrate sq) 10))
  (time ((memIntegrate sq) 10))
  (time ((memIntegrate sq) 10))
  (time ((integrate cube) 10))
  (time ((integrate cube) 10))
  (time ((memIntegrate cube) 10))
  (time ((memIntegrate cube) 10))
  (time ((integrate sqrt) 10))
  (time ((integrate sqrt) 10))
  (time ((memIntegrate sqrt) 10))
  (time ((memIntegrate sqrt) 10)))
