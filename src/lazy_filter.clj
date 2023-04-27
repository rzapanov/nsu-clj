(ns lazy-filter
  (:require [primes :as PS]))

(defn gen-filter
  [granularity parallelism]
  {:pre [(and (integer? granularity) (integer? parallelism))]}
  (letfn [(my-filter [pred coll]
            (let [g granularity
                  by-blocks' (iterate (fn [[_ y]]
                                        [(take g y) (drop g y)])
                                      [(take g coll) (drop g coll)])
                  by-blocks (map (fn [[x _]] x) by-blocks')]
              (letfn [(my-filter' [by-blocks]
                        (let [fs (doall (take parallelism by-blocks))
                              rfs (drop parallelism by-blocks)
                              futures (doall (map
                                               (fn [f] (future
                                                         (doall (filter pred f))))
                                               (rest fs)))
                              get-futures (cons (filter pred (first fs))
                                                (map (fn [f] @f) futures))
                              head (reduce concat get-futures)]
                          (cons head (lazy-seq (my-filter' rfs)))))]
                (let [filtered-blocks (my-filter' by-blocks)]
                  (take-while (fn [x] (not (nil? x)))
                              (map (fn [[_ x]] (first x))
                                   (iterate (fn [[blocks seq]]
                                              (let [next-seq (rest seq)]
                                                (if (empty? next-seq)
                                                  [(rest blocks) (first blocks)]
                                                  [blocks next-seq])))
                                            [(rest filtered-blocks) (first filtered-blocks)])))))))]
    my-filter))

;(println (let [my-filter (gen-filter 16 4)
;               evens (my-filter even? (take 1000 (iterate inc' 0)))]
;           (println (take 1238 evens) "@@@")))
;
(def my-filter (gen-filter 1024 4))

(defn primes-to
  [n]
  (letfn [(next-prime [cs]
            (let [p (first cs)]
              (if (> p (Math/sqrt n))
                cs
                (cons p (lazy-seq (next-prime (my-filter (fn [x] (not (= 0 (mod x p)))) cs)))))))]
    (next-prime (range 2 (inc n)))))

(defn step
  ([tuple] (let [[primes bound count] tuple] (step primes bound count)))
  ([ps-to-bound bound count]
   (let [next-count (inc' count)]
     (if (empty? (rest ps-to-bound))
       (let [new-bound (* 2 bound)]
         (list (drop next-count (primes-to new-bound)) new-bound next-count))
       (list (rest ps-to-bound) bound next-count)))))

(defn construct-primes-single
  [x]
  (map (fn [x] (first (nth x 0))) (iterate PS/step (list (PS/primes-to 10) 10 0))))

(defn construct-primes-parallel
  [x]
  (map (fn [x] (first (nth x 0))) (iterate step (list (primes-to 10) 10 0))))

(defn take-drop
  [n coll]
  (drop n (take n coll)))

(println (take-drop 10000 (construct-primes-single ())))
(println (take-drop 10000 (construct-primes-parallel ())))
(println (time (take-drop 10000 (construct-primes-single ()))))
(println (time (take-drop 10000 (construct-primes-parallel ()))))
(println (time (take-drop 10000 (construct-primes-single ()))))
(println (time (take-drop 10000 (construct-primes-parallel ()))))
(println (time (take-drop 10000 (construct-primes-single ()))))
(println (time (take-drop 10000 (construct-primes-parallel ()))))
