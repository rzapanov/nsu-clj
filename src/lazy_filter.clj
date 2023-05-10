(ns lazy-filter
  (:require [primes :as PS])
  (:require [clojure.test :as test]))

(defn part
  [n coll]
  (if (empty? coll)
    coll
    (lazy-seq
      (let [h (doall (take n coll))]
        (if (= n (count h))
          (cons h (part n (nthrest coll n)))
          (list h))))))

(defn gen-filter
  [granularity parallelism]
  {:pre [(and (integer? granularity) (integer? parallelism))]}
  (fn [pred coll]
    (letfn [(my-filter' [by-blocks]
              (let [fs (doall (take parallelism by-blocks))]
                (concat (->> (rest fs)                        ; calculate (first fs) on current thread
                           (map (fn [f] (future (doall (filter pred f)))))
                           (doall)
                           (map (fn [f] @f))
                           (cons (filter pred (first fs))))
                      (let [dropped (drop parallelism by-blocks)]
                        (if (empty? dropped)
                          dropped
                          (lazy-seq (my-filter' dropped)))))))]
      (let [partitioned (part granularity coll)
            filtered-blocks (my-filter' partitioned)]
        (take-while (fn [x] (not (nil? x)))
                    (->> [(rest filtered-blocks) (first filtered-blocks)]
                         (iterate (fn [[blocks seq]]
                                    (let [next-seq (rest seq)]
                                      (if (empty? next-seq)
                                        [(rest blocks) (first blocks)]
                                        [blocks next-seq]))))
                         (map (fn [[_ x]] (first x)))))))))

(def my-filter (gen-filter 60 4))

(defn primes-to
  [n]
  (letfn [(next-prime [cs]
            (let [p (first cs)]
              (if (> p (Math/sqrt n))
                cs
                (cons p (doall (next-prime (my-filter
                                                (fn [x]
                                                  (Thread/sleep 1)
                                                  (not (= 0 (mod x p))))
                                                cs)))))))]
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
  [_]
  (map (fn [x] (first (nth x 0))) (iterate PS/step (list (PS/primes-to 10) 10 0))))

(defn construct-primes-parallel
  [_]
  (map (fn [x] (first (nth x 0))) (iterate step (list (PS/primes-to 10) 10 0))))

(defn take-drop
  [n coll]
  (drop n (doall (take n coll))))

(println "sequential" (time (take-drop 300 (construct-primes-single ()))))
(println "parallel" (time (take-drop 300 (construct-primes-parallel ()))))
(println "sequential" (time (take-drop 300 (construct-primes-single ()))))
(println "parallel" (time (take-drop 300 (construct-primes-parallel ()))))
;(println (time (take-drop 10000 (construct-primes-single ()))))
;(println (time (take-drop 10000 (construct-primes-parallel ()))))
;(println (time (take-drop 10000 (construct-primes-single ()))))
;(println (time (take-drop 10000 (construct-primes-parallel ()))))
;(println (time (take-drop 10000 (construct-primes-single ()))))
;(println (time (take-drop 10000 (construct-primes-parallel ()))))

(test/deftest filter-tests
  (let [nats (iterate inc' 0)
        fs (take 10000 nats)]
    (test/is (= (filter even? fs) (my-filter even? fs)))
    (test/is (= (filter odd? fs) (my-filter odd? fs)))
    (let [pred (fn [x] (= (mod x 7) 0))]
      (test/is (= (filter pred fs) (my-filter pred fs))))))
