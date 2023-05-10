(ns primes
  (:require [clojure.test :as test]))
(defn primes-to
  [n]
  (letfn [(next-prime [cs]
            (let [p (first cs)]
              (if (> p (Math/sqrt n))
                cs
                (cons p (lazy-seq (next-prime (filter
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

(def primes
  (map (fn [x] (first (nth x 0))) (iterate step (list (primes-to 10) 10 0))))

;(println (time (take 40000 primes)))

(test/deftest primes-tests
  (letfn [(prime? [x]
                  (empty? (filter (fn [v] (= (mod x v) 0)) (range 2 x))))]
    (test/testing "Testing primes"
      (test/is (= (primes-to 10) `(2 3 5 7)))
      (test/is (= (primes-to 20) `(2 3 5 7 11 13 17 19)))
      (test/is (empty? (filter (fn [x] (not (prime? x))) (primes-to 3000))))
      (test/is (= (take 4 primes) (primes-to 10)))
      (test/is (= (take 8 primes) (primes-to 20)))
      (test/is (empty? (filter (fn [x] (not (prime? x))) (take 200 primes))))
      (test/is (= (primes-to 1000) (filter prime? (range 2 1000))))
      (test/is (let [bps (take 2000 primes)]
                 (= (primes-to (+ 1 (last bps))) bps))))))