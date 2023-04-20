(ns integration
  (:require [clojure.test :as test]))

(defn integrate-seg
  [f a b step]
  (let [dots (take (int (/ (- b a) step)) (iterate (fn [x] (+ x step)) a))]
    (reduce + (map (fn [x] (let [y (+ x step)]
                             (/ (* (+ (f x) (f y)) (- y x)) 2))) dots))))

(defn integration-sequence
  [f step granularity]
  (let [integrated-segments (map (fn [x] (integrate-seg f x (inc' x) step)) (iterate (fn [x] (+ x granularity)) 0))]
    (map first (iterate (fn [[acc xs]] [(+ acc (first xs)) (rest xs)]) [0.0 integrated-segments]))))


(defn pintegrator
  ([f step granularity]
   (let [prefixes (integration-sequence f step granularity)]
     (fn [x]
       (let [rounded-down (long x)]
         (+ (nth prefixes rounded-down) (integrate-seg f rounded-down x step))))))
  ([f step] (pintegrator f step 1)))

(defn integrator
  ([f step granularity]
   (let [p (pintegrator f step granularity) n (pintegrator (fn [x] (f (- x))) step granularity)]
     (fn [x]
       (if (pos? x)
         (p x)
         (- (n (- x)))))))
  ([f step] (integrator f step 1)))

(defn abs [x] (if (neg? x) (- x) x))

(defn invsq
  [x]
  (/ 1 (* (+ (abs x) 1) (+ (abs x) 1))))

(def cint
  (integrator invsq 0.01))

(println (time (cint 100)))
(println (time (cint 99)))
(println (time (cint 105)))
(println (time (cint 105.5)))
(println (time (cint 105.1)))
(println (time (cint 105.9)))
(println (time (cint 66.1)))
(println (time (cint -105.9)))
(println (time (cint -77.9)))


(test/deftest integration-tests
  (let [pow2 (fn [x] (* x x))
        pow3 (fn [x] (* x (pow2 x)))
        poly (fn [x] (+ (pow2 x) (- x 1)))
        close (fn [x y e] (< (abs (- x y)) e))]
    (let [step 1e-5 e 1e-3
          pow2int (integrator pow2 step 3)
          polyint (integrator poly step 3)
          sinint (integrator (fn [x] (Math/sin x)) step 0.2)
          cl (fn [x y] (close x y e))]
      (test/is (cl (/ (- (pow3 20) (pow3 -10)) 3) (integrate-seg pow2 -10 20 step)))
      (test/is (cl (/ (- (pow3 20) (pow3 -10)) 3) (- (pow2int 20) (pow2int -10))))
      (test/is (let [ub 100]
                 (cl (pow2int ub) (/ (pow3 ub) 3))))
      (test/is (let [ub 10]
                 (cl (polyint ub) (integrate-seg poly 0 ub step))))
      (test/is (let [ub 18.24]
                 (cl (polyint ub) (+ (integrate-seg pow2 0 ub step) (integrate-seg (fn [x] (- x 1)) 0 ub step)))))
      (test/is (cl (sinint Math/PI) 2))
      (test/is (cl (sinint (- Math/PI)) -2))
      (test/is (cl (sinint (* 2 Math/PI)) 0))
      (test/is (cl (integrate-seg (fn [x] (+ (Math/sin x) (poly x))) 0 10 step) (+ (polyint 10) (sinint 10)))))))
