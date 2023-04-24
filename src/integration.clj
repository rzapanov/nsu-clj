(ns integration
  (:require [clojure.test :as test]))

(defn integrate-seg
  [f a b step]
  (let [dots (take (int (/ (- b a) step)) (iterate (fn [x] (+ x step)) a))]
    (reduce + (map (fn [x] (let [y (+ x step)]
                             (/ (* (+ (f x) (f y)) (- y x)) 2))) dots))))

(defn integration-sequence
  [f step granularity]
  (let [integrated-segments (map (fn [x] (integrate-seg f x (+ x granularity) step)) (iterate (fn [x] (+ x granularity)) 0))]
    (map first (iterate (fn [[acc xs]] [(+ acc (first xs)) (rest xs)]) [0.0 integrated-segments]))))


(defn pintegrator
  ([f step granularity]
   (let [prefixes (integration-sequence f step granularity)]
     (fn [x]
       (let [rounded-down (long (/ x granularity))]
         (+ (nth prefixes rounded-down) (integrate-seg f (* rounded-down granularity) x step))))))
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
        close (fn [x y e] (< (abs (- x y)) e))
        ]
    (let [step 1e-2 e 1e-2
          pow2int (integrator pow2 step 3)
          polyint (integrator poly step 3)
          sinint (integrator (fn [x] (Math/sin x)) step 1)
          maxpow2'' 2
          maxsin'' 1
          maxpoly'' 2
          eps (fn [a b step m] (* m (* (- b a) step)))
          test-close (fn [f g a b step m] (close (f a b) (g a b) (eps a b step m)))]
      (let [delta-pow3 (fn [a b] (/ (- (pow3 b) (pow3 a)) 3))
            segment-pow2 (fn [a b] (integrate-seg pow2 a b step))
            integrator-pow2 (fn [a b] (- (pow2int b) (pow2int a)))
            test-close-pow2 (fn [f g a b] (test-close f g a b step maxpow2''))]
        (test/is (test-close-pow2 delta-pow3 integrator-pow2 0 23.03057774))
        (test/is (test-close-pow2 delta-pow3 segment-pow2 -10 20))
        (test/is (test-close-pow2 delta-pow3 integrator-pow2 -10 20)))
      (let [seg-poly (fn [a b] (integrate-seg poly a b step))
            integrator-poly (fn [a b] (- (polyint b) (polyint a)))
            test-close-poly (fn [a b] (test-close seg-poly integrator-poly a b step maxpoly''))]
        (test/is (test-close-poly 0 10))
        (test/is (test-close-poly 0 18.24))
        (test/is (test-close-poly -3 3)))
      (let [sin (fn [x] (Math/sin x))
            seg-sin (fn [a b] (integrate-seg sin a b step))
            integrator-sin (fn [a b] (- (sinint b) (sinint a)))
            test-close-sin (fn [a b] (test-close seg-sin integrator-sin a b step maxsin''))
            test-close-sin' (fn [a b] (test-close integrator-sin (fn [a b] (- (Math/cos a) (Math/cos b))) a b step maxsin''))]
        (test/is (test-close-sin 0 Math/PI))
        (test/is (test-close-sin 0 18.24))
        (test/is (test-close-sin' -3 3))
        (test/is (test-close-sin' 0 Math/PI))))))
