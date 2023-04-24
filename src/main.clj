(ns main)

(defn my-reverse [xs]
  (letfn [(helper [ys acc]
            (if (empty? ys)
                acc
                (recur (rest ys) (cons (first ys) acc))))]
    (helper xs ())))

(defn my-map-rec [f xs]
  (letfn [(helper [g ys acc]
            (if (empty? ys)
              (my-reverse acc)
              (recur g (rest ys) (cons (g (first ys)) acc))))]
    (helper f xs ())))

;(defn cart
;  ([xs ys map-fun]
;   (letfn [(helper [xs ys acc]
;             (if (empty? xs)
;               acc
;               (recur (rest xs) ys (concat (map-fun (fn [y] (concat (first xs) y)) ys) acc))))]
;     (helper xs ys ())))
;  ([xs ys] (cart xs ys my-map-rec)))

(defn cart
  ([xs ys map-fun]
   (reduce (fn [acc x] (concat (map-fun (fn [y] (concat x y)) ys) acc)) () xs))
  ([xs ys] (cart xs ys my-map-rec)))

(defn all-combinations
  ([n xs cart-fun]
   (reduce (fn [acc _] (cart-fun acc xs)) xs (range (dec n))))
  ([n xs]
   (all-combinations n xs cart)))

(defn my-filter-rec [p xs]
  (letfn [(helper [p xs acc]
            (if (empty? xs)
              (my-reverse acc)
              (recur
                p
                (rest xs)
                (if (p (first xs))
                   (cons (first xs) acc)
                   acc))))]
    (helper p xs ())))

;(defn have-two-subsequent [s]
;  (letfn [(helper [ys previous]
;            (if (empty? ys)
;              false
;              (if (= previous (first ys))
;                true
;                (recur (rest ys) (first ys)))))]
;    (if (empty? s)
;      false
;      (helper (rest s) (first s)))))

(defn have-two-subsequent
  [s]
  (->> (map (fn [x y] [x y]) s (rest s))
       (reduce (fn [b [x y]] (or b (= x y))) false)))

(defn task
  ([cart-fun n xs]
   (filter
     (fn [s] (not (have-two-subsequent s)))
     (all-combinations n xs (fn [xs ys] (cart-fun xs ys)))))
  ([filter-fun map-fun n xs]
   (filter-fun
     (fn [s] (not (have-two-subsequent s)))
     (all-combinations n xs (fn [xs ys] (cart xs ys map-fun))))))

(println (task my-filter-rec my-map-rec 2 `("a" "b" "c")))
(println (task my-filter-rec my-map-rec 3 `("a" "b" "c")))

(defn my-map [f xs]
  (my-reverse (reduce (fn [acc x] (cons (f x) acc)) () xs)))

(defn my-filter [p xs]
  (my-reverse (reduce (fn [acc x] (if (p x) (cons x acc) acc)) () xs)))

(println (task my-filter-rec my-map 2 `("a" "b" "c")))
(println (task my-filter my-map 2 `("a" "b" "c")))
(println (task filter map 2 `("a" "b" "c")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cart4
  [xs ys]
  (reduce (fn [acc x] (concat (map (fn [y] (concat x y)) ys) acc)) () xs))

(println (task cart4 2 `("a" "b" "c")))
