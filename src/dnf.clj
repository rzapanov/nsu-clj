(ns dnf
  (:require [clojure.test :as test]))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))

(defn and [expr & rest]
  (cons ::and (cons expr rest)))

(defn and? [expr]
  (= (first expr) ::and))

(defn or [expr & rest]
  (cons ::or (cons expr rest)))

(defn or? [expr]
  (= (first expr) ::or))

(defn inj [expr & rest]
  (cons ::inj (cons expr rest)))

(defn inj? [expr]
  (= (first expr) ::inj))

(defn inv [expr]
  (list ::inv expr))

(defn inv? [expr]
  (= (first expr) ::inv))

(defn get-cons
  [expr]
  (let [cs (list
             [variable? variable]
             [and? and]
             [or? or]
             [inj? inj]
             [inv? inv])]
    (some (fn [[p c]] (if (p expr) c false)) cs) expr))

(defn args [expr]
  (rest expr))

(def binary-form-rules
  (letfn [(transform [bop xs] (reduce (fn [acc x] (bop acc x)) xs))]
    (list
      [and? (fn [expr] (->> expr args (transform and)))]
      [or? (fn [expr] (->> expr args (transform or)))]
      [inj? (fn [expr] (->> expr args reverse (transform (fn [x y] (inj y x)))))]
      [any? identity])))

(defn translate
  [expr translator]
  (->> (translator expr)
       args
       (map translator)
       (fn [x] ((get-cons expr) x))))

(defn rule-translator
  [rules]
  (fn [expr] ((some (fn [[p t]] (if (p expr) t false)) rules) expr)))

(println ((get-cons (variable :x)) :z))
(println ((rule-translator binary-form-rules) (inj (variable :x) (variable :y) (variable :z))))
(println (translate (inj (variable :x) (variable :y) (variable :z)) (rule-translator binary-form-rules)))
;(println (translate (inj :x :y :z :q) (rule-translator binary-form-rules)))

;(println ((second (nth binary-form-rules 2)) (inj :x :y :z :q)))
;(println ((fn [expr] (reduce (fn [acc x] (and acc x)) (args expr))) (and :x :y :z :q)))
;(println ((fn [xs] (->> xs
;                        reverse
;                        reverse))) (inj :x :y :z :q))
;(def only-and-or-neg
;  (list
;    [inj? ]
;    [pred2 transform2]
;    ...))