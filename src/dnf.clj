(ns dnf
  (:require [clojure.test :as test]))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn const [val]
  {:pre [(boolean? val)]}
  (list ::const val))

(defn const? [expr]
  (= (first expr) ::const))

(defn terminal? [expr]
  (or (variable? expr) (const? expr)))

(defn variable-name [v]
  (second v))

(defn and' [expr & rest]
  (cons ::and (cons expr rest)))

(defn and? [expr]
  (= (first expr) ::and))

(defn or' [expr & rest]
  (cons ::or (cons expr rest)))

(defn or? [expr]
  (= (first expr) ::or))

(defn inj [expr & rest]
  (cons ::inj (cons expr rest)))

(defn inj? [expr]
  (= (first expr) ::inj))

(defn ng [expr]
  (list ::ng expr))

(defn ng? [expr]
  (= (first expr) ::ng))

(defn args [expr]
  (rest expr))

(defn tpe [expr]
  (first expr))

(defn translate
  [translator expr]
  (let [top (translator expr)
        top-type (tpe top)
        top-args (args top)]
    (if (terminal? top)
      top
      (cons top-type
            (map (fn [x] (translate translator x))
                 top-args)))))

(defn rule-translator
  [rules]
  (fn [expr] (let [transformation (some
                                    (fn [[p t]] (if (p expr) t false))
                                    rules)]
               (transformation expr))))

(def binary-form-rules
  (letfn [(transform [bop xs] (reduce (fn [acc x] (bop acc x)) xs))]
    (list
      [and? (fn [expr] (->> expr args (transform and')))]
      [or? (fn [expr] (->> expr args (transform or')))]
      [inj? (fn [expr] (->> expr args reverse (transform (fn [x y] (inj y x)))))]
      [any? identity])))

(def elimination-rules
  (list
    [inj? (fn [expr] (let [[l r] (args expr)]
                       (or' (ng l) r)))]
    [any? identity]))

(def negation-propagation-rules
  (let [propagation-rules
        (list
          [terminal? (fn [expr] (ng expr))]
          [ng? (fn [expr] (first (args expr)))]
          [and? (fn [expr] (let [[l r] (args expr)]
                             (or' (ng l) (ng r))))]
          [or? (fn [expr] (let [[l r] (args expr)]
                            (and' (ng l) (ng r))))]
          [any? (fn [x] (throw (concat x "should not reach here @ propagation rules")))])]
    (list
      [ng? (fn [expr] (let [sub-expr (first (rest expr))
                            rule (some
                                   (fn [[p t]] (if (p sub-expr) t false))
                                   propagation-rules)]
                        (rule sub-expr)))]
      [any? identity])))

(defn substitution-rules
  [var new-val]
  {:pre [(variable? var)]}
  (list
    [variable? (fn [expr] (if (= (variable-name var) (variable-name expr))
                            new-val
                            expr))]
    [any? identity]))

(def multiplication-rules
  (list
    [and? (fn [expr]
            (let [[l r] (args expr)]
              (if (or? l)
                (let [[cl cr] (args l)] (or'
                                          (and' cl r)
                                          (and' cr r)))
                (if (or? r)
                  (let [[cl cr] (args r)] (or'
                                            (and' l cl)
                                            (and' l cr)))
                  expr))))]
    [any? identity]))

(defn translate-whole
  [expr]
  (->> expr
       (translate (rule-translator binary-form-rules))
       (translate (rule-translator elimination-rules))
       (translate (rule-translator negation-propagation-rules))
       (translate (rule-translator multiplication-rules))
       ))

(test/deftest dnf-tests
  (test/is (= (inj (variable :x)
                   (inj (variable :y)
                        (variable :z)))
              (translate
                (rule-translator binary-form-rules)
                (inj (variable :x)
                     (variable :y)
                     (variable :z)))))
  (test/is (= (or' (ng (variable :x))
                   (variable :y))
              (translate
                (rule-translator elimination-rules)
                (inj (variable :x)
                     (variable :y)))))
  (test/is (= (translate
                (rule-translator negation-propagation-rules)
                (ng (and' (variable :x)
                          (variable :y))))
              (or' (ng (variable :x))
                   (ng (variable :y)))))
  (test/is (= (translate
                (rule-translator negation-propagation-rules)
                (ng (ng (ng (variable :x)))))
              (ng (variable :x))))
  (test/is (= (translate
                (rule-translator negation-propagation-rules)
                (ng (and' (ng (variable :x))
                          (ng (variable :y)))))
              (or' (variable :x) (variable :y))))
  (test/is (= (translate
                (rule-translator (substitution-rules (variable :x) (const false)))
                (and' (variable :x) (const true)))
              (and' (const false) (const true))))
  (test/is (= (translate-whole (ng (inj (variable :x)
                                        (variable :y)
                                        (variable :z))))
              (and' (variable :x)
                    (and' (variable :y)
                          (ng (variable :z))))))
  (let [x (variable :x)
        y (variable :y)
        z (variable :z)
        w (variable :w)
        nx (ng x)
        ny (ng y)
        nz (ng z)
        nw (ng w)] (test/is (= (translate-whole (ng (or' (and' x y)
                                                         (and' z w))))
                               (or' (or' (and' nx nz)
                                         (and' nx nw))
                                    (or' (and' ny nz)
                                         (and' ny nw)))))))
