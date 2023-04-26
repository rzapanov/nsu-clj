(ns dnf
  (:require [clojure.test :as test]))

; region nodes
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

(defn conjunction [expr & rest]
  (cons ::and (cons expr rest)))

(defn conjunction? [expr]
  (= (first expr) ::and))

(defn disjunction [expr & rest]
  (cons ::or (cons expr rest)))

(defn disjunction? [expr]
  (= (first expr) ::or))

(defn implication [expr & rest]
  (cons ::inj (cons expr rest)))

(defn implication? [expr]
  (= (first expr) ::inj))

(defn negation [expr]
  (list ::ng expr))

(defn negation? [expr]
  (= (first expr) ::ng))
; endregion

(defn args [expr]
  (rest expr))

(defn tpe [expr]
  (first expr))

(defn translate
  ^{:doc "Transforms expression with given translator.
          Translation proceeds recursively in
          left-to-right order down a tree,
          constructing translated nodes by overwriting existing ones."}
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
  ^{:doc "Creates instance of translator with given rules"}
  [rules]
  (fn [expr] (let [transformation (some
                                    (fn [[p t]] (if (p expr) t false))
                                    rules)]
               (transformation expr))))

(def binary-form-rules
  ^{:doc "Translation rules for transformation expression in binary form.
          E.g. expression like (inj x y z) will be transformed in (inj x (inj y z))"}
  (letfn [(transform [bop xs] (reduce (fn [acc x] (bop acc x)) xs))]
    (list
      [conjunction? (fn [expr] (->> expr args (transform conjunction)))]
      [disjunction? (fn [expr] (->> expr args (transform disjunction)))]
      [implication? (fn [expr] (->> expr args reverse (transform (fn [x y] (implication y x)))))]
      [any? identity])))

(def elimination-rules
  ^{:doc "Translation rules for elimination non-standard operations"}
  (list
    [implication? (fn [expr] (let [[l r] (args expr)]
                               (disjunction (negation l) r)))]
    [any? identity]))

(def negation-propagation-rules
  ^{:doc "Translation rules for negation propagation from binary expressions to it's operands"}
  (let [propagation-rules
        (list
          [terminal? (fn [expr] (negation expr))]
          [negation? (fn [expr] (first (args expr)))]
          [conjunction? (fn [expr] (let [[l r] (args expr)]
                                     (disjunction (negation l) (negation r))))]
          [disjunction? (fn [expr] (let [[l r] (args expr)]
                                     (conjunction (negation l) (negation r))))]
          [any? (fn [x] (throw (concat x "should not reach here @ propagation rules")))])]
    (list
      [negation? (fn [expr] (let [sub-expr (first (rest expr))
                            rule (some
                                   (fn [[p t]] (if (p sub-expr) t false))
                                   propagation-rules)]
                        (rule sub-expr)))]
      [any? identity])))

(defn substitution-rules
  ^{:doc "Translation rules for variable substitution"}
  [var new-val]
  {:pre [(variable? var)]}
  (list
    [variable? (fn [expr] (if (= (variable-name var) (variable-name expr))
                            new-val
                            expr))]
    [any? identity]))

(def multiplication-rules
  ^{:doc "Translation rules for transformation expression to DNF"}
  (list
    [conjunction? (fn [expr]
            (let [[l r] (args expr)]
              (if (disjunction? l)
                (let [[cl cr] (args l)] (disjunction
                                          (conjunction cl r)
                                          (conjunction cr r)))
                (if (disjunction? r)
                  (let [[cl cr] (args r)] (disjunction
                                            (conjunction l cl)
                                            (conjunction l cr)))
                  expr))))]
    [any? identity]))

(defn translate-to-dnf
  ^{:doc "Translates expression to DNF"}
  [expr]
  (->> expr
       (translate (rule-translator binary-form-rules))
       (translate (rule-translator elimination-rules))
       (translate (rule-translator negation-propagation-rules))
       (translate (rule-translator multiplication-rules))))

(let [x (variable :x)
      y (variable :y)
      z (variable :z)
      w (variable :w)
      nx (negation x)
      ny (negation y)
      nz (negation z)
      nw (negation w)]
  (test/deftest dnf-tests
    (test/is (= (implication x (implication y z))
                (translate
                  (rule-translator binary-form-rules)
                  (implication x y z))))
    (test/is (= (disjunction nx y)
                (translate
                  (rule-translator elimination-rules)
                  (implication x y))))
    (test/is (= (translate
                  (rule-translator negation-propagation-rules)
                  (negation (conjunction x y)))
                (disjunction nx ny)))
    (test/is (= (translate
                  (rule-translator negation-propagation-rules)
                  (negation (negation nx)))
                nx))
    (test/is (= (translate
                  (rule-translator negation-propagation-rules)
                  (negation (conjunction nx ny)))
                (disjunction x y)))
    (test/is (= (translate
                  (rule-translator (substitution-rules x (const false)))
                  (conjunction x (const true)))
                (conjunction (const false) (const true))))
    (test/is (= (translate-to-dnf (negation (implication x y z)))
                (conjunction x (conjunction y nz))))
    (test/is (= (translate-to-dnf (negation (disjunction (conjunction x y)
                                                         (conjunction z w))))
                (disjunction (disjunction (conjunction nx nz)
                                          (conjunction nx nw))
                             (disjunction (conjunction ny nz)
                                          (conjunction ny nw)))))))
