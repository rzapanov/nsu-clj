(ns task-c7)

;;;an empty route map
;;;it is enough to use either forward or backward part (they correspond to each other including shared reference to number of tickets)
;;;:forward is a map with route start point names as keys and nested map as values
;;;each nested map has route end point names as keys and route descriptor as values
;;;each route descriptor is a map (structure in fact) of the fixed structure where 
;;;:price contains ticket price
;;;and :tickets contains reference to tickets number
;;;:backward has the same structure but start and end points are reverted 
(def empty-map
  {:forward {},
   :backward {}})

(defn route
  "Add a new route (route) to the given route map
   route-map - route map to modify
   from - name (string) of the start point of the route
   to - name (string) of the end poiunt of the route
   price - ticket price
   tickets-num - number of tickets available"
  [route-map from to price tickets-num]
  (let [tickets (ref tickets-num :validator (fn [state] (>= state 0))),     ;reference for the number of tickets 
        orig-source-desc (or (get-in route-map [:forward from]) {}),
        orig-reverse-dest-desc (or (get-in route-map [:backward to]) {}),
        route-desc {:price price,                                            ;route descriptor
                   :tickets tickets},
        source-desc (assoc orig-source-desc to route-desc),
        reverse-dest-desc (assoc orig-reverse-dest-desc from route-desc)]
    (-> route-map
      (assoc-in [:forward from] source-desc)
      (assoc-in [:backward to] reverse-dest-desc))))

; LCG
(defn step-rnd
  [x]
  (mod (clojure.lang.Numbers/unchecked_add 134775813 (clojure.lang.Numbers/unchecked_multiply 131231317 x)) (bit-shift-left 1 31)))

(defn step-rnd2
  [x]
  (mod (clojure.lang.Numbers/unchecked_add 134775813 (clojure.lang.Numbers/unchecked_multiply 76378981 x)) (bit-shift-left 1 31)))

(defn left-child
  [c] (nth c 1))

(defn right-child
  [c] (nth c 2))

(defn key
  [c] (nth c 0))

(defn data
  [c] (nth c 3))

(defn skew-merge
  [l r]
  (if (nil? l)
    r (if (nil? r)
        l (if (>= (key l) (key r))
            (recur r l)
            [(key l) (skew-merge r (right-child l)) (left-child l)]))))

(defn meld-heap
  [l r rnd]
  (let [min (fn [x y] (if (< (key x) (key y)) x y))
        max (fn [x y] (if (>= (key x) (key y)) x y))]
    (letfn [(meld [l r rnd]
              (if (nil? l)
                r (if (nil? r)
                    l (let [nl (min l r)
                            nr (max l r)
                            lnl (left-child nl)
                            rnl (right-child nl)
                            next-rng (step-rnd rnd)
                            b (== 0 (bit-and rnd 1))]
                        (if b
                          [(key nl) (meld lnl nr next-rng) rnl (data nl)]
                          [(key nl) lnl (meld rnl nr next-rng) (data nl)])))))]
      (meld l r rnd))))

(defn insert
  [k v rnd h]
  (meld-heap h [k nil nil v] rnd))

(defn remove
  [rnd h]
  (meld-heap (left-child h) (right-child h) rnd))

(defn top
  [h]
  (first h))

(defn get-nil-to-inf
  [map x]
  (let [r (get map x)]
    (if (nil? r) 0xfffffff r)))

(defn dijkstra
  [vertex-to-adj-list-map start]
  (letfn [(loop
            [marked-set from-map dist-map heap rnd]
            (if (nil? heap)
              [dist-map from-map]
              (let [from (data heap)
                    nheap (remove rnd heap)]
                (println from)
                (if (contains? marked-set from)
                  (recur marked-set from-map dist-map nheap (step-rnd2 rnd))
                  (letfn [(update-distances
                            [edges dist-map from-map heap rnd]
                            (let [e (first edges)
                                  w (first e)
                                  to (second e)]
                              (if (nil? e)
                                [dist-map from-map heap rnd]
                                (let [new-dist (+ (get-nil-to-inf dist-map from) w)]
                                  (if (<= (get-nil-to-inf dist-map to) new-dist)
                                    [(rest edges) dist-map from-map heap rnd]
                                    (recur (rest edges)
                                           (assoc dist-map to new-dist)
                                           (assoc from-map to from)
                                           (insert new-dist to (step-rnd2 rnd) heap)
                                           (step-rnd2 rnd)))))))]
                    (let [[ndist nfrom nheap2 nrnd]
                          (update-distances
                            (get vertex-to-adj-list-map from)
                            dist-map from-map nheap rnd)]
                      (recur (conj marked-set from) nfrom ndist nheap2 (step-rnd2 nrnd))))))))]
    (loop
      (hash-set) (hash-map) (assoc (hash-map) start 0) [0 nil nil start] 777)))

(defn put
  [k v m]
  (assoc m k v))

(def graph
  (->> (hash-map)
       (put 0 [[1 1] [3 2] [100 4]])
       (put 1 [[1 2]])
       (put 2 [[2 3]])
       (put 3 [[95 4]])))

(println (dijkstra graph 0))

(defn book-tickets
  "Tries to book tickets and decrement appropriate references in route-map atomically
   returns map with either :price (for the whole route) and :path (a list of destination names) keys 
          or with :error key that indicates that booking is impossible due to lack of tickets"
  [route-map from to]
  (if (= from to)
    {:path '(), :price 0}
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;TODO implement me using Dijkstra algorithm
    ;;implementation must be pure functional besides the transaction itself, tickets reference modification and 
    ;;restarts monitoring (atom could be used for this)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    nil))
  
;;;cities
(def spec1 (-> empty-map
             (route "City1" "Capital"    200 5)
             (route "Capital" "City1"    250 5)
             (route "City2" "Capital"    200 5)
             (route "Capital" "City2"    250 5)
             (route "City3" "Capital"    300 3)
             (route "Capital" "City3"    400 3)
             (route "City1" "Town1_X"    50 2)
             (route "Town1_X" "City1"    150 2)
             (route "Town1_X" "TownX_2"  50 2)
             (route "TownX_2" "Town1_X"  150 2)
             (route "Town1_X" "TownX_2"  50 2)
             (route "TownX_2" "City2"    50 3)
             (route "City2" "TownX_2"    150 3)
             (route "City2" "Town2_3"    50 2)
             (route "Town2_3" "City2"    150 2)
             (route "Town2_3" "City3"    50 3)
             (route "City3" "Town2_3"    150 2)))

(defn booking-future [route-map from to init-delay loop-delay]
  (future 
    (Thread/sleep init-delay) 
    (loop [bookings []]
      (Thread/sleep loop-delay)
      (let [booking (book-tickets route-map from to)]
        (if (booking :error)
          bookings
          (recur (conj bookings booking)))))))

(defn print-bookings [name ft]
  (println (str name ":") (count ft) "bookings")
  (doseq [booking ft]
    (println "price:" (booking :price) "path:" (booking :path) )))

(defn run []
  ;;try to tune timeouts in order to all the customers gain at least one booking 
  (let [f1 (booking-future spec1 "City1" "City3" 0 1),
        f2 (booking-future spec1 "City1" "City2" 100 1),
        f3 (booking-future spec1 "City2" "City3" 100 1)]
    (print-bookings "City1->City3:" @f1)
    (print-bookings "City1->City2:" @f2)
    (print-bookings "City2->City3:" @f3)
    ;;replace with you mechanism to monitor a number of transaction restarts
    ;;(println "Total (re-)starts:" @booking.impl/transact-cnt)
    ))
