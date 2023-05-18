(ns phil)

(defn create-fork
  []
  (ref true))

(defn create-philosopher
  [id forks eating-attempts]
  (ref (vector id forks false eating-attempts)))

(defn forks
  [phil]
  (nth @phil 1))

(defn eating?
  [phil]
  (nth @phil 2))

(defn eating-attempts
  [phil]
  (nth @phil 3))

(def g-eat-cnt
  (atom 0))

(def g-eat-cnt-with-forks
  (atom 0))

(def g-stop-eat-cnt
  (atom 0))

(defn start-eating [phil]
  (dosync
    (swap! g-eat-cnt inc)
    (if (every? true? (map ensure (forks phil)))
      (do (swap! g-eat-cnt-with-forks inc)
          (doseq [f (forks phil)] (alter f not))
          (alter phil
                 (fn [[name forks _ food]]
                   [name forks true (dec food)]))
        true)
      false)))

(defn stop-eating [phil]
  (dosync
    (swap! g-stop-eat-cnt inc)
    (when (eating? phil)
      (alter phil (fn [[name forks _ food]] [name forks false food]))
      (doseq [f (forks phil)] (alter f not)))))

(defn try-eat-with-back-off [phil eat-time thinking-time]
  (while (pos? (eating-attempts phil))
    (if (start-eating phil)
      (do (Thread/sleep eat-time)
          (stop-eating phil)
          (Thread/sleep thinking-time))
      (Thread/yield))))

(def eating-attempts-const 100)
(def eat-time-const 4)
(def thinking-time-const 3)

(defn create-philosophers
  [n]
  (let [f (take n (repeatedly create-fork))
        f2 (concat f f)]
    (doall (map
             (fn [id] (create-philosopher
                        id
                        [(nth f2 id) (nth f2 (inc id))]
                        eating-attempts-const))
             (range n)))))

(defn start []
  (doall (map
           (fn [phil] (future
                        (try-eat-with-back-off
                          phil eat-time-const
                          thinking-time-const)))
           (create-philosophers 5))))

(doall (map (fn [x] @x) (start)))

(println @g-eat-cnt @g-eat-cnt-with-forks @g-stop-eat-cnt)

