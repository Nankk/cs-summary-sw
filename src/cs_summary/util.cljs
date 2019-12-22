(ns cs-summary.util)

(def next-key
  (let [counter (atom 0)]
    (fn []
      (swap! counter inc)
      @counter)))

(defn index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll)) 
              (= v (last coll)))
      i)))
