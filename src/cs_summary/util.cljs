(ns cs-summary.util)

(def next-key
  (let [counter (atom 0)]
    (fn []
      (swap! counter inc)
      @counter)))
