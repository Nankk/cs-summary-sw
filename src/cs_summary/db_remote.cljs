(ns cs-summary.db-remote
  (:require
   [clojure.string :as str]
   [cljs.core.async :as async :refer [>! <! go chan timeout go-loop]]
   [async-interop.interop :refer-macros [<p!]]
   [cs-summary.macros :refer [throw-err] :refer-macros [<?]]
   [cs-summary.gapis :as gapis]
   [cs-summary.const :as const]))

(defn get-all-cs-urls [game]
  (println "get-all-cs-urls")
  (let [ch (chan)]
    (go (let [
              sheet-id      (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              data-table (if (= game :sw) const/sw-data-table const/coc-data-table)
              res        (<? (gapis/sheets-get sheet-id (str data-table "B:B")))
              vs         (into [] (rest (js->clj (.. res -data -values))))]
          (>! ch vs)))
    ch))

(defn get-cs-url [char-id game]
  (println "get-cs-url")
  (let [ch (chan)]
    (go (let [urls (<? (get-all-cs-urls game))]
          (>! ch ((urls (dec char-id)) 0))))
    ch))

(defn get-all-char-ids [game]
  (println "get-all-char-ids")
  (let [ch (chan)]
    (go (let [sheet-id   (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              bind-table (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              res        (<? (gapis/sheets-get sheet-id (str bind-table "B:B")))
              _          (println "aaa")
              vs         (into [] (rest (js->clj (.. res -data -values))))
              _          (println vs)]
          (>! ch vs)))
    ch))

(defn set-binded-char-id [console-id new-id game]
  (println "set-binded-char-id")
  (let [ch (chan)]
    (go (let [sheet-id   (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              bind-table (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              res        (<? (gapis/sheets-update
                              sheet-id
                              (str bind-table "B" (inc console-id) ":B" (inc console-id))
                              (clj->js [new-id])))]
          (>! ch res)))
    ch))

(defn get-binded-char-id [console-id game]
  (println "get-binded-char-id")
  (let [ch (chan)]
    (go (let [all (<? (get-all-char-ids game))
              char-id (get-in all [(dec console-id) 0])]
          (println char-id)
          (>! ch char-id)))
    ch))

(defn get-all-var-diffs [game]
  (println "get-all-var-diffs")
  (let [ch (chan)]
    (go (let [sheet-id  (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              var-table (if (= game :sw) const/sw-var-table const/coc-var-table)
              res       (<? (gapis/sheets-get sheet-id (str var-table "B1:D10")))
              _         (println (str "Got var-diffs " (name game)))
              nums      (js->clj (.. res -data -values))
              _         (println nums)
              mapped    (vec (for [items (rest nums)]
                               (into {} (for [i (range (count items))]
                                          {(keyword ((first nums) i)) (js/parseInt (items i))}))))
              _         (println mapped)
              ]
          (>! ch mapped)))
    ch))

(defn get-var-diffs [char-id game]
  (println "get-var-diffs")
  (let [ch (chan)]
    (go (let [all (<? (get-all-var-diffs game))]
          (>! ch (nth all (dec char-id)))))
    ch))

(defn get-all-op-values [game]
  (println "get-all-op-values")
  (let [ch (chan)]
    (go (let [sheet-id    (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              bind-table  (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              sheet-range (if (= game :sw) "" "D1:H10")
              res         (<? (gapis/sheets-get sheet-id (str bind-table sheet-range)))
              _           (println (str "Got op-values " (name game)))
              vs          (js->clj (.. res -data -values))
              _           (println vs)
              mapped      (vec (for [items (rest vs)]
                                 (into {} (for [i (range (count items))]
                                            {(keyword ((first vs) i)) (js/parseInt (items i))}))))
              _           (println mapped)
              ]
          (>! ch mapped)))
    ch))

(defn get-op-values [char-id game]
  (println "get-op-values")
  (let [ch (chan)]
    (go (let [all (<? (get-all-op-values game))]
          (println "aaa")
          (>! ch (nth all (dec char-id)))))
    ch))

(defn change-binded-char-id [console-id diff game]
  (println "change-binded-char-id")
  (let [ch (chan)]
    (go (let [char-id (<? (get-binded-char-id (dec console-id) game))]
          (<? (set-binded-char-id console-id (+ char-id diff) game))))
    ch))

(defn- ->coc-param [num]
  (nth const/coc-params (mod (count const/coc-params) num)))

(defn set-all-op-values [new-values game]
  (println "set-all-op-values")
  (println "Debug-man!! (^v^)")
  (let [ch (chan)]
    (go (let [sheet-id    (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              bind-table  (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              sheet-range (if (= game :sw) "" "D2:H10")
              res         (<? (gapis/sheets-update sheet-id (str bind-table sheet-range) new-values))
              _ (println res)
              ]
          (>! ch res)))
    ch))

(defn- inc-op-value [console-id target cur-val game]
  (let [new-val (inc cur-val)
        params-count (if (= game :sw) (count const/sw-params) (count const/coc-params))]
    (case target
      :op_param (mod new-val params-count)
      :sign (mod new-val 2)
      (mod new-val 10))))

(defn reflect-op [console-id res game]
  )
