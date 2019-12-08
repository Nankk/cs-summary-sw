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
              db-id      (if (= game :sw) const/sw-db-id const/coc-db-id)
              data-table (if (= game :sw) const/sw-data-table const/coc-data-table)
              res        (<? (gapis/sheets-get db-id (str data-table "B:B")))
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
    (go (let [db-id      (if (= game :sw) const/sw-db-id const/coc-db-id)
              bind-table (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              res        (<? (gapis/sheets-get db-id (str bind-table "B:B")))
              _          (println "aaa")
              vs         (into [] (rest (js->clj (.. res -data -values))))
              _          (println vs)]
          (>! ch vs)))
    ch))

(defn set-binded-char-id [console-id new-id game]
  (println "set-binded-char-id")
  (let [ch (chan)]
    (go (let [db-id      (if (= game :sw) const/sw-db-id const/coc-db-id)
              bind-table (if (= game :sw) const/sw-bind-table const/coc-bind-table)
              res        (<? (gapis/sheets-update
                              db-id
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

(defn change-binded-char-id [console-id diff game]
  (println "change-binded-char-id")
  (let [ch (chan)]
    (go (let [char-id (<? (get-binded-char-id (dec console-id) game))]
          (<? (set-binded-char-id console-id (+ char-id diff) game))))
    ch))

