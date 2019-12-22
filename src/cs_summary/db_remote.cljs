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
    (go (let [sheet-id   (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
              data-table (if (= game :sw) const/sw-data-table const/coc-data-table)
              res        (<? (gapis/sheets-get sheet-id (str data-table "B:B")))
              vs         (vec (map #(% 0) (rest (js->clj (.. res -data -values)))))]
          (>! ch vs)))
    ch))
