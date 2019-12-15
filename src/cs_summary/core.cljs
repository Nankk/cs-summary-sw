(ns cs-summary.core
  (:require
   ["express" :as express]
   ["stream" :as stream]
   ["fs" :as fs]
   ["puppeteer" :as ppt]
   [clojure.string :as str]
   [cljs.core.async :as async :refer [>! <! go chan timeout go-loop]]
   [async-interop.interop :refer-macros [<p!]]
   [reagent.core :as reagent]
   [reagent.dom.server :as rdom]
   [cs-summary.gapis :as gapis]
   [cs-summary.const :as const]
   [cs-summary.macros :refer [throw-err] :refer-macros [<?]]
   [cs-summary.sw.png-maker :as sw-png]
   [cs-summary.coc.png-maker :as coc-png]
   [cs-summary.db-remote :as remote]))

;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- png-url [target num game]
  (let [img-root "./public/img"]
    (case target
      :sign     (let [plus-or-minus [(str img-root "/common/plus.png")
                                     (str img-root "/common/minus.png")]]
                  (nth plus-or-minus num))
      :op_param (let [params (if (= game :sw) const/sw-params const/coc-params)]
                  (str img-root "/" (name game) "/" (name (nth params num)) ".png"))
      (str img-root "/common/" num ".png"))))

(defn change-op-value [query v res game]
  (println "change-op-value")
  (go (let [console-id        (query :console_id)
            sheet-id          (if (= game :sw) const/sw-sheet-id const/coc-sheet-id)
            target            (keyword (query :target))
            all-op-values     (<? (remote/get-all-op-values game))
            new-vs            (update-in all-op-values
                                         [(dec console-id) target]
                                         #(remote/inc-op-value console-id target % game))
            new-vs-vectorized (vec (for [v new-vs] (vec (vals v))))
            _                 (<? (remote/set-all-op-values (clj->js new-vs-vectorized) game))
            new-v             (get-in new-vs [(dec console-id) target])
            png               (. fs createReadStream (png-url target new-v game))
            ps                (. stream PassThrough)]
        (. stream pipeline png ps
           (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
        (. ps pipe res))))

(defn- return-cs-summary [query res game]
  (println "return-cs-summary")
  (try (go (let [console-id (:console_id query)
                 png-fn     (if (= game :sw) sw-png/create-cs-png coc-png/create-cs-png)
                 png-name   (<? (png-fn console-id))
                 png        (. fs createReadStream (str "./public/img/" png-name))
                 ps         (. stream PassThrough)]
             (. stream pipeline png ps
                (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
             (. ps pipe res)))
       (catch js/Object e
         (. (. res status 500) end))))

(defn reflect-op [query res game]
  (println "reflect-op")
  (go (let [console-id           (query :console_id)
            op-values            (vec (vals (<? (remote/get-op-values console-id game))))
            params               (if (= game :sw) const/sw-params const/coc-params)
            target               (params (js/parseInt (op-values 0)))
            sign                 (nth ["+" "-"] (op-values 1))
            diff                 (js/parseInt (str sign (str/join "" (subvec op-values 2))))
            _                    (println (str "diff: " diff))
            all-var-diffs        (<? (remote/get-all-var-diffs game))
            _                    (println (str "all-var-diffs: " all-var-diffs))
            new-diffs            (update-in all-var-diffs
                                            [(dec console-id) target]
                                            #(+ % diff))
            new-diffs-vectorized (vec (for [v new-diffs] (vec (vals v))))
            _                    (println (<? (remote/set-all-var-diffs new-diffs-vectorized game)))]
        (return-cs-summary query res game))))

;; SW character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-sw [req res]
  (println "handle-sw")
  (go (try
        (let [query      (js->clj (. req -query) :keywordize-keys true)
              console-id (:console_id query)
              type       (:type query)]
          (if console-id
            (do
              (case type
                "next" (<? (remote/change-binded-char-id console-id 1 :sw))
                "prev" (<? (remote/change-binded-char-id console-id -1 :sw))
                (. js/console log "No type specified."))
              (return-cs-summary query res :sw))
            (. (. res status 400) end)))
        (catch js/Object e
          (. (. res status 500) end)))))

;; CoC character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ex) console_id=1&type=inc&target=op_param
(defn- handle-coc [req res]
  (println "handle-sw")
  (go (try
        (let [query      (js->clj (. req -query) :keywordize-keys true)
              console-id (query :console_id)
              type       (query :type)]
          (if console-id
            (case type
              "inc"     (change-op-value query 1 res :coc)
              "dec"     (change-op-value query -1 res :coc)
              "reflect" (reflect-op query res :coc)
              (return-cs-summary query res :coc))
            (. (. res status 400) end)))
        (catch js/Object e
          (. (. res status 500) end)))))

;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce server (atom nil))
(defonce reload-counter
  (let [counter (atom 0)]
    (fn []
      (swap! counter inc)
      @counter)))

(defn- return-200 [req res]
  (. res writeHead 200 (clj->js {:Content-Type "application/json; charset=utf-8"}))
  (. res end (. js/JSON stringify (clj->js {:status "ok"}))))

(defn ^:dev/after-load main []
  (let [app (express.)]

    (. app get "/status" return-200) ; In order to keep the app active by robot access

    (. app get "/character-sheet-sw" handle-sw)

    (. app get "/character-sheet-coc" handle-coc)

    (when (some? @server)
      (. @server close)
      (println "Why the fuck did you kill the server!!?"))

    (println (str "### You have re-loaded " (reload-counter) " times. ###"))

    ;; Listen process.env.PORT or fixed port 3000
    (let [env-port (.. js/process -env -PORT)
          port (if env-port env-port 3000)]
      (reset! server (. app listen port,
                        #(. js/console log
                            (str "Listening on port " port)))))))

