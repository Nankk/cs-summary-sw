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
   [cs-summary.db-remote :as remote]
   ))

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
          (. (. res status 400) end))
        )
      (catch js/Object e
        (. (. res status 500) end)))))

;; CoC character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-coc [req res]
  (println "handle-sw")
  (go (try
        (let [query      (js->clj (. req -query) :keywordize-keys true)
              console-id (:console_id query)
              type       (:type query)]
          (if console-id
            (do
              (case type
                "next" (<? (remote/change-binded-char-id console-id 1 :coc))
                "prev" (<? (remote/change-binded-char-id console-id -1 :coc))
                (. js/console log "No type specified."))
              (return-cs-summary query res :coc))
            (. (. res status 400) end))
          )
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

