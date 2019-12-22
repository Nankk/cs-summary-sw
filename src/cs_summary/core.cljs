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
   [cs-summary.db-local :as local]
   [cs-summary.db-remote :as remote]))

;; Detail ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- scrape-cs-text [url]
  (println "scrape-cs-text")
  (let [ch (chan)]
    (go (let [selector   "#MAKING > aside.leftsidebar.fixed > section:nth-child(3) > div > button:nth-child(1)"
              browser    (<p! (. ppt launch (clj->js {;; :headless false
                                                      :args     ["--no-sandbox"
                                                                 "--disable-setuid-sandbox"]})))
              page       (<p! (. browser newPage))
              _          (<p! (. page goto url))
              button     (<p! (. page $ selector))
              _          (<p! (. button evaluate (fn [b] (. b click))))
              p-content  (<p! (. page evaluate (fn [] (.. js/document -body -innerHTML))))
              _          (<? (timeout 3000))
              cs-page    (clj->js ((js->clj (<p! (. browser pages))) 2))
              cs-content (<p! (. cs-page evaluate (fn [] (. (. js/document querySelector "pre") -innerHTML))))
              _          (println "cs scraping completed.")]
          (. browser close)
          (println (str "Content is:\n" (take 5 (str/split-lines cs-content))) "...")
          (>! ch cs-content)))
    ch))

(defn- png-url [var v game]
  (let [img-root "./public/img"]
    (case var
      :sign     (let [plus-or-minus [(str img-root "/common/plus.png")
                                     (str img-root "/common/minus.png")]
                      idx           (local/<-sign v)]
                  (nth plus-or-minus idx))
      :param (let [params (const/params game)
                      idx    (local/<-param v game)]
                  (str img-root "/" (name game) "/" (name (nth params idx)) ".png"))
      (str img-root "/common/" v ".png"))))

(defn insanitize
  "Replaces '_' with '-'."
  [s]
  (str/replace s #"_" "-"))

;; Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- return-cs-png [res char-id game]
  (println "return-cs-png")
  (go (try
        (let [data        ((@local/db :cs-data-list) char-id)
              png-creator (if (= game :sw) sw-png/create-cs-png coc-png/create-cs-png)
              png-name    (<? (png-creator char-id))
              png         (. fs createReadStream (str "./public/img/" png-name))
              ps          (. stream PassThrough)]
          (. stream pipeline png ps
             (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
          (. ps pipe res))
        (catch js/Object e
          (. (. res status 500) end)))))

(defn- handle-reflect-op-var [req res]
  (println "handle-reflect-op-var")
  (go (try (let [query         (js->clj (. req -query) :keywordize-keys true)
                 char-id       (js/parseInt (query :char_id))
                 game          (keyword (query :game))
                 reflect-only? (query :reflect_only)]
             (if char-id
               (let [op-vars (@local/db :op-vars)]
                 (local/reflect-op-var char-id)
                 (if reflect-only?
                   (. (. res status 200) end)
                   (return-cs-png res char-id game)))
               (. (. res status 400) end)))
           (catch js/Object e
             (. (. res status 500) end)))))

(defn- handle-change-op-var [req res]
  (println "handle-change-op-var")
  (go (try (let [query   (js->clj (. req -query) :keywordize-keys true)
                 char-id (js/parseInt (query :char_id))
                 type    (keyword (query :type))
                 var     (keyword (insanitize (query :var)))
                 game    (keyword (query :game))
                 _       (println (str "queries {:char_id " char-id " :type " type " :var " var " :game " game "}"))
                 cur-v   (get-in @local/db [:op-vars char-id var])
                 changed (case type
                           :inc (local/changed-op-var var cur-v game 1)
                           :dec (local/changed-op-var var cur-v game -1)
                           (do (. (. res status 412) end)
                               (throw (js/Error. "Unknown change type"))))
                 _       (local/set-op-var char-id var changed)
                 url     (png-url var changed game)
                 png     (. fs createReadStream url)
                 ps      (. stream PassThrough)]
             (. stream pipeline png ps
                (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
             (. ps pipe res))
           (catch js/Object e
             (. (. res status 500) end)))))

(defn- handle-charcter-sheet [req res]
  (println "handle-charcter-sheet")
  (go (try
        (let [data-list (get @local/db :cs-data-list)
              query     (js->clj (. req -query) :keywordize-keys true)
              char-id   (query :char_id)
              game      (keyword (query :game))]
          (if (and char-id game)
            (if (not-empty data-list)
              (return-cs-png res char-id game)
              ;; 412 Precondition Required
              (. (. res status 412) end))
            (. (. res status 400) end))) ; 400 Bad Request
        (catch js/Object e
          (. (. res status 500) end)))))

(defn- handle-init [req res]
  (println "handle-init")
  (go (try
        (let [query (js->clj (. req -query) :keywordize-keys true)
              game  (keyword (query :game))]
          (if game
            (let [urls         (<? (remote/get-all-cs-urls game))
                  _            (println urls)
                  data-creator (if (= game :sw) sw-png/create-cs-data coc-png/create-cs-data)
                  cs-data-list (<? (async/map vector (for [i (range (count urls))] (data-creator (urls i) (* i 15000))) (count urls)))]
              (local/set-cs-data-list cs-data-list)
              (println "cs-data-list set")
              (let [data-list (@local/db :cs-data-list)]
                (println data-list)
                (println (str "Got " (count data-list) " data from the server"))
                (. (. res status 200) end)))
            (. (. res status 400) end)))
        (catch js/Object e
          (println e)
          (. (. res status 500) end)))))

(defn- handle-reset [req res]
  (println "handle-reset")
  (go (try
        (do
          (local/set-default-db)
          (. (. res status 200) end))
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

    (. app get "/status" return-200) ; for activator robot

    (. app get "/reset" handle-reset)

    (. app get "/init" handle-init)

    (. app get "/character-sheet" handle-charcter-sheet)

    (. app get "/change-op-var" handle-change-op-var)

    (. app get "/reflect-op-var" handle-reflect-op-var)

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
