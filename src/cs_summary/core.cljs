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
   [cs-summary.db-remote :as remote]
   [cljs.pprint :refer [pprint]]
   [cs-summary.util :as util]))

;; Private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pretty-string [data]
  (with-out-str (pprint data)))

(defn- return-success [res msg]
  (. res send (str "<pre>" msg "</pre>")))

(defn- return-png [res url]
  (println "return-png")
  (go (try
        (let [png (. fs createReadStream url)
              ps  (. stream PassThrough)]
          (. stream pipeline png ps
             (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
          (. ps pipe res))
        (catch js/Object e
          (do (. js/console log e)
              (. res sendStatus 500))))))

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

(defn- create-cs-png [char-id game]
  (println "create-cs-png")
  (let [ch (chan)]
    (go (let [data        ((@local/db :cs-data-list) char-id)
              png-creator (if (= game :sw) sw-png/create-cs-png coc-png/create-cs-png)
              png-name    (<? (png-creator char-id))]
          (>! ch png-name)))
    ch))

;; Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-init [req res]
  (println "handle-init")
  (go (try
        (do
          (local/set-default-db)
          (return-success res (str "db updated.\n" (pretty-string @local/db))))
        (catch js/Object e
          (. res sendStatus 500)))))

(defn- handle-activate [req res]
  (println "handle-activate")
  (go (try (do (local/activate)
               (. res sendStatus 200))
           (catch js/Object e
             (. res sendStatus 500)))))

(defn- handle-deactivate [req res]
  (println "handle-deactivate")
  (go (try (do (local/deactivate)
               (. res sendStatus 200))
           (catch js/Object e
             (. res sendStatus 500)))))

;; Character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-get-cs [req res]
  (println "handle-get-cs")
  (go (try
        (let [data-list (get @local/db :cs-data-list)
              query     (js->clj (. req -query) :keywordize-keys true)
              char-id   (query :char_id)
              game      (keyword (query :game))]
          (if (and char-id game)
            (if (not-empty data-list)
              (if (<= 0 char-id (dec (count data-list)))
                (return-png res (str "./public/img/" (<? (create-cs-png char-id game))))
                (return-png res "./public/img/common/vacant.png"))
              ;; 412 Precondition Required
              (. res sendStatus 412))
            (. res sendStatus 400))) ; 400 Bad Request
        (catch js/Object e
          (. res sendStatus 500)))))

(defn- handle-load-cs [req res]
  (println "handle-load-cs")
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
                (. res sendStatus 200)))
            (. res sendStatus 400)))
        (catch js/Object e
          (println e)
          (. res sendStatus 500)))))

;; Operation variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- too-quick? [char-id]
  (println "(too-quick? [" char-id "])")
  (let [last (get-in @local/db [:last-access-ms char-id])
        thld (@local/db :interval-thld-ms)
        now (util/current-time-ms)
        diff (- now last)]
    (local/set-access-ms char-id now)
    (<= diff thld)))

(defn- handle-change-op-var [req res]
  (println "handle-change-op-var")
  (go (try (let [query   (js->clj (. req -query) :keywordize-keys true)
                 _       (println "Received queries are " query)
                 char-id (js/parseInt (query :char_id))
                 type    (keyword (query :type))
                 var     (keyword (insanitize (query :var)))
                 game    (keyword (query :game))
                 cur-v   (get-in @local/db [:op-vars char-id var])
                 changed (if (and (@local/db :ready?) (not (too-quick? char-id)))
                           (case type
                             :inc (local/changed-op-var var cur-v game 1)
                             :dec (local/changed-op-var var cur-v game -1)
                             (do (. res sendStatus 412)
                                 (throw (js/Error. "Unknown change type"))))
                           cur-v)
                 _       (println "to " changed)
                 _       (local/set-op-var char-id var changed)
                 url     (png-url var changed game)
                 png     (. fs createReadStream url)
                 ps      (. stream PassThrough)]
             (. stream pipeline png ps
                (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
             (. ps pipe res))
           (catch js/Object e
             (. res sendStatus 500)))))

(defn- handle-reflect-op-var [req res]
  (println "handle-reflect-op-var")
  (go (try (let [query         (js->clj (. req -query) :keywordize-keys true)
                 char-id       (js/parseInt (query :char_id))
                 game          (keyword (query :game))
                 reflect-only? (query :reflect_only)
                 data-list     (get @local/db :cs-data-list)]
             (if (not-empty data-list)
               (if char-id
                 (if (<= 0 char-id (dec (count data-list)))
                   (let [op-vars (@local/db :op-vars)]
                     (local/reflect-op-var char-id)
                     (if reflect-only?
                       (. res sendStatus 200)
                       (return-png res (str "./public/img/" (<? (create-cs-png char-id game))))))
                   (return-png res "./public/img/common/vacant.png"))
                 (. res sendStatus 400))
               (. res sendStatus 412)))
           (catch js/Object e
             (. res sendStatus 500)))))

(defn- handle-reset-op-vars [req res]
  (println "handle-reset-op-vars")
  (go (try (do
             (local/set-op-vars (local/default-db :op-vars))
             (return-success res (str "op-vars reset.\n" (pretty-string (@local/db :op-vars)))))
           (catch js/Object e
             (. res sendStatus 500)))))

(defn- handle-change-threshold [req res]
  (println "handle-change-threshold")
  (go (try (let [query (js->clj (. req -query) :keywordize-keys true)
                 time  (js/parseInt (query :time))]
             (local/set-interval-threshold time)
             (return-success res (str "Interval threshold set.\n" (pretty-string (@local/db :interval-thld-ms)))))
           (catch js/Object e
             (. res sendStatus 500)))))

;; Character variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-reset-char-vars [req res]
  (println "handle-reset-char-vars")
  (go (try (do
             (local/set-char-vars (local/default-db :char-vars))
             (return-success res (str "char-vars reset.\n" (pretty-string (@local/db :char-vars)))))
           (catch js/Object e
             (. res sendStatus 500)))))

(defn- handle-set-char-var [req res]
  (println "handle-set-char-var")
  (go (try (let [query   (js->clj (. req -query) :keywordize-keys true)
                 _       (println "Received queries are " query)
                 char-id (js/parseInt (query :char_id))
                 var     (keyword (insanitize (query :var)))
                 v       (js/parseInt (query :value))]
             (local/set-char-var char-id var v)
             (. res sendStatus 200))
           (catch js/Object e
             (. res sendStatus 500)))))

;; Dice roll ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-roll [req res]
  (println "handle-roll")
  (go (try (let [query   (js->clj (. req -query) :keywordize-keys true)
                 dice    (query :dice)
                 _       (println (str "queries {:dice " dice "}"))
                 pip     (inc (rand-int (js/parseInt (str/join (rest dice)))))
                 png-url (str "./public/img/dice/" dice "/" pip ".png")]
             (return-png res png-url))
           (catch js/Object e
             (. res sendStatus 500)))))

;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce server (atom nil))

(defonce reload-counter
  (let [counter (atom 0)]
    (fn []
      (swap! counter inc)
      @counter)))

(defn ^:dev/after-load main []
  (let [app (express.)]

    ;; Global
    (. app get "/init" handle-init)
    (. app get "/activate" handle-activate)
    (. app get "/deactivate" handle-deactivate)

    ;; Character sheet
    (. app get "/load-cs" handle-load-cs)
    (. app get "/get-cs" handle-get-cs)

    ;; Operation variables
    (. app get "/change-op-var" handle-change-op-var)
    (. app get "/reflect-op-var" handle-reflect-op-var)
    (. app get "/reset-op-vars" handle-reset-op-vars)
    (. app get "/change-threshold" handle-change-threshold)

    ;; Character variables
    (. app get "/reset-char-vars" handle-reset-char-vars)
    (. app get "/set-char-var" handle-set-char-var)

    ;; Dice roll
    (. app get "/roll" handle-roll)

    (when (some? @server)
      (. @server close)
      (println "Server reloaded"))
    (println (str "### You have re-loaded the server " (reload-counter) " times. ###"))

    ;; Listen process.env.PORT or fixed port 55555
    (let [env-port (.. js/process -env -PORT)
          port (if env-port env-port 55555)]
      (reset! server (. app listen port,
                        #(. js/console log
                            (str "Listening on port " port)))))))
