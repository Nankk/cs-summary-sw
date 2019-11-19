(ns cs-summary-sw.core
  (:require
   ["express" :as express]
   [clojure.string :as str]
   ["fs" :as fs]
   [reagent.core :as reagent]
   [reagent.dom.server :as rdom]
   ["puppeteer" :as ppt]
   [cljs.core.async :as async :refer [>! <! go chan timeout go-loop]]
   [async-interop.interop :refer-macros [<p!]]
   ["stream" :as stream]
   [cs-summary-sw.gapis :as gapis]
   [cs-summary-sw.cs-parser :as parser]
   [cs-summary-sw.const :as const]))

(def root js/__dirname)

;; Character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bind-table "console-char-table!")
(def data-table "char-data-table!")
(def db-id "1oMDM0k9TfPREXhXHIrTUPCpxmZpwmhq79yiCkt1JdMM")
(def next-key
  (let [counter (atom 0)]
    (fn []
      (swap! counter inc)
      @counter)))

(defn- table-template [fixed? rows]
  (println "table-template")
  [:table.table (when fixed? {:class "fixed"})
   (let [header (first rows)]
     [:thead
      [:tr
       (for [v header]
         ^{:key (next-key)} [:th v])]])
   (let [data-rows (rest rows)]
     [:tbody
      (for [row data-rows]
        ^{:key (next-key)} [:tr
                            (for [v row]
                              ^{:key (next-key)} [:td v])])])])

(defn- layouted-cs-hiccup [data]
  (println "layouted-cs-hiccup")
  [:div {:style {:width 1700 :height 1000 :margin "20px"}}
   [:div.card {:class "inline-block" :style {:width 800 :height 1000}}
    [:div.card-body {:style {:width 800}}
     [:div.inline-block
      [:div
       [table-template false
        [["名前" "冒険者レベル" "最大HP" "最大MP"]
         [(data :name) (data :level) (data :hp) (data :mp)]]]]
      [:div
       [:div.inline-block {:style {:width 400}}
        [table-template true
         [["能力" "値　　" "ボーナス"]
          ["器用度" (get-in data [:params :raw :dex]) (get-in data [:params :bonus :dex])]
          ["敏捷度" (get-in data [:params :raw :agi]) (get-in data [:params :bonus :agi])]
          ["筋力" (get-in data [:params :raw :str]) (get-in data [:params :bonus :str])]
          ["生命力" (get-in data [:params :raw :con]) (get-in data [:params :bonus :con])]
          ["知力" (get-in data [:params :raw :int]) (get-in data [:params :bonus :int])]
          ["精神力" (get-in data [:params :raw :pow]) (get-in data [:params :bonus :pow])]]]]
       [:div.inline-block {:style {:width 300}} [table-template true (vec (cons ["技能" "レベル"] (vec (for [ability (data :abilities)] ^{:key (next-key)} [(ability :name) (ability :level)]))))]]]
      [:div
       [table-template true
        [["魔物知識" "先制力" "生命抵抗" "精神抵抗" "命中" "回避" "防護点"]
         [[:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :knowledge)]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :initiative)]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :reg-bio)]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :reg-spirit)]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :accuracy)]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "2D6+ "] (data :flee)]
          (str "　" (data :def))]]]]
      [:div
       [:div.inline-block {:style {:width 450}}
        [table-template false
         (vec (cons ["特技" "内容"]
                    (vec (for [skill (data :skills)] ^{:key (next-key)}
                           [(skill :name) (skill :description)]))))]]
       [:div.inline-block {:style {:width 250}}
        [table-template false
         (vec (cons ["魔法技能" "レベル" "魔力"]
                    (vec (for [magic (data :magics)]
                           ^{:key (next-key)} [(magic :name) (magic :level) (magic :power)]))))]]]]]]
   [:div.card {:class "inline-block" :style {:width 800 :height 400}}
    [:div.card-body {:style {:width 800}}
     [:div
      [:div.inline-block
       [table-template false
        (vec (cons (vec (concat ["【威力】" "C値"] (vec (range 3 13)) ["追加"]))
                   (conj [] (vec (concat [(data :weapon-power) (data :critical)]
                                         (const/weapon-rating-table (data :weapon-power))
                                         [(str "+" (data :bonus))])))))]]]
     [:div
      [:div.inline-block
       [table-template false
        [["【探索判定パッケージ】" "技巧" "運動" "観察" "知識"]
         [[:span [:span {:style {:color "#999999" :font-size "65%"}} "スカウト　　"] (get-in data [:package-values :scout])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "A　"] (get-in data [:package-values :a])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "C　"] (get-in data [:package-values :c])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "E　"] (get-in data [:package-values :e])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} " 　"] "-"]]
         [[:span [:span {:style {:color "#999999" :font-size "65%"}} "レンジャー　"] (get-in data [:package-values :ranger])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "B　"] (get-in data [:package-values :b])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "D　"] (get-in data [:package-values :d])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "F　"] (get-in data [:package-values :f])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} " 　"] "-"]]
         [[:span [:span {:style {:color "#999999" :font-size "65%"}} "セージ　　　"] (get-in data [:package-values :sage])]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} " 　"] "-"]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} " 　"] "-"]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} " 　"] "-"]
          [:span [:span {:style {:color "#999999" :font-size "65%"}} "G　"] (get-in data [:package-values :g])]]]]]]]]])

(defn- get-all-cs-urls []
  (println "get-all-cs-urls")
  (let [ch (chan)]
    (go (let [res (<! (gapis/sheets-get db-id (str data-table "B:B")))
              vs  (into [] (rest (js->clj (.. res -data -values))))]
          (>! ch vs)))
    ch))

(defn- get-cs-url [char-id]
  (println "get-cs-url")
  (let [ch (chan)]
    (go (let [urls (<! (get-all-cs-urls))]
          (>! ch ((urls (dec char-id)) 0))))
    ch))

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
              _          (<! (timeout 1200)) ; Ugly. Quite ugly.
              cs-page    (clj->js ((js->clj (<p! (. browser pages))) 2))
              cs-content (<p! (. cs-page evaluate (fn [] (. (. js/document querySelector "pre") -innerHTML))))
              _          (println "Yomikomi owata ＼(^o^)／")]
          (. browser close)
          (println (str "Content is:\n" cs-content))
          (>! ch cs-content)))
    ch))

(defn- set-binded-char-id [console-id new-id]
  (println "set-binded-char-id")
  (let [ch (chan)]
    (go (let [res (<! (gapis/sheets-update
                       db-id
                       (str bind-table "B" (inc console-id) ":B" (inc console-id))
                       (clj->js [new-id])))]
          (>! ch res)))
    ch))

(defn- get-all-char-ids []
  (println "get-all-char-ids")
  (let [ch (chan)]
    (go (let [res (<! (gapis/sheets-get db-id (str bind-table "B:B")))
              _ (println "aaa")
              vs  (into [] (rest (js->clj (.. res -data -values))))
              _ (println vs)]
          (>! ch vs)))
    ch))

(defn- get-binded-char-id [console-id]
  (println "get-binded-char-id")
  (let [ch (chan)]
    (go (let [all (<! (get-all-char-ids))
              char-id (get-in all [(dec console-id) 0])]
          (println char-id)
          (>! ch char-id)))
    ch))

(defn- package-values [data]
  (let [dex    (get-in data [:params :bonus :dex])
        agi    (get-in data [:params :bonus :agi])
        int    (get-in data [:params :bonus :int])
        scout  (:level (or (first (filter #(= (% :name) "スカウト") (data :abilities))) {:level 0}))
        ranger (:level (or (first (filter #(= (% :name) "レンジャー") (data :abilities))) {:level 0}))
        sage   (:level (or (first (filter #(= (% :name) "セージ") (data :abilities))) {:level 0}))]
    {:scout  scout
     :ranger ranger
     :sage   sage
     :a      (if (zero? scout) 0 (+ dex scout))
     :b      (if (zero? ranger) 0 (+ dex ranger))
     :c      (if (zero? scout) 0 (+ agi scout))
     :d      (if (zero? ranger) 0 (+ agi ranger))
     :e      (if (zero? scout) 0 (+ int scout))
     :f      (if (zero? ranger) 0 (+ int ranger))
     :g      (if (zero? sage) 0 (+ int sage))}))

(defn- create-cs-png [console-id]
  (println "create-cs-png")
  (let [ch (chan)]
    (go (let [char-id     (<! (get-binded-char-id console-id))
              cs-url      (<! (get-cs-url char-id))
              cs-text     (<! (scrape-cs-text cs-url))
              cs-data     (parser/chara-data cs-text)
              cs-data+    (assoc cs-data :package-values (package-values cs-data)) ; append package values
              out-name    (str "cs-" console-id ".png")
              browser     (<p! (. ppt launch (clj->js {;; :headless false
                                                       :args ["--no-sandbox"
                                                              "--disable-setuid-sandbox"]})))
              page        (<p! (. browser newPage))
              _           (<p! (. page setViewport (clj->js {:width 1800 :height 1100})))
              _           (<p! (. page goto (str "file://" root "/public/index.html")))
              elem-txt    (rdom/render-to-string [layouted-cs-hiccup cs-data+])
              target-elem (<p! (. page $ "#app"))
              _           (. page evaluate (fn [elem]
                                             (let [target (. js/document getElementById "app")]
                                               (set! (. target -innerHTML) elem))) elem-txt)]
          (println "Screehshot...")
          (<! (timeout 1000))
          (<p! (. page screenshot (clj->js {:path (str "./public/img/" out-name)
                                            :clip {:x      0
                                                   :y      0
                                                   :width  1700
                                                   :height 1050}})))
          (println "Screehshot!")
          (. browser close)
          (>! ch out-name)))
    ch))

(defn- change-binded-char-id [console-id diff]
  (println "change-binded-char-id")
  (let [ch (chan)]
    (go (let [char-id (<! (get-binded-char-id (dec console-id)))]
          (<! (set-binded-char-id console-id (+ char-id diff)))))
    ch))

(defn- return-cs-summary [query res]
  (println "return-cs-summary")
  (go (let [console-id (:console_id query)
            png-name   (<! (create-cs-png console-id))
            png        (. fs createReadStream (str "./public/img/" png-name))
            ps         (. stream PassThrough)]
        (. stream pipeline png ps
           (fn [err] (when err (. js/console log err) (. res sendStatus 400))))
        (. ps pipe res))))

(defn- handle-cs-query [req res]
  (println "handle-cs-query")
  (go (let [query      (js->clj (. req -query) :keywordize-keys true)
            console-id (:console_id query)
            type       (:type query)]
        (if console-id
          (do
            (case type
              "next" (<! (change-binded-char-id console-id 1))
              "prev" (<! (change-binded-char-id console-id -1))
              (. js/console log "No type specified."))
            (return-cs-summary query res))
          (. (. res status 400) end)) ; console-idすら無かったら流石におかしいのでエラー
        )))

;; Judgement package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-jp-query [req res])

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

    (. app get "/character-sheet" handle-cs-query)

    (. app get "/judgement-package" handle-jp-query)

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
