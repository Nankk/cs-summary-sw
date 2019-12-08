(ns cs-summary.coc.png-maker
  (:require
   ["express" :as express]
   ["stream" :as stream]
   ["fs" :as fs]
   ["puppeteer" :as ppt]
   [clojure.string :as str]
   [cljs.core.async :as async :refer [>! <! go chan timeout go-loop]]
   [async-interop.interop :refer-macros [<p!]]
   [cs-summary.coc.parser :as parser]
   [cs-summary.db-remote :as remote]
   [reagent.dom.server :as rdom]
   [cs-summary.util :as util]
   [cs-summary.const :as const]
   [cs-summary.macros :refer [throw-err] :refer-macros [<?]]
   ))

;; Coc character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- table-template [fixed? align rows]
  (println "table-template")
  [:table.table (when fixed? {:class "fixed"})
   [:thead
    [:tr {:style {:text-align (name align)}}
     (for [v (first rows)]
       ^{:key (util/next-key)} [:th v])]]
   (let [data-rows (rest rows)]
     [:tbody
      (for [row data-rows]
        ^{:key (util/next-key)} [:tr {:style {:text-align (name align)}}
                            (for [v row]
                              ^{:key (util/next-key)} [:td (or v "")])])])])

(defn- skills-template [data cols type]
  (let [->name    {:battle      "戦闘"
                   :exploration "探索"
                   :action      "行動"
                   :negotiation "交渉"
                   :knowledge   "知識"}
        type-name (->name type)]
    [:div
     (let [skills  (filter #(= (% :category) type) (data :skills))
           records (map #(vec %) (partition cols cols (vec (take cols (repeat {:name "" :proficiency ""}))) skills))]
       [table-template true :left
        (cons (into [(->name type)] (take (dec (* 2 cols)) (repeat  "")))
              (for [record records]
                (flatten (for [idx (range cols)]
                           [((record idx) :name) ((record idx) :proficiency)]))))])]))

(defn- layouted-cs-hiccup [data]
  (println "layouted-cs-hiccup")
  [:div {:style {:width 1700 :height 1000 :margin "20px"}}
   ;; Left panel
   [:div.card {:class "inline-block" :style {:width 800 :height 1000}}
    [:div.card-body {:style {:width 800}}
     [:div {:style {:text-align "left"}} [:h4 [:span.strong "基本 "] "データ"]]
     [:div
      [table-template false :center
       [["名前" "性別" "年齢" "職業" "HP" "MP" "アイデア" "幸運" "知識" "DB"]
        (concat (list (data :name) (data :gender) (data :age) (data :occupation)) (vals (data :ability)))]]]
     [:div
      [table-template true :center
       [(map (comp str/upper-case name) (keys (data :params)))
        (vals (data :params))]]]
     [:div {:style {:text-align "left"}} [:h4 [:span.strong "探索 "] "技能リスト"]]
     [skills-template data 5 :exploration]
     [skills-template data 5 :negotiation]
     [skills-template data 4 :knowledge]
     [skills-template data 5 :action]]]
   ;; Right panel
   [:div.card {:class "inline-block" :style {:width 800 :height 400}}
    [:div.card-body {:style {:width 800}}
     [:div {:style {:text-align "left"}} [:h4 [:span.strong "戦闘 "] "技能リスト"]]
     [:div
      [skills-template data 4 :battle]]
     [:div
      [table-template false :left
       (cons ["装備品" "成功率" "ダメージ" "射程" "攻撃回数" "装弾数" "耐久力"]
             (for [eqp (data :equipments)] (vals eqp)))]]]]])

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
              _          (<? (timeout 2000)) ; Ugly. Quite ugly.
              cs-page    (clj->js ((js->clj (<p! (. browser pages))) 2))
              cs-content (<p! (. cs-page evaluate (fn [] (. (. js/document querySelector "pre") -innerHTML))))
              _          (println "Yomikomi owata ＼(^o^)／")]
          (. browser close)
          (println (str "Content is:\n" cs-content))
          (>! ch cs-content)))
    ch))

(defn create-cs-png [console-id]
  (println "create-cs-png")
  (let [ch (chan)]
    (go (let [char-id     (<? (remote/get-binded-char-id console-id :coc))
              cs-url      (<? (remote/get-cs-url char-id :coc))
              cs-text     (<? (scrape-cs-text cs-url))
              cs-data     (parser/chara-data cs-text)
              out-name    (str "coc-cs-" console-id ".png")
              browser     (<p! (. ppt launch (clj->js {;; :headless false
                                                       :args ["--no-sandbox"
                                                              "--disable-setuid-sandbox"]})))
              page        (<p! (. browser newPage))
              _           (<p! (. page setViewport (clj->js {:width 1800 :height 1100})))
              _           (<p! (. page goto (str "file://" const/root "/public/index.html")))
              elem-txt    (rdom/render-to-string [layouted-cs-hiccup cs-data])
              target-elem (<p! (. page $ "#app"))
              _           (. page evaluate (fn [elem]
                                             (let [target (. js/document getElementById "app")]
                                               (set! (. target -innerHTML) elem))) elem-txt)]
          (println "Screehshot...")
          (<? (timeout 500))
          (<p! (. page screenshot (clj->js {:path (str "./public/img/" out-name)
                                            :clip {:x      0
                                                   :y      0
                                                   :width  1700
                                                   :height 1050}})))
          (println "Screehshot!")
          (. browser close)
          (>! ch out-name)))
    ch))


