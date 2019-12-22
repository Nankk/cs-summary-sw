(ns cs-summary.sw.png-maker
  (:require
   ["express" :as express]
   ["stream" :as stream]
   ["fs" :as fs]
   ["puppeteer" :as ppt]
   [clojure.string :as str]
   [cljs.core.async :as async :refer [>! <! go chan timeout go-loop]]
   [async-interop.interop :refer-macros [<p!]]
   [cs-summary.sw.parser :as parser]
   [cs-summary.db-remote :as remote]
   [reagent.dom.server :as rdom]
   [cs-summary.util :as util]
   [cs-summary.const :as const]
   [cs-summary.macros :refer [throw-err] :refer-macros [<?]]))

;; SW character sheet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- table-template [fixed? rows]
  (println "table-template")
  [:table.table (when fixed? {:class "fixed"})
   (let [header (first rows)]
     [:thead
      [:tr
       (for [v header]
         ^{:key (util/next-key)} [:th v])]])
   (let [data-rows (rest rows)]
     [:tbody
      (for [row data-rows]
        ^{:key (util/next-key)} [:tr
                                 (for [v row]
                                   ^{:key (util/next-key)} [:td v])])])])

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
       [:div.inline-block {:style {:width 300}} [table-template true (vec (cons ["技能" "レベル"] (vec (for [ability (data :abilities)] ^{:key (util/next-key)} [(ability :name) (ability :level)]))))]]]
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
                    (vec (for [skill (data :skills)] ^{:key (util/next-key)}
                              [(skill :name) (skill :description)]))))]]
       [:div.inline-block {:style {:width 250}}
        [table-template false
         (vec (cons ["魔法技能" "レベル" "魔力"]
                    (vec (for [magic (data :magics)]
                           ^{:key (util/next-key)} [(magic :name) (magic :level) (magic :power)]))))]]]]]]
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
              _          (println "Yomikomi owata ＼(^o^)／")]
          (. browser close)
          (println (str "Content is:\n" cs-content))
          (>! ch cs-content)))
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

(defn create-cs-data [url]
  (let [ch (chan)]
    (go (let [cs-text     (<? (scrape-cs-text url))
              cs-data     (parser/chara-data cs-text)
              cs-data+    (assoc cs-data :package-values (package-values cs-data))]
          (>! ch cs-data+)))
    ch))

(defn create-cs-png [cs-data char-id]
  (println "create-cs-png")
  (let [ch (chan)]
    (go (let [out-name    (str "sw-cs-" char-id ".png")
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

