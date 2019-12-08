(ns cs-summary.coc.parser
  (:require [clojure.string :as str]))

;; Text extraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- character-part [text]
  (re-find #"キャラクター名：[\s\S]+出身" text))

(defn- equipment-part [text]
  (first (re-find #"■戦闘■\r?\n\r?\n([^\r\n]+\r?\n)+" text)))

(defn- params-part [text]
  (first (re-find #"■簡易用■\r?\n([^\r\n]+\r?\n)+" text)))

(defn- battle-skills-part [text]
  (first (re-find #"------------------------ 戦闘系技能 ------------------------\r?\n([^\r\n]+\r?\n)+" text)))

(defn- exploration-skills-part [text]
  (first (re-find #"------------------------ 探索系技能 ------------------------\r?\n([^\r\n]+\r?\n)+" text)))

(defn- action-skills-part [text]
  (first (re-find #"------------------------ 行動系技能 ------------------------\r?\n([^\r\n]+\r?\n)+" text)))

(defn- negotiation-skills-part [text]
  (first (re-find #"------------------------ 交渉系技能 ------------------------\r?\n([^\r\n]+\r?\n)+" text)))

(defn- knowledge-skills-part [text]
  (first (re-find #"------------------------ 知識系技能 ------------------------\r?\n([^\r\n]+\r?\n)+" text)))

;; Parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-char-data [text]
  {:name (str/trim ((re-find #"キャラクター名：(.+)" text) 1))
   :age  (str/trim ((re-find #"年齢：([0-9]+)" text) 1))
   :gender (str/trim ((re-find #"性別：(.+)" text) 1))
   :occupation (str/trim ((re-find #"職業：(.+)" text) 1))})

(defn- parse-params [text]
  (let [lines   (str/split-lines text)
        targets (subvec lines 2)
        first   (re-find #".*STR:([0-9]+)\s*DEX:([0-9]+)\s*INT:([0-9]+)\s*ｱｲﾃﾞｱ:([0-9]+)" (targets 0))
        second  (re-find #".*CON:([0-9]+) *APP:([0-9]+) *POW:([0-9]+) *幸 運:([0-9]+)" (targets 1))
        third   (re-find #".*SIZ:([0-9]+) *SAN:([0-9]+) *EDU:([0-9]+) *知 識:([0-9]+)" (targets 2))
        fourth  (re-find #".*H P:([0-9]+) *M P:([0-9]+) *回避:[^ ]+ *ﾀﾞﾒｰｼﾞﾎﾞｰﾅｽ:(.*)" (targets 3))]
    {:params  {:str (js/parseInt (first 1))
               :dex (js/parseInt (first 2))
               :int (js/parseInt (first 3))
               :con (js/parseInt (second 1))
               :app (js/parseInt (second 2))
               :pow (js/parseInt (second 3))
               :siz (js/parseInt (third 1))
               :san (* 5 (js/parseInt (second 3)))
               :edu (js/parseInt (third 3))}
     :ability {:hp-max    (js/parseInt (fourth 1))
               :mp-max    (js/parseInt (fourth 2))
               :idea      (js/parseInt (first 4))
               :luck      (js/parseInt (second 4))
               :knowledge (js/parseInt (third 4))
               :db        (js/parseInt (fourth 3))}}))

(defn- parse-equipment [text]
  (let [lines      (str/split-lines text)
        header     (lines 3)
        body-lines (subvec lines 4)
        num-start  (+ (str/index-of header "成功率") 2) ; need to detect the starting position (variable)
        ]
    (if (= "/" (str/trim (body-lines 0))) ; Skip if empty; character maker has 2 empty records by default
      {:equipments []}
      {:equipments (vec (for [line body-lines]
                          (let [diff  (str/index-of line " ") ; double-byte letter treatment
                                start (- num-start diff)]
                            {:name         (str/trim (subs line 0 diff))
                             :criteria     (if (js/isNaN (js/parseInt (subs line (+ start 0) (+ start 7)))) 0 (js/parseInt (subs line (+ start 0) (+ start 7))))
                             :damage       (str/trim (subs line (+ start 7) (+ start 15)))
                             :range        (if (js/isNaN (js/parseInt (subs line (+ start 17) (+ start 25)))) 0 (js/parseInt (subs line (+ start 17) (+ start 25))))
                             :attack-times (if (js/isNaN (js/parseInt (subs line (+ start 26) (+ start 35)))) 0 (js/parseInt (subs line (+ start 26) (+ start 35))))
                             :bullets      (if (js/isNaN (js/parseInt (subs line (+ start 36) (+ start 43)))) 0 (js/parseInt (subs line (+ start 36) (+ start 43))))
                             :durability   (if (js/isNaN (js/parseInt (subs line (+ start 44) (+ start 51)))) 0 (js/parseInt (subs line (+ start 44) (+ start 51))))})))})))

(defn- parse-skills [text category]
  (let [lines (str/split-lines text)
        targets (subvec lines 2)]
    {:skills (vec
              (filter
               #(not-empty (% :name))
               (flatten
                (for [l targets]
                  (let [m (re-find #"《([^》]*)》([^％]*)％[^《]*《([^》]*)》([^％]*)％[^《]*《([^》]*)》([^％]*)％" l)]
                    [{:name (m 1) :proficiency (js/parseInt (m 2)) :category category}
                     {:name (m 3) :proficiency (js/parseInt (m 4)) :category category}
                     {:name (m 5) :proficiency (js/parseInt (m 6)) :category category}])))))}))

;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some data might not exist needs nil-check (e.g. magics)

(defn chara-data [text-raw]
  (println "chara-data")
  (let [text (str/replace text-raw #"　" "  ")]
    (->> {}
         (merge (parse-char-data (character-part text)))
         (merge (parse-equipment (equipment-part text)))
         (merge (parse-params (params-part text)))
         (merge-with into (parse-skills (battle-skills-part text) :battle))
         (merge-with into (parse-skills (exploration-skills-part text) :exploration))
         (merge-with into (parse-skills (action-skills-part text) :action))
         (merge-with into (parse-skills (negotiation-skills-part text) :negotiation))
         (merge-with into (parse-skills (knowledge-skills-part text) :knowledge)))))

