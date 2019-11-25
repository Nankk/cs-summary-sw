(ns cs-summary.coc-parser
  (:require [clojure.string :as str]))

;; Text extraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- name-part [text]
  (re-find #"キャラクター名：.*\r?\n" text))

(defn- equipment-part [text]
  (first (re-find #"■戦闘■\r?\n\r?\n([^\r\n]+\r?\n)+" text)))

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

(defn- parse-char-name [text]
  (let [lines   (str/split-lines text)
        targets (subvec lines 0)]
    {:name (str/trim ((re-find #"キャラクター名：(.+)" text) 1))}))

(defn- parse-equipment [text]
  (let [lines      (str/split-lines text)
        header     (lines 3)
        body-lines (subvec lines 4)
        num-start  (+ (str/index-of header "成功率") 2)]
    {:equipments (vec (for [l body-lines]
                        (let [diff  (str/index-of l " ") ; double-byte letter treatment
                              start (- num-start diff)]
                          {:name         (str/trim (subs l 0 diff))
                           :criteria     (if (js/isNaN (js/parseInt (subs l (+ start 0) (+ start 7)))) 0 (js/parseInt (subs l (+ start 0) (+ start 7))))
                           :damage       (str/trim (subs l (+ start 7) (+ start 15)))
                           :range        (if (js/isNaN (js/parseInt (subs l (+ start 17) (+ start 25)))) 0 (js/parseInt (subs l (+ start 17) (+ start 25))))
                           :attack-times (if (js/isNaN (js/parseInt (subs l (+ start 26) (+ start 35)))) 0 (js/parseInt (subs l (+ start 26) (+ start 35))))
                           :bullets      (if (js/isNaN (js/parseInt (subs l (+ start 36) (+ start 43)))) 0 (js/parseInt (subs l (+ start 36) (+ start 43))))
                           :durability   (if (js/isNaN (js/parseInt (subs l (+ start 44) (+ start 51)))) 0 (js/parseInt (subs l (+ start 44) (+ start 51))))})))}))

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
        (merge (parse-char-name (name-part text)))
        (merge (parse-equipment (equipment-part text)))
        (merge-with into (parse-skills (battle-skills-part text) :battle))
        (merge-with into (parse-skills (exploration-skills-part text) :exploration))
        (merge-with into (parse-skills (action-skills-part text) :action))
        (merge-with into (parse-skills (negotiation-skills-part text) :negotiation))
        (merge-with into (parse-skills (knowledge-skills-part text) :knowledge)))))

