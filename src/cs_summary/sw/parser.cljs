(ns cs-summary.sw.parser
  (:require [clojure.string :as str]))

;; Text extraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- weapon-part [text]
  (first (re-find #"・武器\r?\n([^\r\n]+\r?\n)+" text)))

(defn- armor-part [text]
  (first (re-find #"・防具\r?\n([^\r\n]+\r?\n)+" text)))

(defn- ability-part [text]
  (first (re-find #"■レベル・技能■\r?\n.*\r?\n.*\r?\n([^\r\n]+(\r|\n))+" text)))

(defn- skills-part [text]
  (first (re-find #"■戦闘特技・値■\r?\n([^\r\n]+\r?\n)+" text)))

(defn- params-part [text]
  (first (re-find #"■能力値■\r?\n([^\r\n]+\r?\n)+" text)))

(defn- magics-part [text]
  (let [match (re-find #"■魔力■\r?\n([^\r\n]+\r?\n)+" text)]
    (if match
      (first match)
      nil)))

(defn- name-part [text]
  (re-find #"キャラクター名：.*\r?\n" text))

(defn- money-part [text]
  (re-find #"所持金.*\r?\n" text))

(defn- vitality-part [text]
  (first (re-find #"生命 +精神\r?\n([^\r\n]+\r?\n)+" text)))

(defn- knowledge-part [text]
  (first (re-find #"魔物 +全力\r?\n([^\r\n]+\r?\n)+" text)))

;; Parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-weapon [text]
  (let [lines  (str/split-lines text)
        target (lines 2)]
    {:critical     (js/parseInt (subs target 36 40))
     :weapon-power (js/parseInt (subs target 29 34))
     :bonus        (js/parseInt (subs target 41 46))
     :accuracy     (js/parseInt (subs target 24 28))}))

(defn- parse-armor [text]
  (let [lines  (str/split-lines text)
        target (lines 5)]
    {:flee (js/parseInt (subs target 11 15))
     :def  (js/parseInt (subs target 17 21))}))

(defn- parse-level&ability [text-raw]
  (let [text      (str/replace text-raw #"／" "\n")
        lines     (str/split-lines text)
        l-target  (lines 1)
        a-targets (subvec lines 3)]
    {:level     (js/parseInt ((re-find #"冒険者レベル：([0-9]+)\s*Lv" l-target) 1))
     :abilities (vec (for [line a-targets
                           :when (re-find #"^\s*([^\s0-9]+)\s*([0-9]+)\s*Lv" line)]
                       (let [match (re-find #"^\s*([^\s0-9]+)\s*([0-9]+)\s*Lv" line)]
                         {:name  (str/trim (match 1))
                          :level (js/parseInt (match 2))})))}))

(defn- parse-skills [text]
  (let [lines  (str/split-lines text)
        targets (subvec lines 2)
        skills {:skills (let [re #"\[p([^\]]+)\]\s*([^ ]+)\s*:\s*([^ ]+)\s*:"]
                          (vec (for [line targets
                                     :when (re-find re line)]
                                 (let [match (re-find re line)]
                                   {:name (str/trim (match 2))
                                    :description (str/trim (match 3))}))))}]
    skills))

(defn- parse-params [text]
  (let [lines        (str/split-lines text)
        target-raw   (lines 7)
        target-bonus (lines 8)]
    {:params {:raw   {:dex (js/parseInt (subs target-raw 5 9))
                      :agi (js/parseInt (subs target-raw 11 15))
                      :str (js/parseInt (subs target-raw 16 21))
                      :con (js/parseInt (subs target-raw 22 27))
                      :int (js/parseInt (subs target-raw 29 33))
                      :pow (js/parseInt (subs target-raw 35))}
              :bonus {:dex (js/parseInt (subs target-bonus 7 11))
                      :agi (js/parseInt (subs target-bonus 13 17))
                      :str (js/parseInt (subs target-bonus 18 23))
                      :con (js/parseInt (subs target-bonus 24 29))
                      :int (js/parseInt (subs target-bonus 31 35))
                      :pow (js/parseInt (subs target-bonus 37))}}}))

(defn- parse-magics [text]
  (if text
    (let [lines   (str/split-lines text)
          targets (subvec lines 3)]
      {:magics (let [re #"^([^ ]+)\s*([0-9]+)[^0-9]+([0-9]+)"]
                 (vec (for [line targets]
                        {:name  (str/trim ((re-find re line) 1))
                         :level (js/parseInt ((re-find re line) 2))
                         :power (js/parseInt ((re-find re line) 3))})))})
    {:magics nil}))

(defn- parse-char-name [text]
  (let [lines   (str/split-lines text)
        targets (subvec lines 0)]
    {:name (str/trim ((re-find #"キャラクター名：(.+)" text) 1))}))

(defn- parse-money [text]
  (let [lines   (str/split-lines text)
        targets (subvec lines 0)]
    {:money (js/parseInt ((re-find #"所持金\s*([0-9]+)" text) 1))}))

(defn- parse-vitality [text]
  (let [lines  (str/split-lines text)
        target (lines 5)]
    {:reg-bio    (js/parseInt (subs target 5 9))
     :reg-spirit (js/parseInt (subs target 11 15))
     :hp (js/parseInt (subs target 16 21))
     :mp (js/parseInt (subs target 22))}))

(defn- parse-knowledge [text]
  (let [lines  (str/split-lines text)
        target (lines 5)]
    {:knowledge  (js/parseInt (subs target 5 9))
     :initiative (js/parseInt (subs target 11 15))}))

;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some data might not exist needs nil-check (e.g. magics)

(defn chara-data [text-raw]
  (println "chara-data")
  (let [text (str/replace text-raw #"　" "  ")] ;; THE MADNESS
    (-> {}
        (merge (parse-weapon (weapon-part text)))
        (merge (parse-armor (armor-part text)))
        (merge (parse-level&ability (ability-part text)))
        (merge (parse-skills (skills-part text)))
        (merge (parse-params (params-part text)))
        (merge (parse-magics (magics-part text)))
        (merge (parse-char-name (name-part text)))
        (merge (parse-money (money-part text)))
        (merge (parse-vitality (vitality-part text)))
        (merge (parse-knowledge (knowledge-part text))))))
