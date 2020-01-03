(ns cs-summary.db-local
  (:require [cs-summary.util :as util]
            [cs-summary.const :as const]))

(def default-db {:ready?       false
                 :cs-data-list []
                 :op-vars      (vec (take 10 (repeat {:param   :hp
                                                      :sign    :-
                                                      :digit-3 0
                                                      :digit-2 0
                                                      :digit-1 0})))
                 :char-vars    (vec (take 10 (repeat {:hp 0 :mp 0 :san 0})))})

(def db (atom default-db))

(defn set-default-db []
  (reset! db default-db))

(defn set-db [new-db]
  (reset! db new-db))

(defn set-cs-data-list [cs-data-list]
  (swap! db #(assoc % :cs-data-list cs-data-list)))

(defn ->param [num game]
  (println "->param " num " " game)
  (nth (const/params game) num))

(defn <-param [param game]
  (println "<-param " param " " game)
  (let [params (const/params game)]
    (util/index-of params param)))

(defn ->sign [num]
  (println "->sign")
  (let [mod-idx (mod num 2)]
    (case num
      0 :+
      1 :-)))

(defn <-sign [sign]
  (println "<-sign")
  (case sign
    :+ 0
    :- 1))

(defn changed-op-var [var cur-v game diff]
  (println "changed-op-var [" var " " cur-v " " game " " diff "]")
  (let [changed (+ diff (case var
                          :param (<-param cur-v game)
                          :sign  (<-sign cur-v)
                          cur-v))
        modded  (case var
                  :param (mod changed (count (const/params game)))
                  :sign  (mod changed 2)
                  (mod changed 10))]
    (case var
      :param (->param modded game)
      :sign  (->sign modded)
      modded)))

(defn set-op-vars [v]
  (swap! db assoc :op-vars v))

(defn set-op-var [char-id var v]
  (swap! db assoc-in [:op-vars char-id var] v))

(defn set-char-vars [v]
  (swap! db assoc :char-vars v))

(defn set-char-var [char-id var v]
  (swap! db assoc-in [:char-vars char-id var] v))

(defn reflect-op-var [char-id]
  (println "reflect-op-var")
  (let [op-var ((@db :op-vars) char-id)
        param (op-var :param)
        sign (name (op-var :sign))
        diff (js/parseInt (str (name (op-var :sign))
                               (op-var :digit-3)
                               (op-var :digit-2)
                               (op-var :digit-1)))]
    (swap! db update-in [:char-vars char-id param] #(+ % diff))
    (println (str "db [:char-vars " char-id " " param "] updated to " (get-in @db [:char-vars char-id param])))))

(defn activate []
  (swap! db assoc :ready? true))

(defn deactivate []
  (swap! db assoc :ready? false))
