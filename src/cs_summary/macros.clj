(ns cs-summary.macros)

(defmacro <? [ch]
  `(throw-err (async/<! ~ch)))

;; (defmacro async [& symbols]
;;   `(let [ch (async/chan)]
;;      ))
