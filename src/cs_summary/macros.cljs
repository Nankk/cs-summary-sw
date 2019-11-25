(ns cs-summary.macros
  (:require-macros
   [cs-summary.macros])
  (:require
   [cljs.core.async :as async]))

(defn error? [x]
  (instance? js/Error x))

(defn throw-err [e]
  (when (error? e) (throw e))
  e)

