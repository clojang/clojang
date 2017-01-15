(ns clojang.core
  (:require [clojang.converter :as converter]))

;;; Aliases

(def ->erlang
  "An alias for ``clj->term``"
  #'converter/clj->term)

(def ->clojure
  "An alias for ``term->clj``"
  #'converter/term->clj)

;;; Functions

(defn !
  ""
  [process-name msg]
  )
