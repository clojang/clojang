(ns clojang.core
  (:require [clojang.converter :as converter]))

;;; Aliases

(def ->erl
  "An alias for ``clj->term``"
  #'converter/clj->term)

(def ->clj
  "An alias for ``term->clj``"
  #'converter/term->clj)

;;; Functions

(defn !
  ""
  [process-name msg]
  )
