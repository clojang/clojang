(ns clojang.core
  (:require [clojang.converter :as converter]
            [clojang.mbox :as mbox]))

;;; Aliases

(def ->erl
  "An alias for ``clj->term``"
  #'converter/clj->term)

(def ->clj
  "An alias for ``term->clj``"
  #'converter/term->clj)

;;; Functions

(def ! #'mbox/send)
(def receive #'mbox/receive)
(def self #'mbox/self)
