(ns clojang.core
  (:require [clojang.types.converter :as converter]
            [clojang.mbox :as mbox]))

;;; Aliases

(def ->erl #'converter/clj->erl)

(def ->clj #'converter/erl->clj)

;;; Functions

(def ! #'mbox/send)
(def receive #'mbox/receive)
(def self #'mbox/self)
