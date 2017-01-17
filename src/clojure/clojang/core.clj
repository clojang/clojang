(ns clojang.core
  (:require [clojang.types.core :as types]
            [clojang.mbox :as mbox]))

;;; Aliases

(def ->erl #'types/clj->erl)

(def ->clj #'types/erl->clj)

;;; Functions

(def ! #'mbox/send)
(def receive #'mbox/receive)
(def self #'mbox/self)
