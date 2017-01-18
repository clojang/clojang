(ns clojang.core
  (:require [clojang.types.core :as types]
            [clojang.mbox :as mbox]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def
  "Convert Clojure types to Erlang/JInterface types."
  ->erl #'types/clj->erl)

(def
  "Convert Erlang/JInterface types to Clojure types."
  ->clj #'types/erl->clj)

(def ! #'mbox/send)
(def receive #'mbox/receive)
(def self #'mbox/self)
