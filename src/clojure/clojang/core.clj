(ns clojang.core
  (:require
    [clojang.mbox :as mbox]
    [clojang.types.core :as types]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def
  #^{:doc "Convert Clojure types to Erlang/JInterface types."}
  ->erl #'types/clj->erl)

(def
  #^{:doc "Convert Erlang/JInterface types to Clojure types."}
  ->clj #'types/erl->clj)

(def ! #'mbox/send)
(def receive #'mbox/receive)
(def self #'mbox/self)
