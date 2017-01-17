(ns clojang.types.constructor
  (:require [clojang.core :as clojang]
            [clojang.types.predicate]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [atom boolean byte char double
                            float int list long map short]))

(defn atom
  ""
  [arg]
  (clojang/->erl arg))

(defn boolean
  ""
  [arg]
  (clojang/->erl arg))

(defn byte
  ""
  [arg]
  (clojang/->erl arg))

(defn char
  ""
  [arg]
  (clojang/->erl arg))

(defn double
  ""
  [arg]
  (clojang/->erl arg))

(defn float
  ""
  [arg]
  (clojang/->erl arg))

(defn int
  ""
  [arg]
  (clojang/->erl arg))

(defn list
  ""
  ([]
    (clojang/->erl '()))
  ([arg]
    (clojang/->erl arg)))

(defn long
  ""
  [arg]
  (clojang/->erl arg))

(defn map
  ""
  [arg]
  (clojang/->erl arg))

(defn short
  ""
  [arg]
  (clojang/->erl arg))

(defn string
  ""
  [arg]
  (clojang/->erl arg))

(defn tuple
  ""
  [arg]
  (clojang/->erl arg))

(defn undefined
  ""
  []
  (clojang/->erl nil))

(defn uint
  ""
  [arg]
  (clojang/->erl arg))

(defn ushort
  ""
  [arg]
  (clojang/->erl arg))

(defn pid
  ""
  [node id serial creation]
  (clojang/->erl [node id serial creation]))
