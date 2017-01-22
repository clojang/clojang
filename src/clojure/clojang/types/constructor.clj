(ns clojang.types.constructor
  (:require [clojang.types.converter :as types]
            [clojang.types.predicate]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [atom boolean byte char double
                            float int list long map short]))

(defn atom
  ""
  [arg]
  (types/clj->erl arg))

(defn boolean
  ""
  [arg]
  (types/clj->erl arg))

(defn byte
  ""
  [arg]
  (types/clj->erl arg))

(defn char
  ""
  [arg]
  (types/clj->erl arg))

(defn double
  ""
  [arg]
  (types/clj->erl arg))

(defn float
  ""
  [arg]
  (types/clj->erl arg))

(defn int
  ""
  [arg]
  (types/clj->erl arg))

(defn list
  ""
  ([]
    (types/clj->erl '()))
  ([arg]
    (types/clj->erl arg)))

(defn long
  ""
  [arg]
  (types/clj->erl arg))

(defn map
  ""
  [arg]
  (types/clj->erl arg))

(defn short
  ""
  [arg]
  (types/clj->erl arg))

(defn string
  ""
  [arg]
  (types/clj->erl arg))

(defn tuple
  ""
  [arg]
  (types/clj->erl arg))

(defn undefined
  ""
  []
  (types/clj->erl nil))

(defn uint
  ""
  [arg]
  (types/clj->erl arg))

(defn ushort
  ""
  [arg]
  (types/clj->erl arg))

(defn pid
  ""
  [node id serial creation]
  (types/clj->erl [node id serial creation]))

(defn port
  ""
  ([buf]
    (types/clj->erl [buf]))
  ([node id creation]
    (types/clj->erl [node id creation]))
  ([tag node id creation]
    (types/clj->erl [tag node id creation])))

(defn ref
  ""
  ([buf]
    (types/clj->erl [buf]))
  ([node ids creation]
    (types/clj->erl [node ids creation]))
  ([tag node ids creation]
    (types/clj->erl [tag node ids creation])))
