(ns clojang.types.constructor
  (:require [clojang.types.converter :as types]
            [clojang.types.predicate]
            [clojang.types.records :as records]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [atom boolean byte char double
                            float int list long map ref short]))

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
  (types/clj->erl
    (records/->Pid node id serial creation)))

(defn port
  ""
  ([node id creation]
    (types/clj->erl
      (records/->Port node id creation)))
  ([tag node id creation]
    (types/clj->erl
      (records/->Port node id creation))))

(defn ref
  ""
  ([node ids creation]
    (types/clj->erl
      (records/->Ref node ids creation)))
  ([tag node ids creation]
    (types/clj->erl
      (records/->Ref node ids creation))))
