(ns clojang.types
  (:require [clojang.core :as clojang]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte char double float int list long
                            map short]))

(defn undefined
  ""
  []
  (clojang/->erlang nil))

(defn atom
  ""
  [arg]
  (clojang/->erlang arg))

(defn boolean
  ""
  [arg]
  (clojang/->erlang arg))

(defn tuple
  ""
  [arg]
  (clojang/->erlang arg))

(defn list
  ""
  ([]
    (clojang/->erlang '()))
  ([arg]
    (clojang/->erlang arg)))

(defn char
  ""
  [arg]
  (clojang/->erlang arg))

(defn string
  ""
  [arg]
  (clojang/->erlang arg))

(defn long
  ""
  [arg]
  (clojang/->erlang arg))

(defn byte
  ""
  [arg]
  (clojang/->erlang arg))

(defn char
  ""
  [arg]
  (clojang/->erlang arg))

(defn int
  ""
  [arg]
  (clojang/->erlang arg))

(defn short
  ""
  [arg]
  (clojang/->erlang arg))

(defn uint
  ""
  [arg]
  (clojang/->erlang arg))

(defn ushort
  ""
  [arg]
  (clojang/->erlang arg))

(defn float
  ""
  [arg]
  (clojang/->erlang arg))

(defn double
  ""
  [arg]
  (clojang/->erlang arg))

(defn map
  ""
  [arg]
  (clojang/->erlang arg))
