(ns clojang.types
  (:require [clojang.core :as clojang]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean char list long]))

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
  [arg]
  (clojang/->erlang arg))

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

(defn short
  ""
  [arg]
  (clojang/->erlang arg))
