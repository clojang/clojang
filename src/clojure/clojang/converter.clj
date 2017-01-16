(ns clojang.converter
  (:require [clojang.node :as node]
            [clojure.string :as clj-string]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]]
            [jiface.erlang.atom :as atom]
            [jiface.erlang.atom :as boolean]
            [jiface.erlang.float :as float]
            [jiface.erlang.int :as int]
            [jiface.erlang.map :as map-type]
            [jiface.erlang.string :as string]
            [jiface.erlang.tuple :as tuple]
            [jiface.erlang.types :as types]
            [jiface.otp.nodes])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte float int]))

(declare clj->term
         term->clj)

;;; Helper Functions

;; XXX standardize the following ...

(defn ->erl-array [xs]
  "Convert a Clojure seq into a Java array of Erlang JInterface obects."
  (into-array (types/object) (map clj->term xs)))

(defn ->clj-vector [xs]
  (map #(term->clj %) (into [] xs)))

(defn vector->ji-tuple
  "Convert a Clojure vector into an Erlang JInterface tuple."
  [v]
  (types/tuple (->erl-array v)))

(defn list->ji-list
  "Convert a Clojure list into an Erlang JInterface list."
  ([]
    (types/list))
  ([v]
    (types/list (->erl-array v))))

(defn map->ji-map
  "Convert a Clojure map into an Erlang JInterface map."
  ([]
    (types/map))
  ([m]
    (types/map (->erl-array (keys m))
               (->erl-array (vals m)))))

(defn ji-map->map
  "Convert an Erlang JInterface map into a Clojure map."
  [erl-obj]
  ;; XXX Is it guaranteed that keys/values will be extracted in a correlated
  ;; order? If not, we'll need to go one-by-one ...
  ; (apply hash-map (interleave (->clj-vector
  ;                                (map-type/get-keys erl-obj))
  ;                             (->clj-vector
  ;                                (map-type/get-values erl-obj)))))
  (let [erl-map-keys (map-type/get-keys erl-obj)
        map-keys (->clj-vector erl-map-keys)
        map-vals (->clj-vector (map #(map-type/get erl-obj %)
                                    erl-map-keys))]
    (apply hash-map (interleave map-keys map-vals))))

;;; Protocols

(defprotocol ClojureTypesConverter
  (clj->term [this]
    "Convert Clojure types to Erlang/JInterface types."))

(extend-protocol ClojureTypesConverter
  ;; nil
  nil
  (clj->term [clj-obj]
    (types/atom "undefined"))
  ;; atom / keyword
  clojure.lang.Keyword
  (clj->term [clj-obj]
    (types/atom (name clj-obj)))
  ;; symbol
  clojure.lang.Symbol
  (clj->term [clj-obj]
    (types/atom (str clj-obj)))
  ;; boolean
  java.lang.Boolean
  (clj->term [clj-obj]
    (types/boolean clj-obj))
  ;; tuple /vector
  clojure.lang.PersistentVector
  (clj->term [clj-obj]
    (vector->ji-tuple clj-obj))
  clojure.lang.PersistentVector$ChunkedSeq
  (clj->term [clj-obj]
    (vector->ji-tuple clj-obj))
  ;; list
  clojure.lang.PersistentList
  (clj->term [clj-obj]
    (list->ji-list clj-obj))
  clojure.lang.PersistentList$EmptyList
  (clj->term [clj-obj]
    (list->ji-list))
  ;; char
  java.lang.Character
  (clj->term [clj-obj]
    (types/char clj-obj))
  ;; map
  clojure.lang.PersistentArrayMap
  (clj->term [clj-obj]
    (map->ji-map clj-obj))
  ;; long
  java.lang.Long
  (clj->term [clj-obj]
    (types/long clj-obj))
  clojure.lang.BigInt
  (clj->term [clj-obj]
    (types/long clj-obj))
  java.math.BigInteger
  (clj->term [clj-obj]
    (types/long clj-obj))
  ;; byte
  java.lang.Byte
  (clj->term [clj-obj]
    (types/byte clj-obj))
  ;; char
  java.lang.Character
  (clj->term [clj-obj]
    (types/char clj-obj))
  ;; int
  java.lang.Integer
  (clj->term [clj-obj]
    (types/int clj-obj))
  ;; short
  java.lang.Short
  (clj->term [clj-obj]
    (types/short clj-obj))
  ; ;; uint
  ; java.lang.Integer
  ; (clj->term [clj-obj]
  ;   (types/uint clj-obj))
  ; ;; ushort
  ; java.lang.Short
  ; (clj->term [clj-obj]
  ;   (types/ushort clj-obj))
  ;; float
  java.lang.Float
  (clj->term [clj-obj]
    (types/float clj-obj))
  ;; double
  java.lang.Double
  (clj->term [clj-obj]
    (types/double clj-obj))
  ;; string
  java.lang.String
  (clj->term [clj-obj]
    (types/string clj-obj))
  ;; XXX JInterface objects
  com.ericsson.otp.erlang.OtpConnection
  (clj->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpNode
  (clj->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpMbox
  (clj->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpErlangPid
  (clj->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpErlangObject
  (clj->term [obj]
    obj))

(defprotocol ErlangTermConverter
  (term->clj [this]
    "Convert Erlang/JInterface terms to Clojure types."))

(extend-protocol ErlangTermConverter
  ;; nil
  nil
  (term->clj [erl-obj]
    erl-obj)
  ;; atom / keyword & undefined
  com.ericsson.otp.erlang.OtpErlangAtom
  (term->clj [erl-obj]
    (let [trans (atom/->str erl-obj)]
      (if (= trans "undefined")
        nil
        (keyword (clj-string/replace trans "'" "")))))
  ;; boolean
  com.ericsson.otp.erlang.OtpErlangBoolean
  (term->clj [erl-obj]
    (boolean/get-boolean-value erl-obj))
  ;; tuple /vector
  com.ericsson.otp.erlang.OtpErlangTuple
  (term->clj [erl-obj]
    (into [] (->clj-vector (tuple/get-elements erl-obj))))
  ;; list
  com.ericsson.otp.erlang.OtpErlangList
  (term->clj [erl-obj]
    (->clj-vector erl-obj))
  ;; char
  com.ericsson.otp.erlang.OtpErlangChar
  (term->clj [erl-obj]
    (int/get-char-value erl-obj))
  ;; map
  com.ericsson.otp.erlang.OtpErlangMap
  (term->clj [erl-obj]
    (ji-map->map erl-obj))
  ;; long
  com.ericsson.otp.erlang.OtpErlangLong
  (term->clj [erl-obj]
    (int/get-bigint-value erl-obj))
  ;; byte
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-byte-value erl-obj))
  ;; char
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-char-value erl-obj))
  ;; int
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-int-value erl-obj))
  ;; short
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-short-value erl-obj))
  ;; uint
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-uint-value erl-obj))
  ;; ushort
  com.ericsson.otp.erlang.OtpErlangShort
  (term->clj [erl-obj]
    (int/get-ushort-value erl-obj))
  ;; float
  com.ericsson.otp.erlang.OtpErlangFloat
  (term->clj [erl-obj]
    (float/get-float-value erl-obj))
  ;; double
  com.ericsson.otp.erlang.OtpErlangDouble
  (term->clj [erl-obj]
    (float/get-double-value erl-obj))
  ;; pid
  com.ericsson.otp.erlang.OtpErlangPid
  (term->clj [erl-obj]
    erl-obj)
  ;; ref
  com.ericsson.otp.erlang.OtpErlangRef
  (term->clj [erl-obj]
    erl-obj)
  ;; XXX Clojure/Java objects ...
  java.lang.Object
  (term->clj [obj]
    obj)
  ;; string
  com.ericsson.otp.erlang.OtpErlangString
  (term->clj [erl-obj]
    (clj-string/replace
      (string/->str erl-obj)
      #"\""
      "")))
