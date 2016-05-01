(ns clojang.core
  (:require [clojure.string :as clj-string]
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
            [jiface.otp.nodes]
            [clojang.node :as node])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte float int]))

(declare edn->term term->edn)

(defn ->erl-array [xs]
  (into-array (types/object) (map edn->term xs)))

(defn ->clj-vector [xs]
  (map #(term->edn %) (into [] xs)))

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

(defprotocol EDNConverter
  "Convert EDN."
  (edn->term [this]
    "Convert Clojure EDN to JInterface Erlang types."))

(extend-protocol EDNConverter
  ;; nil
  nil
  (edn->term [edn]
    (types/atom "undefined"))
  ;; atom / keyword
  clojure.lang.Keyword
  (edn->term [edn]
    (types/atom (name edn)))
  ;; symbol
  clojure.lang.Symbol
  (edn->term [edn]
    (types/atom (str edn)))
  ;; boolean
  java.lang.Boolean
  (edn->term [edn]
    (types/boolean edn))
  ;; tuple /vector
  clojure.lang.PersistentVector
  (edn->term [edn]
    (vector->ji-tuple edn))
  clojure.lang.PersistentVector$ChunkedSeq
  (edn->term [edn]
    (vector->ji-tuple edn))
  ;; list
  clojure.lang.PersistentList
  (edn->term [edn]
    (list->ji-list edn))
  clojure.lang.PersistentList$EmptyList
  (edn->term [edn]
    (list->ji-list))
  ;; char
  java.lang.Character
  (edn->term [edn]
    (types/char edn))
  ;; map
  clojure.lang.PersistentArrayMap
  (edn->term [edn]
    (map->ji-map edn))
  ;; long
  java.lang.Long
  (edn->term [edn]
    (types/long edn))
  clojure.lang.BigInt
  (edn->term [edn]
    (types/long edn))
  java.math.BigInteger
  (edn->term [edn]
    (types/long edn))
  ;; byte
  java.lang.Byte
  (edn->term [edn]
    (types/byte edn))
  ;; char
  java.lang.Character
  (edn->term [edn]
    (types/char edn))
  ;; int
  java.lang.Integer
  (edn->term [edn]
    (types/int edn))
  ;; short
  java.lang.Short
  (edn->term [edn]
    (types/short edn))
  ; ;; uint
  ; java.lang.Integer
  ; (edn->term [edn]
  ;   (types/uint edn))
  ; ;; ushort
  ; java.lang.Short
  ; (edn->term [edn]
  ;   (types/ushort edn))
  ;; float
  java.lang.Float
  (edn->term [edn]
    (types/float edn))
  ;; double
  java.lang.Double
  (edn->term [edn]
    (types/double edn))
  ;; string
  java.lang.String
  (edn->term [edn]
    (types/string edn))
  ;; XXX JInterface objects
  com.ericsson.otp.erlang.OtpConnection
  (edn->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpNode
  (edn->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpMbox
  (edn->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpErlangPid
  (edn->term [obj]
    obj)
  com.ericsson.otp.erlang.OtpErlangObject
  (edn->term [obj]
    obj))

(defprotocol TermConverter
  "Convert JInterface Erlang terms."
  (term->edn [this]
    "Convert JInterface Erlang types to EDN."))

(extend-protocol TermConverter
  ;; nil
  nil
  (term->edn [erl-obj]
    erl-obj)
  ;; atom / keyword & undefined
  com.ericsson.otp.erlang.OtpErlangAtom
  (term->edn [erl-obj]
    (let [trans (atom/->str erl-obj)]
      (if (= trans "undefined")
        nil
        (keyword (clj-string/replace trans "'" "")))))
  ;; boolean
  com.ericsson.otp.erlang.OtpErlangBoolean
  (term->edn [erl-obj]
    (boolean/get-boolean-value erl-obj))
  ;; tuple /vector
  com.ericsson.otp.erlang.OtpErlangTuple
  (term->edn [erl-obj]
    (into [] (->clj-vector (tuple/get-elements erl-obj))))
  ;; list
  com.ericsson.otp.erlang.OtpErlangList
  (term->edn [erl-obj]
    (->clj-vector erl-obj))
  ;; char
  com.ericsson.otp.erlang.OtpErlangChar
  (term->edn [erl-obj]
    (int/get-char-value erl-obj))
  ;; map
  com.ericsson.otp.erlang.OtpErlangMap
  (term->edn [erl-obj]
    (ji-map->map erl-obj))
  ;; long
  com.ericsson.otp.erlang.OtpErlangLong
  (term->edn [erl-obj]
    (int/get-bigint-value erl-obj))
  ;; byte
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-byte-value erl-obj))
  ;; char
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-char-value erl-obj))
  ;; int
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-int-value erl-obj))
  ;; short
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-short-value erl-obj))
  ;; uint
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-uint-value erl-obj))
  ;; ushort
  com.ericsson.otp.erlang.OtpErlangShort
  (term->edn [erl-obj]
    (int/get-ushort-value erl-obj))
  ;; float
  com.ericsson.otp.erlang.OtpErlangFloat
  (term->edn [erl-obj]
    (float/get-float-value erl-obj))
  ;; double
  com.ericsson.otp.erlang.OtpErlangDouble
  (term->edn [erl-obj]
    (float/get-double-value erl-obj))
  ;; pid
  com.ericsson.otp.erlang.OtpErlangPid
  (term->edn [erl-obj]
    erl-obj)
  ;; ref
  com.ericsson.otp.erlang.OtpErlangRef
  (term->edn [erl-obj]
    erl-obj)
  ;; XXX Clojure/Java objects ...
  java.lang.Object
  (term->edn [obj]
    obj)
  ;; string
  com.ericsson.otp.erlang.OtpErlangString
  (term->edn [erl-obj]
    (clj-string/replace
      (string/->str erl-obj)
      #"\""
      "")))

;;; Aliases

(def ->erlang "An alias for ``edn->term``" #'edn->term)
(def ->clojure "An alias for ``term->edn``" #'term->edn)
