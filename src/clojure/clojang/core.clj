(ns clojang.core
  (:require [clojure.string :as clj-string]
            [dire.core :refer [with-handler!]]
            [clojang.jinterface.erlang.atom :as atom]
            [clojang.jinterface.erlang.boolean :as boolean]
            [clojang.jinterface.erlang.int :as int]
            [clojang.jinterface.erlang.string :as string]
            [clojang.jinterface.erlang.tuple :as tuple]
            [clojang.jinterface.erlang.types :as types]
            [clojang.jinterface.otp.nodes]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte int]))

(declare edn->term)

(defn vector->ji-tuple
  "Convert a Clojure vector into an Erlang JInterface tuple."
  [v]
  (types/tuple (into-array (types/object) (map #(edn->term %) v))))

(defn list->ji-list
  "Convert a Clojure list into an Erlang JInterface list."
  ([]
    (types/list))
  ([v]
    (types/list (into-array (types/object) (map #(edn->term %) v)))))

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
  ;; XXX map
  ;; long
  java.lang.Long
  (edn->term [edn]
    (types/long edn))
  clojure.lang.BigInt
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
  ;; string
  java.lang.String
  (edn->term [edn]
    (types/string edn))
  ;; XXX JInterface objects
  com.ericsson.otp.erlang.OtpErlangObject
  (edn->term [obj]
    obj))

(defprotocol TermConverter
  "Convert JInterface Erlang terms."
  (term->edn [this]
    "Convert JInterface Erlang types to EDN."))

(extend-protocol TermConverter
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
    (boolean/get-value erl-obj))
  ;; tuple /vector
  com.ericsson.otp.erlang.OtpErlangTuple
  (term->edn [erl-obj]
    (into [] (map #'term->edn (tuple/get-elements erl-obj))))
  ;; list
  com.ericsson.otp.erlang.OtpErlangList
  (term->edn [erl-obj]
    (map #'term->edn erl-obj))
  ;; XXX char
  com.ericsson.otp.erlang.OtpErlangChar
  (term->edn [erl-obj]
    ())
  ;; XXX map
  com.ericsson.otp.erlang.OtpErlangPid
  (term->edn [erl-obj]
    erl-obj)
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
  ;; XXX Clojure/Java objects ...
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
