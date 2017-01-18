(ns clojang.types.predicate
  (:require [clojang.types.converter :as types])
  (:import [com.ericsson.otp.erlang
             OtpErlangAtom
             OtpErlangBoolean
             OtpErlangBinary
             OtpErlangBitstr
             OtpErlangByte
             OtpErlangChar
             OtpErlangDouble
             OtpErlangFloat
             OtpErlangFun
             OtpErlangInt
             OtpErlangList
             OtpErlangList$SubList
             OtpErlangLong
             OtpErlangMap
             OtpErlangObject
             OtpErlangPid
             OtpErlangPort
             OtpErlangRef
             OtpErlangShort
             OtpErlangString
             OtpErlangTuple
             OtpErlangUInt
             OtpErlangUShort])
  (:refer-clojure :exclude [char? float? list? map? string?]))

(defn atom?
  ""
  [arg]
  (instance? OtpErlangAtom arg))

(defn boolean?
  ""
  [arg]
  (instance? OtpErlangBoolean arg))

(defn binary?
  ""
  [arg]
  (instance? OtpErlangBinary arg))

(defn bitstr?
  ""
  [arg]
  (instance? OtpErlangBitstr arg))

(defn byte?
  ""
  [arg]
  (instance? OtpErlangByte arg))

(defn char?
  ""
  [arg]
  (instance? OtpErlangChar arg))

(defn double?
  ""
  [arg]
  (instance? OtpErlangDouble arg))

(defn float?
  ""
  [arg]
  (instance? OtpErlangFloat arg))

(defn fun?
  ""
  [arg]
  (instance? OtpErlangFun arg))

(defn int?
  ""
  [arg]
  (instance? OtpErlangInt arg))

(defn list?
  ""
  [arg]
  (instance? OtpErlangList arg))

(defn long?
  ""
  [arg]
  (instance? OtpErlangLong arg))

(defn map?
  ""
  [arg]
  (instance? OtpErlangMap arg))

(defn object?
  ""
  [arg]
  (instance? OtpErlangObject arg))

(defn pid?
  ""
  [arg]
  (instance? OtpErlangPid arg))

(defn port?
  ""
  [arg]
  (instance? OtpErlangPort arg))

(defn ref?
  ""
  [arg]
  (instance? OtpErlangRef arg))

(defn short?
  ""
  [arg]
  (instance? OtpErlangShort arg))

(defn string?
  ""
  [arg]
  (instance? OtpErlangString arg))

(defn sublist?
  ""
  [arg]
  (instance? OtpErlangList$SubList arg))

(defn tuple?
  ""
  [arg]
  (instance? OtpErlangTuple arg))

(defn undefined?
  ""
  [arg]
  (and (instance? OtpErlangAtom arg)
       (= (types/erl->clj arg) nil)))

(defn uint?
  ""
  [arg]
  (instance? OtpErlangUInt arg))

(defn ushort?
  ""
  [arg]
  (instance? OtpErlangUShort arg))
