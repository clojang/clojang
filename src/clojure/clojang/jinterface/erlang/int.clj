(ns clojang.jinterface.erlang.int
  (:require [potemkin :refer [import-vars]]
            [clojang.util :as util]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang
             OtpErlangByte
             OtpErlangChar
             OtpErlangInt
             OtpErlangLong
             OtpErlangShort
             OtpErlangUInt
             OtpErlangUShort])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangInt
  (get-bigint-value [this]
    "Get this number as a BigInteger.")
  (get-bit-length [this]
    "Returns the number of bits in the minimal two's-complement
    representation of this BigInteger, excluding a sign bit.")
  (get-byte-value [this]
    "Get this number as a byte.")
  (get-char-value [this]
    "Get this number as a char.")
  (get-int-value [this]
    "Get this number as an int.")
  (long? [this]
    "Determine if this value can be represented as a long without truncation.")
  (ulong? [this]
    "Determine if this value can be represented as an unsigned long without
    truncation, that is if the value is non-negative and its bit pattern
    completely fits in a long.")
  (get-long-value [this]
    "Get this number as a long, or rather truncate all but the least
    significant 64 bits from the 2's complement representation of this number
    and return them as a long.")
  (get-short-value [this]
    "Get this number as a short.")
  (get-signum [this]
    "Return the signum function of this object.")
  (get-uint-value [this]
    "Get this number as a non-negative int.")
  (get-ushort-value [this]
    "Get this number as a non-negative short."))

(def behaviour
  {:get-bigint-value (fn [this] (.bigIntegerValue this))
   :get-bit-length (fn [this] (.bitLength this))
   :get-byte-value (fn [this] (.byteValue this))
   :get-char-value (fn [this] (.charValue this))
   :get-int-value (fn [this] (.intValue this))
   :long? (fn [this] (.isLong this))
   :ulong? (fn [this] (.isULong this))
   :get-long-value (fn [this] (.longValue this))
   :get-short-value (fn [this] (.shortValue this))
   :get-signum (fn [this] (.signum this))
   :get-uint-value (fn [this] (.uIntValue this))
   :get-ushort-value (fn [this] (.uShortValue this))})

(extend OtpErlangByte object/ErlangObject object/behaviour)
(extend OtpErlangByte ErlangInt behaviour)

(extend OtpErlangChar object/ErlangObject object/behaviour)
(extend OtpErlangChar ErlangInt behaviour)

(extend OtpErlangInt object/ErlangObject object/behaviour)
(extend OtpErlangInt ErlangInt behaviour)

(extend OtpErlangLong object/ErlangObject object/behaviour)
(extend OtpErlangLong ErlangInt behaviour)

(extend OtpErlangShort object/ErlangObject object/behaviour)
(extend OtpErlangShort ErlangInt behaviour)

(extend OtpErlangUInt object/ErlangObject object/behaviour)
(extend OtpErlangUInt ErlangInt behaviour)

(extend OtpErlangUShort object/ErlangObject object/behaviour)
(extend OtpErlangUShort ErlangInt behaviour)

;;; Aliases

(import-vars
  [object
   ;; object-behaviour
   bind
   clone
   decode
   encode
   equal?
   hash
   match
   ->str])
