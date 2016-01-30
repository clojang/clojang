(ns clojang.jinterface.erlang.long
  (:require [clojang.util :as util]
            [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangLong])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangLong
  (bind [this binds]
    "Make new Erlang term replacing variables with the respective values
    from bindings argument(s).")
  (clone [this]
    "Clone the Erlang object.")
  (decode [this buff]
    "Read binary data in the Erlang external format, and produce a
    corresponding Erlang data type object.")
  (encode [this buff]
    "Convert the object according to the rules of the Erlang external
    format.")
  (equal? [this other-erl-obj]
    "Determine if two Erlang objects are equal.")
  (hash [this]
    "Get the object hash code.")
  (match [this term binds]
    "Perform match operation against given term.")
  (->str [this]
    "Convert to a string.")
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

(def long-behaviour
  (merge object-behaviour
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
          :get-ushort-value (fn [this] (.uShortValue this))}))

(extend OtpErlangLong ErlangLong long-behaviour)
