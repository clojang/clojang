(ns clojang.jinterface.erlang.char
  (:require [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangChar])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangChar
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
    "Convert to a string."))

(extend OtpErlangChar ErlangChar object-behaviour)
