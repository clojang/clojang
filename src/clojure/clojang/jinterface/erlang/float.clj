(ns clojang.jinterface.erlang.float
  (:require [clojang.util :as util]
            [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang
             OtpErlangDouble
             OtpErlangFloat])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangDouble
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
  (get-double-value [this]
    "Get the value, as a double.")
  (get-float-value [this]
    "Get the value, as a float."))

(def double-behaviour
  (merge object-behaviour
         {:get-double-value (fn [this] (.doubleValue this))
          :get-float-value (fn [this] (.floatValue this))}))

(extend OtpErlangDouble ErlangDouble double-behaviour)
(extend OtpErlangFloat ErlangDouble double-behaviour)
