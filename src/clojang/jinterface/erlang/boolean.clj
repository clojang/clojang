(ns clojang.jinterface.erlang.boolean
  (:require [clojang.jinterface.erlang.atom :refer [atom-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangBoolean])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangBoolean
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
  (get-atom-value [this]
    "Get the actual string contained in this object.")
  (get-value [this]
    "The boolean value of this atom."))

(def boolean-behaviour
  (merge atom-behaviour
         {:get-atom-value (fn [this] (.atomValue this))
          :get-value (fn [this] (.booleanValue this))}))

(extend OtpErlangBoolean ErlangBoolean boolean-behaviour)
