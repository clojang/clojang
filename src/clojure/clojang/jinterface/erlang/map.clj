(ns clojang.jinterface.erlang.map
  (:require [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangMap])
  (:refer-clojure :exclude [get hash keys remove]))

(defprotocol ErlangMap
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
  (get-arity [this]
    "Get the arity of the map.")
  (->set [this]
    "Make a ``Set`` view of the map key-value pairs.")
  (get [this key]
    "Get the specified value from the map.")
  (get-keys [this]
    "Get all the keys from the map as an array.")
  (put [this key value]
    "Put value corresponding to key into the map.")
  (remove [this key]
    "Removes mapping for the key if present.")
  (get-values [this]
    "Get all the values from the map as an array."))

(def map-behaviour
  (merge object-behaviour
         {:get-arity (fn [this] (.arity this))
          :->set (fn [this] (.entrySet this))
          :get (fn [this key] (.get this key))
          :get-keys (fn [this] (.keys this))
          :put (fn [this key value] (.put this key value))
          :remove (fn [this key] (.remove this key))
          :get-values (fn [this] (.values this))}))

(extend OtpErlangMap ErlangMap map-behaviour)
