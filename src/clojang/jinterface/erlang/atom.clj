(ns clojang.jinterface.erlang.atom
  (:require [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangAtom])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangAtom
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
  (get-value [this]
    "Get the actual string contained in this object.")
  (get-boolean-value [this]
    "The boolean value of this atom.")
    ;; XXX not working right now - it can't find the static field
    ;;(get-max-length [this] "The maximun allowed length of an atom, in characters.")
    )

(def atom-behaviour
  (merge object-behaviour
         {:get-value (fn [this] (.atomValue this))
          ;; The maxAtomLength static field isn't getting found ... not sure what's up widdat
          ;;:get-max-length (fn [this]
          ;;      (.-maxAtomLength this))
          :get-boolean-value (fn [this] (.booleanValue this))}))

(extend OtpErlangAtom ErlangAtom atom-behaviour)
