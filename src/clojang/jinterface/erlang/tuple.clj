(ns clojang.jinterface.erlang.tuple
  (:require [clojang.util :as util]
            [clojang.jinterface.erlang.object :refer [object-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangTuple])
  (:refer-clojure :exclude [hash]))

(def tuple-behaviour
  (merge object-behaviour
         {:get-arity (fn [this] (.arity this))
          :get-size (fn [this] (.arity this))
          :get-element (fn [this index] (.elementAt this index))
          :get-elements (fn [this] (into-array (.elements this)))}))

(defprotocol ErlangTuple
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
    "Get the arity of the tuple.")
  (get-size [this]
    "Alias for ``get-arity``")
  (get-element [this index]
    "Get the specified element from the tuple.")
  (get-elements [this]
    "Get all the elements from the tuple as an array."))

(extend OtpErlangTuple ErlangTuple tuple-behaviour)
