(ns clojang.jinterface.erlang.list
  (:require [clojang.util :as util]
            [clojang.jinterface.erlang.tuple :refer [tuple-behaviour]])
  (:import [com.ericsson.otp.erlang OtpErlangList])
  (:refer-clojure :exclude [hash]))

(def list-behaviour
  (merge tuple-behaviour
         {:length (fn [this] (.arity this))
          :get-head (fn [this] (.getHead this))
          :get-last-tail (fn [this] (.getLastTail this))
          :get-nth-tail (fn [this index] (.getNthTail this index))
          :get-tail (fn [this] (.getTail this))
          :proper? (fn [this index] (.isProper this))
          :get-string-value (fn [this] (.stringValue this))}))

(defprotocol ErlangList
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
  (length [this]
    "Alias for ``get-arity``")
  (get-element [this index]
    "Get the specified element from the tuple.")
  (get-elements [this]
    "Get all the elements from the tuple as an array.")
  (get-head [this]
    "")
  (get-last-tail [this]
    "")
  (get-nth-tail [this index]
    "")
  (get-tail [this]
    "")
  (proper? [this]
    "")
  (get-string-value [this]
    "Convert a list of integers into a Unicode string, interpreting each
    integer as a Unicode code point value."))

(extend OtpErlangList ErlangList list-behaviour)
