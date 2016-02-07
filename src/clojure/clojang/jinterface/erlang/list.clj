(ns clojang.jinterface.erlang.list
  (:require [potemkin :refer [import-vars]]
            [clojang.util :as util]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang
            OtpErlangList
            OtpErlangString])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangList
  (get-arity [this]
    "Get the arity of the tuple.")
  (length [this]
    "Alias for ``get-arity`` borrowed from the Erlang world.")
  (get-length [this]
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

(def behaviour
  {:get-arity (fn [this] (.arity this))
   :get-element (fn [this index] (.elementAt this index))
   :get-elements (fn [this] (.elements this))
   :length (fn [this] (.arity this))
   :get-length (fn [this] (.arity this))
   :get-head (fn [this] (.getHead this))
   :get-last-tail (fn [this] (.getLastTail this))
   :get-nth-tail (fn [this index] (.getNthTail this index))
   :get-tail (fn [this] (.getTail this))
   :proper? (fn [this index] (.isProper this))
   :get-string-value (fn [this] (.stringValue this))})

(extend OtpErlangList object/ErlangObject object/behaviour)
(extend OtpErlangList ErlangList behaviour)

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
