(ns clojang.jinterface.erlang.string
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang OtpErlangString])
  (:refer-clojure :exclude [hash new]))

(defprotocol ErlangString
  (valid-code-point? [this code-point]
    "Validate a code point according to Erlang definition; Unicode 3.0.")
  (new [this bytes]
    "Construct a String from a Latin-1 (ISO-8859-1) encoded byte array, if
    Latin-1 is available, otherwise use the default encoding.")
  (->code-points [this str]
    "Create Unicode code points from a String.")
  (get-value [this]
    "Get the actual string contained in this object."))

(def behaviour
  {:valid-code-point? (fn [this integer] (.isValidCodePoint this integer))
   :new (fn [this bytes] (.newString this bytes))
   :->code-points (fn [this string] (.stringToCodePoints this string))
   :get-value (fn [this] (.stringValue this))})

(extend OtpErlangString object/ErlangObject object/behaviour)
(extend OtpErlangString ErlangString behaviour)

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
