(ns clojang.jinterface.erlang.float
  (:require [potemkin :refer [import-vars]]
            [clojang.util :as util]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang
             OtpErlangDouble
             OtpErlangFloat])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangDouble
  (get-double-value [this]
    "Get the value, as a double.")
  (get-float-value [this]
    "Get the value, as a float."))

(def behaviour
  {:get-double-value (fn [this] (.doubleValue this))
   :get-float-value (fn [this] (.floatValue this))})

(extend OtpErlangDouble object/ErlangObject object/behaviour)
(extend OtpErlangDouble ErlangDouble behaviour)

(extend OtpErlangFloat object/ErlangObject object/behaviour)
(extend OtpErlangFloat ErlangDouble behaviour)

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
