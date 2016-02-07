(ns clojang.jinterface.erlang.map
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang OtpErlangMap])
  (:refer-clojure :exclude [get hash keys remove]))

(defprotocol ErlangMap
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

(def behaviour
  {:get-arity (fn [this] (.arity this))
   :->set (fn [this] (.entrySet this))
   :get (fn [this key] (.get this key))
   :get-keys (fn [this] (.keys this))
   :put (fn [this key value] (.put this key value))
   :remove (fn [this key] (.remove this key))
   :get-values (fn [this] (.values this))})

(extend OtpErlangMap object/ErlangObject object/behaviour)
(extend OtpErlangMap ErlangMap behaviour)

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
