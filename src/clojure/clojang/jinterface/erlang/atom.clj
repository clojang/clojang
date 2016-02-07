(ns clojang.jinterface.erlang.atom
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.erlang.object :as object])
  (:import [com.ericsson.otp.erlang
            OtpErlangAtom
            OtpErlangBoolean])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangAtom
  (get-atom-value [this]
    "Get the actual string contained in this object.")
  (get-boolean-value [this]
    "The boolean value of this atom.")
    ;; XXX not working right now - it can't find the static field
    ;;(get-max-length [this] "The maximun allowed length of an atom, in characters.")
    )

(def behaviour
  {:get-atom-value (fn [this] (.atomValue this))
   ;; The maxAtomLength static field isn't getting found ... not sure what's up widdat
   ;;:get-max-length (fn [this]
   ;;      (.-maxAtomLength this))
   :get-boolean-value (fn [this] (.booleanValue this))})

(extend OtpErlangAtom object/ErlangObject object/behaviour)
(extend OtpErlangAtom ErlangAtom behaviour)

(extend OtpErlangBoolean object/ErlangObject object/behaviour)
(extend OtpErlangBoolean ErlangAtom behaviour)

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
