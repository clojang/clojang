(ns clojang.jinterface.erlang.types
  (:require [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            OtpErlangAtom
            OtpErlangBinary
            OtpErlangBitstr
            OtpErlangBoolean
            OtpErlangByte
            OtpErlangChar
            OtpErlangDouble
            OtpErlangExternalFun
            OtpErlangFloat
            OtpErlangFun
            OtpErlangInt
            OtpErlangList
            OtpErlangList$SubList
            OtpErlangLong
            OtpErlangMap
            OtpErlangObject
            OtpErlangObject$Hash
            OtpErlangPid
            OtpErlangPort
            OtpErlangRef
            OtpErlangShort
            OtpErlangString
            OtpErlangTuple
            OtpErlangUInt
            OtpErlangUShort])
  (:refer-clojure :exclude [atom boolean char list float hash long map ref short]))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Macros and helper functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defn- make-erl-name [name-symbol]
  (util/make-jinterface-name name-symbol "OtpErlang"))

(defmacro deferltype [erl-type docstring args]
  "Create an Erlang data type."
  `(defn ~erl-type ~docstring [~@args]
     (new ~(make-erl-name erl-type) ~@args)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Data types constructors
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(deferltype atom "Constroctor for an Erlang atom data type." [arg])
(deferltype boolean "Constroctor for an Erlang boolean (atom) data type." [bool])
(deferltype char "Constroctor for an Erlang boolean (atom) data type." [ch])

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Data types protocols
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defprotocol ErlangObject
  (bind [this binds] "Make new Erlang term replacing variables with the respective
                      values from bindings argument(s).")
  (clone [this] "Clone the Erlang object.")
  (decode [this buff] "Read binary data in the Erlang external format, and produce
                       a corresponding Erlang data type object.")
  (encode [this buff] "Convert the object according to the rules of the Erlang
                       external format.")
  (equal? [this other-erl-obj] "Determine if two Erlang objects are equal.")
  (hash [this] "Get the object hash code.")
  (match [this term binds] "Perform match operation against given term.")
  (->str [this] "Convert to a string."))

(extend-type OtpErlangObject ErlangObject
  (bind [this binds]
    (.bind this binds))
  (clone [this]
    (.clone this))
  (decode [this buff]
    (.decode this buff))
  (encode [this buff]
    (.encode this buff))
  (equal? [this other-erl-obj]
    (.equals this other-erl-obj))
  (hash [this]
    (.hashCode this))
  (match [this term binds]
    (.match this term binds))
  (->str [this]
    (.toString this)))

(defprotocol ErlangAtom
  (get-atom-value [this] "Get the actual string contained in this object.")
  (get-value [this] "The boolean value of this atom.")
  ;; XXX not working right now
  ;;(get-max-length [this] "The maximun allowed length of an atom, in characters.")
  )

(extend-type OtpErlangAtom ErlangAtom
  (get-atom-value [this]
    (.atomValue this))
  (get-value [this]
    (.booleanValue this))
  ;; The maxAtomLength static field isn't getting found ... not sure what's up widdat
  ;;(get-max-length [this]
  ;;  (.-maxAtomLength this))
  )
