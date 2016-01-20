(ns clojang.jinterface.erlang.types
  (:require [clojure.string :as string])
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
  (:refer-clojure :exclude [atom list float hash long map ref short]))

(defn convert-type-name [name-symbol]
  (case name-symbol
    "external-fun" "ExternalFun"
    "list-sublist" "List$SubList"
    "object-hash" "Object$Hash"
    "uint" "UInt"
    "ushort" "UShort"
    (string/capitalize name-symbol)))

(defn make-type-name [name-symbol]
  (->> name-symbol
       (str)
       (convert-type-name)
       (str "OtpErlang")
       (symbol)))

(defmacro deferltype [erl-type args]
  "Create an Erlang data type."
  `(defn ~erl-type [~@args]
     (new ~(make-type-name erl-type) ~@args)))

(deferltype atom [arg])

(defprotocol ErlangObject
  (bind [this binds] "Make new Erlang term replacing variables with the respective
                      values from bindings argument(s).")
  (clone [this] "Clone the Erlang object")
  (decode [this buff] "Read binary data in the Erlang external format, and produce
                       a corresponding Erlang data type object.")
  (encode [this buff] "Convert the object according to the rules of the Erlang
                       external format.")
  (equal? [this other-erl-obj] "Determine if two Erlang objects are equal.")
  (hash [this] "Get the object hash code.")
  (match [this term binds] "Perform match operation against given term.")
  (->str [this] "Convert to a string."))

(extend-type OtpErlangObject
  ErlangObject
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
