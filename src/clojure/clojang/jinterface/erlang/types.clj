(ns clojang.jinterface.erlang.types
  (:require [clojang.jinterface.erlang :as erlang]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte char int list float long map ref short]))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Data types constructors
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defn object
  "This is a psuedo-constructor: ``OtpErlangObject`` doesn't provide a
  constructor method, but the object itself is needed for creating tuples,
  so this function simply returns ``OtpErlangObject``."
  []
  com.ericsson.otp.erlang.OtpErlangObject)

(defn atom
  "Constructor for an Erlang atom data type."
  [arg]
  (erlang/init 'atom arg))

(defn boolean
  "Constructor for an Erlang boolean (atom) data type."
  [bool]
  (erlang/init 'boolean bool))

(defn tuple
  "Provides a Java representation of Erlang tuples. Tuples are created from
  one or more arbitrary Erlang terms.

  The arity of the tuple is the number of elements it contains. Elements are
  indexed from 0 to (arity-1) and can be retrieved individually by using the
  appropriate index."
  [args]
  (erlang/init 'tuple args))

(defn list
  "Provides a Java representation of Erlang lists. Lists are created from
  zero or more arbitrary Erlang terms.

  The arity of the list is the number of elements it contains."
  ([]
    (erlang/init 'list))
  ([args]
    (erlang/init 'list args)))

(defn string
  "Provides a Java representation of Erlang strings."
  [str]
  (erlang/init 'string str))

(defn map
  "Provides a Java representation of Erlang maps. Maps are created from one
  or more arbitrary Erlang terms.

  The arity of the map is the number of elements it contains. The keys and
  values can be retrieved as arrays and the value for a key can be
  queried."
  [ks vs]
  (erlang/init 'map ks vs))

(defn long
  "Provides a Java representation of Erlang integral types. Erlang does not
  distinguish between different integral types, however this class and its
  subclasses OtpErlangByte, OtpErlangChar, OtpErlangInt , and OtpErlangShort
  attempt to map the Erlang types onto the various Java integral types. Two
  additional classes, OtpErlangUInt and OtpErlangUShort are provided for
  Corba compatibility. See the documentation for IC for more information."
  [num]
  (erlang/init 'long num))

(defn byte
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'byte num))

(defn char
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'char num))

(defn int
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'int num))

(defn short
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'short num))

(defn uint
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'uint num))

(defn ushort
  "See the docstring for ``#'types/long``."
  [num]
  (erlang/init 'ushort num))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Error handling
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(util/add-err-handler #'erlang/init
  [java.lang.IllegalArgumentException,
   java.lang.InstantiationException]
  "[ERROR] could not instantiate object!")

