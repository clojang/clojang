(ns clojang.jinterface.erlang.types
  (:require [clojang.util :as util])
  (:refer-clojure :exclude [atom boolean char list float long map ref short]))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Helper functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defn make-erl-name [name-symbol]
  "Given a symbol representing an Erlang type name, this function generates
  a JInterface classname as a symbol, resolvable to an imported class."
  (util/make-jinterface-name "OtpErlang" name-symbol))

(defn init [& args]
  "Common function for type instantiation.

  Having a single function which is ultimately responsible for creating
  objects allows us to handle instantiation errors easily, adding one handler
  for ``#'init`` instead of a bunch of handlers, one for each data type."
  (apply #'util/dynamic-init
         (cons #'make-erl-name args)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Data types constructors
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defn atom
  "Constructor for an Erlang atom data type."
  [arg]
  (init 'atom arg))

(defn boolean
  "Constructor for an Erlang boolean (atom) data type."
  [bool]
  (init 'boolean bool))

(defn char
  "Constructor for an Erlang char."
  [ch]
  (init 'char ch))

(defn tuple
  "Provides a Java representation of Erlang tuples. Tuples are created from
  one or more arbitrary Erlang terms.

  The arity of the tuple is the number of elements it contains. Elements are
  indexed from 0 to (arity-1) and can be retrieved individually by using the
  appropriate index."
  [args]
  (init 'tuple args))

(defn list
  "Provides a Java representation of Erlang lists. Lists are created from
  zero or more arbitrary Erlang terms.

  The arity of the list is the number of elements it contains."
  ([]
    (init 'list))
  ([args]
    (init 'list args)))

(defn string
  "Provides a Java representation of Erlang strings."
  [str]
  (init 'string str))

(defn map
  "Provides a Java representation of Erlang maps. Maps are created from one
  or more arbitrary Erlang terms.

  The arity of the map is the number of elements it contains. The keys and
  values can be retrieved as arrays and the value for a key can be
  queried."
  [ks vs]
  (init 'map ks vs))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Error handling
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(util/add-err-handler #'init
  [java.lang.IllegalArgumentException,
   java.lang.InstantiationException]
  "[ERROR] could not instantiate object!")

