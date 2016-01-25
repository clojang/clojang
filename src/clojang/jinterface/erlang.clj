(ns clojang.jinterface.erlang
  (:require [clojure.core.match]
            [clojang.util :as util]
            [dire.core :refer [with-handler!]])
  (:import [com.ericsson.otp.erlang]))

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
