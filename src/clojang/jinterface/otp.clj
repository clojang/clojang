(ns clojang.jinterface.otp
  (:require [clojang.util :as util]))

(defn make-otp-name [name-symbol]
  "Given a symbol representing an OTP object name, this function generates
  a JInterface classname as a symbol, resolvable to an imported class."
  (util/make-jinterface-name "Otp" name-symbol))

(defn init [& args]
  "Common function for node instantiation.

  Having a single function which is ultimately responsible for creating
  objects allows us to handle instantiation errors easily, adding one handler
  for ``#'init`` instead of a bunch of handlers, one for each type of node."
  (apply #'util/dynamic-init
         (cons #'make-otp-name args)))
