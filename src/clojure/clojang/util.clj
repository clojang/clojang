(ns clojang.util
  (:require [clojure.string :as string]
            [dire.core :refer [with-handler!]])
  (:import [clojure.lang Reflector]
           [com.ericsson.otp.erlang]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  []
  'TBD)

(defn ->str-arg [arg]
  (condp #(%1 %2) arg
    keyword? (name arg)
    symbol? (str arg)
    arg))

(defn ->str-args [args]
  (reduce
    (fn [acc x]
      (into acc [x]))
    []
    (map ->str-arg args)))

(defn defined?
  [item]
  (cond
    empty? false
    nil? false))
