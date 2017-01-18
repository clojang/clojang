(ns clojang.epmd
  (:require [dire.core :refer [with-handler!]]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  []
  'TBD)
