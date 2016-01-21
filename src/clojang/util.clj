(ns clojang.util
  (:require [clojure.string :as string]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd []
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  'TBD)

(defn convert-class-name [name-symbol]
  "A helper function for use when creating Erlang class wrappers."
  (case name-symbol
    ;; Types
    "external-fun" "ExternalFun"
    "list-sublist" "List$SubList"
    "object-hash" "Object$Hash"
    "uint" "UInt"
    "ushort" "UShort"
    ;; OTP
    "local-node" "LocalNode"
    ;; Everything else
    (string/capitalize name-symbol)))

(defn make-jinterface-name [name-symbol prefix]
  "A helper function for use when defining constructor macros."
  (->> name-symbol
       (str)
       (convert-class-name)
       (str prefix)
       (symbol)))

(defn get-hostname []
  (-> (java.net.InetAddress/getLocalHost)
      (.getHostName)))
