(ns clojang.epmd
  (:require [clojang.epmd :as epmd]
            [dire.core :refer [with-handler!]]
            [potemkin :refer [import-vars]]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  []
  'TBD)

(defn- -parse-lookup-names [results]
  (into [] results))

(defn lookup-names
  ([]
    (-> (epmd/lookup-names)
        (-parse-lookup-names)))
  ([inet-addr-str]
    (-> (java.net.InetAddress/getByName inet-addr-str)
        (epmd/lookup-names)
        (-parse-lookup-names)))
  ([inet-addr-str transport]
    (-> (java.net.InetAddress/getByName inet-addr-str)
        (epmd/lookup-names transport)
        (-parse-lookup-names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [epmd
   ;; epmd
   ;; lookup-names -- see above
   lookup-port
   publish-port
   unpublish-port
   use-port])
