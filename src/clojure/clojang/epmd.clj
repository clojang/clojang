(ns clojang.epmd
  (:require [clojang.caller :refer [call call!]]
            [jiface.epmd :as epmd]
            [dire.core :refer [with-handler!]]
            [potemkin :refer [import-vars]]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  []
  :TBD)

(defn- -parse-name
  [result]
  (->> result
       (re-matches #"name (.*) at port (.*)")
       (#(vector (get % 1) (Integer/parseInt (get % 2))))))

(defn- -parse-names [results]
  (->> results
       (map -parse-name)
       vec))

(defn lookup-names
  ([]
    (->> (call epmd/lookup-names)
         (-parse-names)))
  ([inet-addr-str]
    (->> (java.net.InetAddress/getByName inet-addr-str)
         (call epmd/lookup-names)
         (-parse-names)))
  ([inet-addr-str transport]
    (->> (java.net.InetAddress/getByName inet-addr-str)
         (into [transport])
         (call epmd/lookup-names)
         (-parse-names))))

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
