(ns ^{:doc
  "This is the ping-pong client namespace."}
  ping-pong.client
  (:require [clojure.core.match :refer [match]]
            [clojure.tools.logging :as log]
            [clojang.mbox :as mbox]
            [clojang.node :as node]))

(def client-timeout 1000)

(defn gen-name [kwd]
  (gensym (str (name kwd) "-")))

(defn make-payload
  [node mbox]
  {:self node
   :inbox mbox})

(defn factory
  ([]
    (factory (gen-name :ping-client)))
  ([client-name]
    (let [self (node/new client-name)
          inbox (mbox/new self :pinger)]
      (make-payload self inbox))))

(defn shutdown
  [client]
  (log/debug "Shutting down client ...")
  (mbox/close (:inbox client))
  (node/close (:self client))
  [:ok :client-stopped])

(defn handle-resp
  [client msg]
  (shutdown client)
  msg)

(defn -snd
  [snd-msg]
  (let [client (factory)]
    ;;
    (mbox/! client
            :pinger
            :ping-server
            [snd-msg (mbox/get-pid (:inbox client))])
    (match [(mbox/receive (:inbox client) client-timeout)]
      [rcv-msg] (handle-resp client rcv-msg))))

(defn snd
  [snd-msg]
  (match [(-snd snd-msg)]
    [nil] [:error "Request timed out. Is the server running?"]
    [result] result))
