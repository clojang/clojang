(ns ^{:doc
  "This is the home of the ping-pong server callbacks."}
  ping-pong.callbacks
  (:require [clojure.tools.logging :as log]
            [clojang.mbox :as mbox]
            [clojang.util :as util]
            [ping-pong.client :as client]))

(defn handle-ping
  [png-png-client caller state]
  (mbox/! png-png-client caller :pong)
  (inc state))

(defn handle-get-count
  [png-png-client caller state]
  (mbox/! png-png-client caller state)
  state)

(defn handle-stop
  [png-png-client caller state]
  (mbox/! png-png-client caller :stopping)
  (client/shutdown png-png-client)
  :server-stopped)

(defn handle-unknown-cmd
  [png-png-client caller cmd state]
  (mbox/! png-png-client
          caller
          [:error (str "Unknown command: " cmd)])
  state)

(defn handle-unknown
  [_client msg state]
  (log/error "Unknown message:" msg)
  state)
