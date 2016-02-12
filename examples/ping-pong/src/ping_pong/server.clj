(ns ^{:doc
  "This is the ping-pong server implementation."}
  ping-pong.server
  (:require [clojure.core.match :refer [match]]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler! with-finally!]]
            [clojang.mbox :as mbox]
            [clojang.node :as node]
            [clojang.types :as types]
            [ping-pong.callbacks :as callbacks]
            [ping-pong.client :as client]))

(defn png-png
  [init-state]
  (let [int-client (client/factory :ping-server)]
    (loop [png-count init-state]
      (match [(mbox/receive (:inbox int-client))]
        ;; Handle defined command messages
        [[:ping caller]]
          (recur (callbacks/handle-ping int-client caller png-count))
        [[:get-count caller]]
          (recur (callbacks/handle-get-count int-client caller png-count))
        [[:stop caller]]
          (recur (callbacks/handle-stop int-client caller png-count))
        ;; Handle unknown messages
        [[cmd (caller :guard types/pid?)]]
          (recur (callbacks/handle-unknown-cmd int-client caller cmd png-count))
        [msg]
          (recur (callbacks/handle-unknown int-client msg png-count))))))

(defn start
  [& {:keys [init-state]
      :or {init-state 0}}]
  (png-png init-state))

(defn spawn
  [& {:keys [init-state]
      :or {init-state 0}}]
  {:spawned? true
   :future (future (start :init-state init-state))})

(defn stop
  [server]
  (client/snd :stop)
  (if (:spawned? server) @server))
