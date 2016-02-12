(ns ^{:doc
  "This is the ping-pong server API namespace."}
  ping-pong.api
  (:require [ping-pong.client :as client]))

(defn ping
  []
  (client/snd :ping))

(defn get-count
  []
  (client/snd :get-count))

(defn stop
  []
  (client/snd :stop))
