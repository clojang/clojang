(ns ^{:doc
  "This is the ping-pong core namespace."}
  ping-pong.core
  (:require [clojure.tools.logging :as log]
            [dire.core :refer [with-handler! with-finally!]]
            [clojang.mbox :as mbox]
            [clojang.node :as node]
            [ping-pong.api :as api]
            [ping-pong.callbacks :as callbacks]
            [ping-pong.client :as client]
            [ping-pong.server :as server])
  (:gen-class))

;;; Exception handling

(defn get-err
  [e args msg]
  (log/error msg e args)
  [:error msg {:error e :args args}])

(with-handler! #'node/new
  java.lang.Exception
  (fn [e & args] (get-err e args "Could not create node")))

(with-handler! #'mbox/new
  java.lang.Exception
  (fn [e & args] (get-err e args "Could not create mbox")))

(with-handler! #'mbox/receive
  java.lang.Exception
  (fn [e & args] (get-err e args "Mbox could not receive message")))

(with-handler! #'mbox/!
  java.lang.Exception
  (fn [e & args] (get-err e args "Could not send message to mbox")))

(with-handler! #'mbox/close
  java.lang.Exception
  (fn [e & args] (get-err e args "Could not close mbox")))

(with-handler! #'node/close
  java.lang.Exception
  (fn [e & args] (get-err e args "Could not close node")))

(defn -main [& args]
  (server/start))
