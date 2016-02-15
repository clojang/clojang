(ns clojang.startup
  (:require [clojure.tools.logging :as log]
            [clojang.node :as node]
            [clojang.util :as util]
            [twig.core :as twig])
  (:import [java.lang.instrument])
  (:gen-class
    :methods [^:static [premain [String java.lang.instrument.Instrumentation] void]
              ^:static [agentmain [String java.lang.instrument.Instrumentation] void]]))

(defn perform-gui-tasks
  []
  (util/close-splash-screen))

(defn perform-node-tasks
  []
  (twig/set-level! 'clojang :info)
  (log/infof "Bringing up OTP node on %s ..." (node/get-default-name))
  (node/get-node))

(defn -premain
  [args instrument]
  (perform-node-tasks)
  (perform-gui-tasks))
