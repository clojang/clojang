(ns clojang.node
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.util :as util])
  (:refer-clojure :exclude [new]))

(defn new
  "An alias for ``clojang.jinterface.otp.nodes/node`` but one that allows for
  symbols and keywords to be used as node names, a closer match for BEAM
  language nodes, which use atoms for their names."
  [& args]
  (apply #'nodes/node (util/->str-args args)))

(defn ping
  "An alias for ``clojang.jinterface.otp.nodes/ping`` that also allows for a
  2-arity call (with the default timeout set to 1000)."
  ([this-node other-node]
    (ping this-node other-node 1000))
  ([this-node other-node timeout]
    (apply #'nodes/ping (util/->str-args [this-node other-node timeout]))))

(defn get-names
  "An alias for ``clojang.jinterface.otp.nodes/get-names`` that returns a list
  of a node's registered mailbox names as a list of strings."
  [node]
  (into [] (nodes/get-names node)))

;;; Aliases

(import-vars
  [nodes

   get-cookie
   get-hostname
   get-name
   close
   register-status-handler])
