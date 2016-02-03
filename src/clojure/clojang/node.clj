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

(defn self
  "An alias for for the constructor``clojang.jinterface.otp.nodes/self``
  but one that allows for symbols and keywords to be used as node names, a
  closer match for BEAM language nodes, which use atoms for their names."
  [& args]
  (apply #'nodes/self (util/->str-args args)))

(defn peer
  "An alias for the constructor ``clojang.jinterface.otp.nodes/peer`` but
  one that allows for symbols and keywords to be used as node names, a
  closer match for BEAM language nodes, which use atoms for their names."
  [& args]
  (apply #'nodes/peer (util/->str-args args)))

(defn ping
  "An alias ``clojang.jinterface.otp.nodes/ping`` that also allows for a
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

(defn whereis
  "An alias for ``clojang.jinterface.otp.nodes/whereis`` that also allows for
  the mailbox name argument to be a symbol, keyword, or string."
  [& args]
  (apply #'nodes/whereis (util/->str-args args)))

;;; Aliases

(import-vars
  [nodes

   get-cookie
   get-hostname
   get-name
   close
   register-status-handler

   accept
   connect
   get-pid
   publish-port
   unpublish-port])
