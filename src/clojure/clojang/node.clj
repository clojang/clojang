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

(defn get-short-name
  "Get the OTP-style short name for the default node of this JVM instance."
  []
  (System/getProperty "node.sname"))

(defn get-long-name
  "Get the OTP-style long name for the default node of this JVM instance."
  []
  (System/getProperty "node.name"))

(defn get-default-name
  "Get the node name for the default node of this JVM instance. First the
  node's short name will be checked, and it that's null, the long name will
  be used."
  []
  (if-let [name (get-short-name)]
    (format "%s@%s" name (util/get-hostname))
    (get-long-name)))

(defn get-name
  "Get the name of the given node. If no name is given, return the name of
  the default node for the currently running JVM."
  ([]
    (get-default-name))
  ([node]
    (nodes/get-name node)))

(def get-node
  "Get the default node object for the currently running instance of the JVM.
  In general, one should not need more than one node per JVM."
  (let [default-node (nodes/node (name (get-default-name)))]
    (fn [] default-node)))

(defn get-names
  "An alias for ``clojang.jinterface.otp.nodes/get-names`` that returns a list
  of a node's registered mailbox names as a list of strings."
  ([]
    (get-names (get-node)))
  ([node]
    (into [] (nodes/get-names node))))

(defn ping
  "An alias ``clojang.jinterface.otp.nodes/ping`` that also allows for a
  2-arity call (with the default timeout set to 1000)."
  ([other-node]
    (ping (get-node) other-node))
  ([this-node other-node]
    (ping this-node other-node 1000))
  ([this-node other-node timeout]
    (apply #'nodes/ping (util/->str-args [this-node other-node timeout]))))

(defn whereis
  "An alias for ``clojang.jinterface.otp.nodes/whereis`` that also allows for
  the mailbox name argument to be a symbol, keyword, or string."
  [& args]
  (apply #'nodes/whereis (util/->str-args args)))

;;; Aliases

(import-vars
  [nodes

   ;; abstract-node-behaviour
   get-alivename
   get-cookie
   create-transport
   create-server-transport
   get-hostname
   ;; get-name -- see above
   set-cookie
   ->str
   ;; local-node-behaviour
   create-pid
   create-port
   create-ref
   get-port
   ;; node-behaviour
   close
   close-mbox
   create-mbox
   ;; get-names -- see above
   ;; ping -- see above
   register-mbox
   register-status-handler
   set-flags
   ;; whereis -- see above
   ;; self-behaviour
   accept
   connect
   get-pid
   publish-port
   unpublish-port])
