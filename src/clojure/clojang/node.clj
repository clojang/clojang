(ns clojang.node
  (:require
    [clojang.util :as util]
    [clojure.core.memoize :as memo]
    [jiface.otp.nodes :as nodes]
    [jiface.util :as ji-util]
    [potemkin :refer [import-vars]]
    [trifl.net :as net])
  (:refer-clojure :exclude [new]))

(defn new
  "An alias for ``jiface.otp.nodes/node`` but one that allows for
  symbols and keywords to be used as node names, a closer match for BEAM
  language nodes, which use atoms for their names."
  [& args]
  (apply #'nodes/node (util/->str-args args)))

(defn self
  "An alias for for the constructor``jiface.otp.nodes/self``
  but one that allows for symbols and keywords to be used as node names, a
  closer match for BEAM language nodes, which use atoms for their names."
  [& args]
  (apply #'nodes/self (util/->str-args args)))

(defn peer
  "An alias for the constructor ``jiface.otp.nodes/peer`` but
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
  (if-let [short-name (get-short-name)]
    (format "%s@%s" short-name (net/get-local-hostname))
    (get-long-name)))

(defn get-name
  "Get the name of the given node. If no name is given, return the name of
  the default node for the currently running JVM."
  ([]
    (get-default-name))
  ([node]
    (nodes/get-name node)))

(defn get-default-node
  "Get the default node object for the currently running instance of the JVM.
  In general, one should not need more than one node per JVM."
  []
  (nodes/default-node (get-default-name)))

(defn get-names
  "An alias for ``jiface.otp.nodes/get-names`` that returns a list
  of a node's registered mailbox names as a list of strings."
  ([]
    (get-names (get-default-node)))
  ([node]
    (into [] (nodes/get-names node))))

(defn ping
  "An alias ``jiface.otp.nodes/ping`` that also allows for a
  2-arity call (with the default timeout set to 1000)."
  ([node-name]
    (ping node-name 1000))
  ([node-name timeout]
    (ping (get-default-node) node-name timeout))
  ([this-node node-name timeout]
    (if (apply #'nodes/ping (util/->str-args [this-node node-name timeout]))
      :pong
      :pang)))

(defn whereis
  "An alias for ``jiface.otp.nodes/whereis`` that also allows for
  the mailbox name argument to be a symbol, keyword, or string."
  [& args]
  (apply #'nodes/whereis (util/->str-args args)))

(def connect
  "An alias for the constructor ``jiface.otp.nodes/connect`` but one that
  caches connections based on source and destination node name and  allows for
  symbols and keywords to be used as node names, a closer match for BEAM
  language nodes, which use atoms for their names."
  (memo/lru
    (fn ([remote-node-name]
          (connect (get-default-name) remote-node-name))
        ([local-node-name remote-node-name]
          (let [self (self (util/->str-arg local-node-name))
                peer (peer (util/->str-arg remote-node-name))]
             (nodes/connect self peer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def get-node #'get-default-node)
(def get-default #'get-default-node)

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
   ;; connect -- see above
   get-pid
   publish-port
   unpublish-port])
