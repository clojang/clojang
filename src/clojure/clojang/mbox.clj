(ns clojang.mbox
  (:require [clojang.agent.const :as const]
            [clojang.converter :as converter]
            [clojang.node :as node]
            [clojang.util :as util]
            [clojure.core.memoize :as memo]
            [potemkin :refer [import-vars]]
            [jiface.otp.messaging :as messaging]
            [jiface.otp.nodes :as nodes])
  (:import [com.ericsson.otp.erlang
            OtpErlangPid
            OtpMbox
            OtpMsg
            OtpNode])
  (:refer-clojure :exclude [new send]))

(declare get-name)

(defn register-name
  "An alias for `jiface.otp.messaging/register-name` that also
  allows for mailbox and node name arguments to be symbols, keywords, or
  strings."
  [& args]
  (apply #'messaging/register-name (util/->str-args args)))

(defn new
  "An alias for `jiface.otp.messaging/mbox`."
  ([node-instance]
    (messaging/mbox node-instance))
  ([node-instance mbox-name]
    (let [mbox (messaging/mbox node-instance)]
      (register-name mbox (util/->str-arg mbox-name))
      mbox)))

(defn get-default
  "Get the mbox for the default node.

  If an optional node instance is passed, a new mbox will be returned for that
  node.

  The results of this function are memoized as the intent is to obtain a
  singleton instance for the default node. (The Erlang JInterface docs
  recommend that only one node be run per JVM instance.)"
  []
  (messaging/default-mbox
    (nodes/default-node (node/get-default-name))
    const/default-mbox-name))

(defn self
  ""
  ([]
    (self (get-default)))
  ([mbox]
    (messaging/self mbox)))

(defmulti send
  "An alias for `jiface.otp.messaging/send`. This version of
  the function also:
  * allows for mailbox and node name arguments to be symbols, keywords, or
    strings;
  * allows for node and mbox objects to be passed (useful for dire's
    `with-finally!` function);
  * allows for 'client' data to be passed (a map of
    `{:self node :inbox mbox}` -- simplifies message-passing code)"
  (fn [& args] (mapv class args)))

(defmethod send [clojure.lang.PersistentVector]
                [msg]
  (send (get-default)
        const/default-mbox-name
        (node/get-default-name)
        msg))

(defmethod send [clojure.lang.Keyword
                 java.lang.String
                 clojure.lang.PersistentVector]
                [remote-mbox-name remote-node-name msg]
  (send (get-default)
        remote-mbox-name
        remote-node-name
        msg))

(defmethod send [OtpMbox
                 java.lang.Object
                 java.lang.Object
                 clojure.lang.PersistentVector]
                [remote-mbox remote-mbox-name remote-node-name msg]
  (messaging/send remote-mbox
                  (util/->str-arg remote-mbox-name)
                  (util/->str-arg remote-node-name)
                  (converter/clj->term msg)))

(defn get-names
  "An alias for `jiface.otp.messaging/get-names` that returns a
  vector of names on the same node as the given inbox"
  [inbox]
  (into [] (messaging/get-names inbox)))

(defn receive
  "An alias for `jiface.otp.messaging/receive` that returns the
  received data as Clojure data types."
  ([]
    (converter/term->clj (messaging/receive (get-default))))
  ([inbox]
    (converter/term->clj (messaging/receive inbox)))
  ([inbox timeout]
    (converter/term->clj (messaging/receive inbox timeout))))

(defn close
  ""
  ([]
    (close (get-default)))
  ([node]
    (messaging/close node)))

;;; Aliases

(import-vars
  [messaging

   ;; close
   equal?
   exit
   link
   get-name
   receive-buf
   receive-msg
   ;;self
   ping
   get-pid
   unlink])
