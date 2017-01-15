(ns clojang.mbox
  (:require [clojure.core.memoize :as memo]
            [potemkin :refer [import-vars]]
            [jiface.otp.messaging :as messaging]
            [clojang.core :as clojang]
            [clojang.node :as node]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            OtpErlangPid
            OtpMbox
            OtpMsg
            OtpNode])
  (:refer-clojure :exclude [new send]))

(declare get-name)

(defn register-name
  "An alias for ``jiface.otp.messaging/register-name`` that also
  allows for mailbox and node name arguments to be symbols, keywords, or
  strings."
  [& args]
  (apply #'messaging/register-name (util/->str-args args)))

(defn new
  "An alias for ``jiface.otp.messaging/mbox``."
  ([node-instance]
    (messaging/mbox node-instance))
  ([node-instance mbox-name]
    (let [mbox (messaging/mbox node-instance)]
      (register-name mbox (util/->str-arg mbox-name))
      mbox)))

(def get-default
  "Get the mbox for the default node.

  If an optional node instance is passed, a new mbox will be returned for that
  node.

  The results of this function are memoized as the intent is to obtain a
  singleton instance for the default node. (The Erlang JInterface docs
  recommend that only one node be run per JVM instance.)"
  (memo/lru
    (fn
      ([]
        (get-default (node/get-node)))
      ([node-obj]
        (clojang.mbox/new node-obj)))))

(defn self
  ""
  ([]
    (self (get-default)))
  ([mbox]
    (messaging/self mbox)))

(defmulti send
  "An alias for ``jiface.otp.messaging/send``. This version of
  the function also:
  * allows for mailbox and node name arguments to be symbols, keywords, or
    strings;
  * allows for node and mbox objects to be passed (useful for dire's
    ``with-finally!`` function);
  * allows for 'client' data to be passed (a map of
    ``{:self node :inbox mbox}`` -- simplifies message-passing code)"
  (fn [& args] (mapv class args)))

(defmethod send [clojure.lang.PersistentArrayMap java.lang.Object]
                [[process-name remote-node-name] msg]
  (send (get-default)
        process-name
        remote-node-name
        (clojang/->erl msg)))

(defmethod send [OtpMbox OtpErlangPid java.lang.Object]
                [client-mbox pid msg]
  (messaging/send client-mbox
                  pid
                  (clojang/->erl msg)))

(defmethod send [OtpMbox OtpNode OtpErlangPid java.lang.Object]
                [client-mbox _node pid msg]
  (messaging/send client-mbox
                  pid
                  (clojang/->erl msg)))

(defmethod send [clojure.lang.PersistentArrayMap OtpErlangPid java.lang.Object]
                [client pid msg]
  (messaging/send (:inbox client)
                  pid
                  (clojang/->erl msg)))

(defmethod send [OtpMbox java.lang.Object java.lang.Object]
                [client-mbox mbox-name msg]
  (messaging/send client-mbox
                  (name mbox-name)
                  (clojang/->erl msg)))

(defmethod send [OtpMbox OtpMbox OtpNode java.lang.Object]
                [client-mbox mbox-obj node-obj msg]
  (messaging/send client-mbox
                  (get-name mbox-obj)
                  (node/get-name node-obj)
                  (clojang/->erl msg)))

(defmethod send [OtpMbox
                 java.lang.Object
                 java.lang.Object
                 java.lang.Object]
                [client-mbox mbox-name node-name msg]
  (messaging/send client-mbox
                  (name mbox-name)
                  (name node-name)
                  (clojang/->erl msg)))

(defmethod send [clojure.lang.PersistentArrayMap
                 java.lang.Object
                 java.lang.Object
                 java.lang.Object]
                [client mbox-name node-name msg]
  (messaging/send (:inbox client)
                  (name mbox-name)
                  (name node-name)
                  (clojang/->erl msg)))

(defn get-names
  "An alias for ``jiface.otp.messaging/get-names`` that returns a
  vector of names on the same node as the given inbox"
  [inbox]
  (into [] (messaging/get-names inbox)))

(defn receive
  "An alias for ``jiface.otp.messaging/receive`` that returns the
  received data as Clojure data types."
  ([]
    (clojang/->clj (messaging/receive (get-default))))
  ([inbox]
    (clojang/->clj (messaging/receive inbox)))
  ([inbox timeout]
    (clojang/->clj (messaging/receive inbox timeout))))

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

(def ! "An alias for ``#'mbox/send``." #'send)
