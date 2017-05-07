(ns clojang.mbox
  (:require [clojang.agent.const :as const]
            [clojang.node :as node]
            [clojang.types.core :as types]
            [clojang.util :as util]
            [clojure.core.memoize :as memo]
            [potemkin :refer [import-vars]]
            [jiface.otp.messaging :as messaging]
            [jiface.otp.nodes :as nodes])
  (:import [clojang.types.records Pid]
           [com.ericsson.otp.erlang
            OtpErlangPid
            OtpMbox
            OtpMsg
            OtpNode])
  (:refer-clojure :exclude [add new send]))

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

(defn add
  "Add an mbox to an existing node."
  ([mbox-name]
    (add mbox-name (node/get-default-node)))
  ([mbox-name node-instance]
    (clojang.mbox/new node-instance mbox-name)))

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
    (types/erl->clj (messaging/self mbox))))

(defmulti send
  "An alias for `jiface.otp.messaging/send`. This version of
  the function also:
  * allows for mailbox and node name arguments to be symbols, keywords, or
    strings;
  * converts between Erlang and Clojure objects"
  (fn [& args] (mapv class args)))

(defmethod send [java.lang.Object]
                [msg]
  (send
    (get-default)
    const/default-mbox-name
    (node/get-default-name)
    msg))

(defmethod send [Pid java.lang.Object]
                [pid-record msg]
  (send
    (types/clj->erl pid-record)
    msg))

(defmethod send [OtpErlangPid java.lang.Object]
                [pid msg]
  (send
    (get-default)
    pid
    msg))

(defmethod send [OtpMbox
                 OtpErlangPid
                 java.lang.Object]
                [remote-mbox pid msg]
  (messaging/send
    remote-mbox
    pid
    (types/clj->erl msg))
  :ok)

(defmethod send [clojure.lang.Keyword
                 java.lang.String
                 java.lang.Object]
                [remote-mbox-name remote-node-name msg]
  (send
    (get-default)
    remote-mbox-name
    remote-node-name
    msg))

(defmethod send [OtpMbox
                 java.lang.Object
                 java.lang.Object
                 java.lang.Object]
                [remote-mbox remote-mbox-name remote-node-name msg]
  (messaging/send
    remote-mbox
    (util/->str-arg remote-mbox-name)
    (util/->str-arg remote-node-name)
    (types/clj->erl msg))
  :ok)

(defn get-names
  "An alias for `jiface.otp.messaging/get-names` that returns a
  vector of names on the same node as the given inbox"
  [inbox]
  (into [] (messaging/get-names inbox)))

(defn receive
  "An alias for `jiface.otp.messaging/receive` that returns the
  received data as Clojure data types."
  ([]
    (types/erl->clj (messaging/receive (get-default))))
  ([inbox]
    (types/erl->clj (messaging/receive inbox)))
  ([inbox timeout]
    (types/erl->clj (messaging/receive inbox timeout))))

(defn ping
  "An alias for `jiface.otp.messaging/ping` that returns the
  same values as Erlang (pong/pang)."
  ([node-name]
    (ping (get-default) node-name 1000))
  ([node-name timeout]
    (ping (get-default) node-name timeout))
  ([inbox node-name timeout]
    (if (apply #'messaging/ping (util/->str-args [inbox node-name timeout]))
      :pong
      :pang)))

(defn get-pid [inbox]
  (types/erl->clj (messaging/get-pid inbox)))

(defn link
  "Link to a remote mailbox or Erlang process."
  ([pid]
    (link (get-default) pid))
  ([inbox pid]
    (messaging/link inbox (types/clj->erl pid))
    :ok))

(defn unlink
  "Remove a link to a remote mailbox or Erlang process."
  ([pid]
    (unlink (get-default) pid))
  ([inbox pid]
    (messaging/unlink inbox (types/clj->erl pid))
    :ok))

(defn close
  "Close the default or named mailbox."
  ([]
    (close (get-default)))
  ([mbox]
    (messaging/close mbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [messaging

   ;; close -- see above
   equal?
   exit
   ;;link -- see above
   get-name
   receive-buf
   receive-msg
   ;; self -- see above
   ;; ping -- see above
   ;; get-pid -- see above
   ;; unlink -- see above
   ])
