(ns clojang.mbox
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.otp.messaging :as messaging]
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
  "An alias for ``clojang.jinterface.otp.messaging/register-name`` that also
  allows for mailbox and node name arguments to be symbols, keywords, or
  strings."
  [& args]
  (apply #'messaging/register-name (util/->str-args args)))

(defn new
  "An alias for ``clojang.jinterface.otp.messaging/mbox``."
  ([node-instance]
    (messaging/mbox node-instance))
  ([node-instance mbox-name]
    (let [mbox (messaging/mbox node-instance)]
      (register-name mbox (util/->str-arg mbox-name))
      mbox)))

(defmulti send
  "An alias for ``clojang.jinterface.otp.messaging/send``. This version of
  the function also:
  * allows for mailbox and node name arguments to be symbols, keywords, or
    strings;
  * allows for node and mbox objects to be passed (useful for dire's
    ``with-finally!`` function);
  * allows for 'client' data to be passed (a map of
    ``{:self node :inbox mbox}`` -- simplifies message-passing code)"
  (fn [& args] (mapv class args)))

(defmethod send [OtpMbox OtpErlangPid java.lang.Object]
                [client-mbox pid msg]
  (messaging/send client-mbox
                  pid
                  (clojang/->erlang msg)))

(defmethod send [OtpMbox OtpNode OtpErlangPid java.lang.Object]
                [client-mbox _node pid msg]
  (messaging/send client-mbox
                  pid
                  (clojang/->erlang msg)))

(defmethod send [clojure.lang.PersistentArrayMap OtpErlangPid java.lang.Object]
                [client pid msg]
  (messaging/send (:inbox client)
                  pid
                  (clojang/->erlang msg)))

(defmethod send [OtpMbox java.lang.Object java.lang.Object]
                [client-mbox mbox-name msg]
  (messaging/send client-mbox
                  (name mbox-name)
                  (clojang/->erlang msg)))

(defmethod send [OtpMbox OtpMbox OtpNode java.lang.Object]
                [client-mbox mbox-obj node-obj msg]
  (messaging/send client-mbox
                  (get-name mbox-obj)
                  (node/get-name node-obj)
                  (clojang/->erlang msg)))

(defmethod send [OtpMbox
                 java.lang.Object
                 java.lang.Object
                 java.lang.Object]
                [client-mbox mbox-name node-name msg]
  (messaging/send client-mbox
                  (name mbox-name)
                  (name node-name)
                  (clojang/->erlang msg)))

(defmethod send [clojure.lang.PersistentArrayMap
                 java.lang.Object
                 java.lang.Object
                 java.lang.Object]
                [client mbox-name node-name msg]
  (messaging/send (:inbox client)
                  (name mbox-name)
                  (name node-name)
                  (clojang/->erlang msg)))

(defn get-names
  "An alias for ``clojang.jinterface.otp.messaging/get-names`` that returns a
  vector of names on the same node as the given inbox"
  [inbox]
  (into [] (messaging/get-names inbox)))

(defn receive
  "An alias for ``clojang.jinterface.otp.messaging/receive`` that returns the
  received data as Clojure data types."
  ([inbox]
    (clojang/->clojure (messaging/receive inbox)))
  ([inbox timeout]
    (clojang/->clojure (messaging/receive inbox timeout))))

;;; Aliases

(import-vars
  [messaging

   close
   equal?
   exit
   link
   get-name
   receive-buf
   receive-msg
   self
   get-pid
   unlink])

(def ! "An alias for ``#'mbox/send``." #'send)
