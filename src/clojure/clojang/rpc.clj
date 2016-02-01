(ns clojang.rpc
  (:require [potemkin :refer [import-vars]]
            [clojang.core :as clojang]
            [clojang.conn :as conn]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.jinterface.otp.connection :as connection]
            [clojang.msg :as msg]
            [clojang.node :as node]
            [clojang.util :as util])
  (:refer-clojure :exclude [deliver new send]))

(defn exit
  "An alias for ``clojang.jinterface.otp.connection/exit`` that automatically
  converts the ``reason`` argument to an appropriate Erlang type."
  [dest-pid reason]
  (apply #'connection/exit dest-pid (clojang/->erlang reason)))

(defn rpc? [msg-data]
  (let [gen (first msg-data)]
    (and (vector? msg-data)
         (or (= :$gen_call gen)
             (= :$gen_cast gen)))))

(defn parse-rpc
  "Parse the message data in an RPC message."
  [[gen-call-type [calling-pid calling-ref]
                  [call-type module function arguments group-leader]]]
  {:gen-call-type gen-call-type
   :calling-pid calling-pid
   :calling-ref calling-ref
   :call-type call-type
   :mod module
   :func function
   :args arguments
   :group-leader group-leader})

(defn msg->rpc-data
  "Convert RPC message data to a Clojure map, if the message contains RPC
  data."
  [msg-data]
  (let [is-rpc (rpc? msg-data)]
    {:rpc? is-rpc
     :rpc-data (if is-rpc (parse-rpc msg-data) nil)}))

(defn ->map
  "Convert the JInterface ``OtpMsg`` to a Clojure map, parsing any RPC data
  that may be included in the message."
  [otp-msg]
  (let [converted-msg (msg/->map otp-msg)
        msg-data (:msg converted-msg)]
    (merge converted-msg
           (msg->rpc-data msg-data))))

(defn receive-msg
  "An alias for ``clojang.jinterface.otp.connection/receive-msg`` that returns the
  received data as Clojure data types."
  ([connx]
    (->map (connection/receive-msg connx)))
  ([connx timeout]
    (->map (connection/receive-msg connx timeout))))

(defn send
  "An alias for ``clojang.jinterface.otp.connection/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (connection/send connx (util/->str-arg dest) (clojang/->erlang msg)))

(defn receive
  ""
  [& args]
  (let [connx (first args)
        self (node/get-pid (conn/get-self connx))
        msg-data (apply receive-msg args)
        client-pid (get-in msg-data [:rpc-data :calling-pid])
        client-node (get-in msg-data [:rpc-data :calling-node])
        client-ref (get-in msg-data [:rpc-data :calling-ref])
        result (get-in msg-data [:rpc-data :args])
        response (clojang/->erlang [client-ref result])
        ]
    (connection/send connx client-pid response)
    [:ok response]))

;;; Aliases

(import-vars
  [connection

   deliver
   link
   get-msg-count
   get-peer
   receive-buf
   get-self
   send-buf
   unlink])

(def recv "" receive)
(def ! "" send)
(def snd "" send)

