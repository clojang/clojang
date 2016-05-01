(ns ^{:doc
  "The ``clojang.rpc`` namespace has no analong in the JInterface package. This
  namepsace intends to provide a similar interface as that provided by the
  ``clojang.conn`` namespace: a set of functions for sending and receiving
  messages to and from remote processes. In this case, however, there is a
  very specfic client we expect to be receiving requests from and replying to:
  an RPC client, and one that expects this namespace to provide functions
  compatible with the RPC capabilities of an Erlang/OTP ``gen_server``."}
  clojang.rpc
  (:require [potemkin :refer [import-vars]]
            [jiface.otp.nodes :as nodes]
            [jiface.otp.connection :as connection]
            [clojang.core :as clojang]
            [clojang.conn :as conn]
            [clojang.msg :as msg]
            [clojang.node :as node]
            [clojang.util :as util])
  (:refer-clojure :exclude [deliver new send]))

(defn exit
  "An alias for ``jiface.otp.connection/exit`` that automatically
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
  data. If not, set the ``:rpc-data`` value to ``nil``."
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
  "An alias for ``jiface.otp.connection/receive-msg`` that returns the
  received data as Clojure data types."
  ([connx]
    (->map (connection/receive-msg connx)))
  ([connx timeout]
    (->map (connection/receive-msg connx timeout))))

(defn send
  "An alias for ``jiface.otp.connection/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (connection/send connx (util/->str-arg dest) (clojang/->erlang msg)))

(defn receive
  "This function is called by the RPC server in anticipation of a message from
  an Erlang RPC client. As such, it parses the ``OtpMsg`` for RPC content,
  identifies the function to call on behalf of the client, makes this call
  (using ``eval``), and then sends the result to the calling ``OtpPid`` (being
  that of the client).

  Note that in order for an Erlang RPC client to accept a message from an RPC
  server, the message sent by the server has to conform to a very specific
  format, a format that is not speficied anywhere except the OTP ``gen_server``
  code. As such, this Clojure function sends a reply to the client using this
  format. Since the format is only specificed in OTP source code, it may change
  in the future, in which case this function will need to be updated.

  The request sent by the RPC client boils down to the following chain of calls
  (some have been elided):
   * ``rpc:do_call/3``
   * ``gen_server:call/3``
   * ``gen:do_call/4``

  The response that the RPC client is expected is created/formated in the
  following functions, and these are what govern how this particular Clojure
  function sends its results back to the RPC client, especially the ``reply/2``
  function:
   * ``gen_server:handle_msg/*``
   * ``gen_server:reply/2``

  In particular, this sending code is what we used:
  ``To ! {Tag, Reply}`` (which, in LFE, would be ``(! pid `#(,tag ,reply))``)."
  [& args]
  (let [connx (first args)
        msg-data (apply receive-msg args)
        client-pid (get-in msg-data [:rpc-data :calling-pid])
        client-ref (get-in msg-data [:rpc-data :calling-ref])
        ;; XXX the next assignment actually needs to pass the args
        ;; to a function that will eval mod/func/args ... the following
        ;; is just a placeholder
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

