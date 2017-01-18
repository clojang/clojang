(ns clojang.rpc
  "The `clojang.rpc` namespace has no analong in the JInterface package. This
  namepsace intends to provide a similar interface as that provided by the
  `clojang.conn` namespace: a set of functions for sending and receiving
  messages to and from remote processes. In this case, however, there is a
  very specfic client we expect to be receiving requests from and replying to:
  an RPC client, and one that expects this namespace to provide functions
  compatible with the RPC capabilities of an Erlang/OTP `gen_server`."
  (:require [clojang.conn :as conn]
            [clojang.types.core :as types]
            [clojang.msg :as msg]
            [clojang.node :as node]
            [clojang.util :as util]
            [jiface.otp.connection :as connection]
            [jiface.otp.nodes :as nodes]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [cast deliver send]))

(defn open
  ([peer-node-name]
    (node/connect peer-node-name))
  ([self-node-name peer-node-name]
    (node/connect self-node-name peer-node-name)))

(defn close
  ([peer-node-name]
    (conn/close (node/connect peer-node-name)))
  ([self-node-name peer-node-name]
    (conn/close (node/connect self-node-name peer-node-name))))

(defn send
  [peer-node-name & args]
  (apply conn/send-rpc (into [(open peer-node-name)] args)))

(defn receive
  [peer-node-name & args]
  (apply conn/receive-rpc (into [(open peer-node-name)] args)))

(defn cast
  [peer-node-name & args]
  (apply send (into [peer-node-name] args))
  (receive peer-node-name)
  :ok)

(defn call
  [peer-node-name & args]
  (apply send (into [peer-node-name] args))
  (receive peer-node-name))

; (defn rpc? [msg-data]
;   (let [gen (first msg-data)]
;     (and (vector? msg-data)
;          (or (= :$gen_call gen)
;              (= :$gen_cast gen)))))

; (defn parse-rpc
;   "Parse the message data in an RPC message."
;   [[gen-call-type [calling-pid calling-ref]
;                   [call-type module function arguments group-leader]]]
;   {:gen-call-type gen-call-type
;    :calling-pid calling-pid
;    :calling-ref calling-ref
;    :call-type call-type
;    :mod module
;    :func function
;    :args arguments
;    :group-leader group-leader})

; (defn get-rpc-data
;   "Convert RPC message data to a Clojure map, if the message contains RPC
;   data. If not, set the `:rpc-data` value to `nil`."
;   [msg-data]
;   (let [is-rpc (rpc? msg-data)]
;     {:rpc? is-rpc
;      :rpc-data (if is-rpc (parse-rpc msg-data) nil)}))

; (defn msg->rpc-msg
;   "Convert the JInterface `OtpMsg` to a Clojure map, parsing any RPC data
;   that may be included in the message."
;   [otp-msg]
;   (let [msg (types/erl->clj otp-msg)
;         msg-data (:msg msg)]
;     (merge msg
;            (get-rpc-data msg-data))))

; (defn receive-msg
;   "An alias for `jiface.otp.connection/receive-msg` that returns the
;   received data as Clojure data types."
;   ([rpc-client]
;     (msg->rpc-msg (connection/receive-msg (:conn rpc-client))))
;   ([rpc-client timeout]
;     (msg->rpc-msg (connection/receive-msg (:conn rpc-client) timeout))))

; (defn send
;   "An alias for `jiface.otp.connection/send` that also allows for
;   mailbox and node name arguments to be symbols, keywords, or strings."
;   [rpc-client dest msg]
;   (connection/send (:conn rpc-client)
;                    (util/->str-arg dest)
;                    (types/clj->erl msg))
;   :ok)

; (defn receive
;   "This function is called by the RPC server in anticipation of a message from
;   an Erlang RPC client. As such, it parses the `OtpMsg` for RPC content,
;   identifies the function to call on behalf of the client, makes this call
;   (using `eval`), and then sends the result to the calling `OtpPid` (being
;   that of the client).

;   Note that in order for an Erlang RPC client to accept a message from an RPC
;   server, the message sent by the server has to conform to a very specific
;   format, a format that is not speficied anywhere except the OTP `gen_server`
;   code. This function sends a reply to the client using this undocumented
;   format. Since the format is only specificed in OTP source code, it may
;   change in the future, in which case this function will need to be updated.

;   The request sent by the RPC client boils down to the following chain of calls
;   (some have been elided):
;    * `rpc:do_call/3`
;    * `gen_server:call/3`
;    * `gen:do_call/4`

;   The response that the RPC client is expected is created/formated in the
;   following functions, and these are what govern how this particular Clojure
;   function sends its results back to the RPC client, especially the `reply/2`
;   function:
;    * `gen_server:handle_msg/*`
;    * `gen_server:reply/2`

;   In particular, this sending code is what we used:
;   `To ! {Tag, Reply}` (which, in LFE, would be `(! pid `#(,tag ,reply))`)."
;   [& args]
;   (let [connx (:conn (first args))
;         msg-data (apply receive-msg args)
;         client-pid (get-in msg-data [:rpc-data :calling-pid])
;         client-ref (get-in msg-data [:rpc-data :calling-ref])
;         ;; XXX the next assignment actually needs to pass the args
;         ;; to a function that will eval mod/func/args ... the following
;         ;; is just a placeholder
;         result (get-in msg-data [:rpc-data :args])
;         response (types/clj->erl [client-ref result])]
;     (connection/send connx client-pid response)
;     [:ok response]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ! #'send)

(import-vars
  [connection

   exit
   deliver
   link
   get-msg-count
   get-peer
   receive-buf
   get-self
   send-buf
   unlink])
