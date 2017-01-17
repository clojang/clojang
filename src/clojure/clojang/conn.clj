(ns clojang.conn
  (:require [clojang.msg :as msg]
            [clojang.types.converter :as converter]
            [clojang.util :as util]
            [jiface.otp.connection :as connection]
            [jiface.otp.nodes :as nodes]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [deliver new send]))

(defn exit
  "An alias for ``jiface.otp.connection/exit`` that automatically
  converts the ``reason`` argument to an appropriate Erlang type."
  [dest-pid reason]
  (apply #'connection/exit dest-pid (converter/clj->erl reason)))

(defn receive
  "An alias for ``jiface.otp.connection/receive`` that returns the
  received data as Clojure data types."
  ([connx]
    (converter/erl->clj (connection/receive connx)))
  ([connx timeout]
    (converter/erl->clj (connection/receive connx timeout))))

(defn receive-msg
  "An alias for ``jiface.otp.connection/receive-msg`` that returns the
  received data as Clojure data types."
  ([connx]
    (converter/erl->clj (connection/receive-msg connx)))
  ([connx timeout]
    (converter/erl->clj (connection/receive-msg connx timeout))))

(defn receive-rpc
  "An alias for ``jiface.otp.connection/receive-rpc`` that returns the
  received data as Clojure data types."
  [connx]
  (converter/erl->clj (connection/receive-rpc connx)))

(defn send
  "An alias for ``jiface.otp.connection/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (connection/send connx (util/->str-arg dest) (converter/clj->erl msg))
  :ok)

(defn send-rpc
  "An alias for ``jiface.otp.connection/send-rpc`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  ([connx mod fun]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (converter/clj->erl '()))
    :ok)
  ([connx mod fun args]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (converter/clj->erl args))
    :ok))

(defn- -parse-lookup-names [results]
  (into [] results))

(defn lookup-names
  ([]
    (-> (connection/lookup-names)
        (-parse-lookup-names)))
  ([inet-addr-str]
    (-> (java.net.InetAddress/getByName inet-addr-str)
        (connection/lookup-names)
        (-parse-lookup-names)))
  ([inet-addr-str transport]
    (-> (java.net.InetAddress/getByName inet-addr-str)
        (connection/lookup-names transport)
        (-parse-lookup-names))))

;;; Aliases

(import-vars
  [connection
   ;; abstract-connection-behaviour
   close
   deliver
   get-flags
   get-trace-level
   connected?
   run
   set-flags
   set-trace-level
   ;; connection-behaviour
   ;; exit -- see above
   link
   get-msg-count
   get-peer
   ;; receive -- see above
   receive-buf
   ;; redeive-msg -- see above
   ;; receive-rpc -- see above
   get-self
   ;; send -- see above
   send-buf
   ;; send-rpc -- see above
   unlink
   ;; epmd
   ;; lookup-names -- see above
   lookup-port
   publish-port
   unpublish-port
   use-port])
