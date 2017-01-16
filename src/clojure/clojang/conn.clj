(ns clojang.conn
  (:require [potemkin :refer [import-vars]]
            [jiface.otp.nodes :as nodes]
            [jiface.otp.connection :as connection]
            [clojang.converter :as converter]
            [clojang.msg :as msg]
            [clojang.util :as util])
  (:refer-clojure :exclude [deliver new send]))

(defn exit
  "An alias for ``jiface.otp.connection/exit`` that automatically
  converts the ``reason`` argument to an appropriate Erlang type."
  [dest-pid reason]
  (apply #'connection/exit dest-pid (converter/clj->term reason)))

(defn receive
  "An alias for ``jiface.otp.connection/receive`` that returns the
  received data as Clojure data types."
  ([connx]
    (converter/term->clj (connection/receive connx)))
  ([connx timeout]
    (converter/term->clj (connection/receive connx timeout))))

(defn receive-msg
  "An alias for ``jiface.otp.connection/receive-msg`` that returns the
  received data as Clojure data types."
  ([connx]
    (msg/->map (connection/receive-msg connx)))
  ([connx timeout]
    (msg/->map (connection/receive-msg connx timeout))))

(defn receive-rpc
  "An alias for ``jiface.otp.connection/receive-rpc`` that returns the
  received data as Clojure data types."
  [connx]
  (converter/term->clj (connection/receive-rpc connx)))

(defn send
  "An alias for ``jiface.otp.connection/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (connection/send connx (util/->str-arg dest) (converter/clj->term msg)))

(defn send-rpc
  "An alias for ``jiface.otp.connection/send-rpc`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  ([connx mod fun]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (converter/clj->term '())))
  ([connx mod fun args]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (converter/clj->term args))))

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

(def recv "" receive)
(def recv-msg "" receive-msg)
(def recv-buf "" receive-buf)
(def recv-rpc "" receive-rpc)
(def ! "" send)
(def !rpc "" send-rpc)
(def !buf "" send-buf)
(def snd "" send)
(def snd-rpc "" send-rpc)
(def snd-buf "" send-buf)
