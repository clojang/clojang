(ns clojang.conn
  (:require [potemkin :refer [import-vars]]
            [clojang.core :as clojang]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.jinterface.otp.connection :as connection]
            [clojang.msg :as msg]
            [clojang.util :as util])
  (:refer-clojure :exclude [deliver new send]))

(defn exit
  "An alias for ``clojang.jinterface.otp.connection/exit`` that automatically
  converts the ``reason`` argument to an appropriate Erlang type."
  [dest-pid reason]
  (apply #'connection/exit dest-pid (clojang/->erlang reason)))

(defn receive
  "An alias for ``clojang.jinterface.otp.connection/receive`` that returns the
  received data as Clojure data types."
  ([connx]
    (clojang/->clojure (connection/receive connx)))
  ([connx timeout]
    (clojang/->clojure (connection/receive connx timeout))))

(defn receive-msg
  "An alias for ``clojang.jinterface.otp.connection/receive-msg`` that returns the
  received data as Clojure data types."
  ([connx]
    (msg/->map (connection/receive-msg connx)))
  ([connx timeout]
    (msg/->map (connection/receive-msg connx timeout))))

(defn receive-rpc
  "An alias for ``clojang.jinterface.otp.connection/receive-rpc`` that returns the
  received data as Clojure data types."
  [connx]
  (clojang/->clojure (connection/receive-rpc connx)))

(defn send
  "An alias for ``clojang.jinterface.otp.connection/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (connection/send connx (util/->str-arg dest) (clojang/->erlang msg)))

(defn send-rpc
  "An alias for ``clojang.jinterface.otp.connection/send-rpc`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  ([connx mod fun]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (clojang/->erlang '())))
  ([connx mod fun args]
    (connection/send-rpc
      connx
      (util/->str-arg mod)
      (util/->str-arg fun)
      (clojang/->erlang args))))

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
(def recv-msg "" receive-msg)
(def recv-buf "" receive-buf)
(def recv-rpc "" receive-rpc)
(def ! "" send)
(def !rpc "" send-rpc)
(def !buf "" send-buf)
(def snd "" send)
(def snd-rpc "" send-rpc)
(def snd-buf "" send-buf)
