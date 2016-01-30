(ns clojang.mbox
  (:require [potemkin :refer [import-vars]]
            [clojang.jinterface.otp.messaging :as messaging]
            [clojang.core :as clojang]
            [clojang.node :as node]
            [clojang.util :as util])
  (:refer-clojure :exclude [new send]))

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

(defn send
  "An alias for ``clojang.jinterface.otp.messaging/send`` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [& args]
  (apply
    #'messaging/send
    (conj (util/->str-args (butlast args))
          (clojang/->erlang (last args)))))

(defn get-names
  "An alias for ``clojang.jinterface.otp.messaging/get-names`` that retuns a
  vector of "
  [inbox]
  (into [] (messaging/get-names inbox)))

(defn receive
  "An alias for ``clojang.jinterface.otp.messaging/receive`` that retuns a
  vector of "
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

(def ! #'send)
