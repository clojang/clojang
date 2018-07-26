(ns clojang.conn
  (:require
    [clojang.caller :refer [call call!]]
    [clojang.msg :as msg]
    [clojang.types.core :as types]
    [clojang.util :as util]
    [jiface.otp.connection :as connection]
    [jiface.otp.nodes :as nodes]
    [jiface.otp.streams :as streams]
    [potemkin :refer [import-vars]])
  (:import
    (clojang.types.records Pid))
  (:refer-clojure :exclude [deliver new send]))

(defn close
  [conn]
  (call! connection/close [conn]))

(defn deliver
  [conn ex-or-msg]
  (call! connection/deliver [conn ex-or-msg]))

(defn exit
  "An alias for `jiface.otp.connection/exit` that automatically
  converts the `reason` argument to an appropriate Erlang type."
  [^Pid dest-pid ^String reason]
  (apply #'connection/exit dest-pid (types/clj->erl reason)))

(defn link
  [conn ^Pid pid]
  (->> (types/clj->erl pid)
       (conj [conn])
       (call! connection/link)))

(defn receive
  "An alias for `jiface.otp.connection/receive` that returns the
  received data as Clojure data types."
  ([connx]
    (->> [connx]
         (call connection/receive)
         (types/erl->clj)))
  ([connx ^Long timeout]
    (->> [connx timeout]
         (call connection/receive)
         (types/erl->clj))))

(defn receive-buf
  "An alias for `jiface.otp.connection/receive-buff` that returns the
  received data as Clojure data types."
  ([connx]
    (->> [connx]
         (call connection/receive-buf)
         (types/erl->clj)))
  ([connx ^Long timeout]
    (->> [connx timeout]
         (call connection/receive-buf)
         (types/erl->clj))))

(defn receive-msg
  "An alias for `jiface.otp.connection/receive-msg` that returns the
  received data as Clojure data types."
  ([connx]
    (->> [connx]
         (call connection/receive-msg)
         (types/erl->clj)))
  ([connx ^Long timeout]
    (->> [connx timeout]
         (call connection/receive-msg)
         (types/erl->clj))))

(defn receive-rpc
  "An alias for `jiface.otp.connection/receive-rpc` that returns the
  received data as Clojure data types."
  [connx]
  (->> [connx]
       (call connection/receive-rpc)
       (types/erl->clj)))

(defn run
  [conn]
  (call! connection/run [conn]))

(defn send
  "An alias for `jiface.otp.connection/send` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (->> (types/clj->erl msg)
       (vector connx (util/->str-arg dest))
       (call! connection/send)))

(defn send-buf
  "An alias for `jiface.otp.connection/send-buf` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  [connx dest msg]
  (->> (types/clj->erl msg)
       (streams/output-stream)
       (conj [connx (util/->str-arg dest)])
       (call! connection/send-buf)))

(defn send-rpc
  "An alias for `jiface.otp.connection/send-rpc` that also allows for
  mailbox and node name arguments to be symbols, keywords, or strings."
  ([connx mod fun]
    (send-rpc connx mod fun []))
  ([connx mod fun args]
    (->> args
         (into (list))
         (types/clj->erl)
         (conj [connx (util/->str-arg mod) (util/->str-arg fun)])
         (call! connection/send-rpc))))

(defn set-flags
  [conn ^Integer flags]
  (call! connection/set-flags [conn flags]))

(defn unlink
  [conn ^Pid pid]
  (->> (types/clj->erl pid)
       (conj [conn])
       (call! connection/unlink)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [connection
   ;; abstract-connection-behaviour
   ;; close -- see above
   ;; deliver see above
   get-flags
   get-trace-level
   connected?
   ;; run -- see above
   ;; set-flags -- see abolve
   set-trace-level
   ;; connection-behaviour
   ;; exit -- see above
   ;; link -- see above
   get-msg-count
   get-peer
   ;; receive -- see above
   ;; receive-buf -- see above
   ;; redeive-msg -- see above
   ;; receive-rpc -- see above
   get-self
   ;; send -- see above
   ;; send-buf -- see above
   ;; send-rpc -- see above
   ;; unlink -- see above
   ])
