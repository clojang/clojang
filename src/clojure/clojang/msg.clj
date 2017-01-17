(ns clojang.msg
  (:require [potemkin :refer [import-vars]]
            [jiface.otp.messaging :as messaging]
            [clojang.core :as clojang]
            [clojang.node :as node]
            [clojang.util :as util]))

(defn get-msg
  "An alias for ``jiface.otp.messaging/get-msg`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clj (messaging/get-msg msg)))

(defn get-recipient
  "An alias for ``jiface.otp.messaging/get-recipient`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clj (messaging/get-recipient msg)))

(defn get-recipient-name
  "An alias for ``jiface.otp.messaging/get-recipient-name`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clj (messaging/get-recipient-name msg)))

(defn get-recipient-pid
  "An alias for ``jiface.otp.messaging/get-recipient-pid`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clj (messaging/get-recipient-pid msg)))

(defn get-sender-pid
  "An alias for ``jiface.otp.messaging/get-sender-pid`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clj (messaging/get-sender-pid msg)))

(defn get-type
  "An alias for ``jiface.otp.messaging/get-type`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (-> msg
      (messaging/get-type)
      (clojang/->clj)
      (messaging/msg-type-lookup)))
