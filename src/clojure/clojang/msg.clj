(ns clojang.msg
  (:require [potemkin :refer [import-vars]]
            [jiface.otp.messaging :as messaging]
            [clojang.core :as clojang]
            [clojang.node :as node]
            [clojang.util :as util]))

(defn get-msg
  "An alias for ``clojang.jinterface.otp.messaging/get-msg`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clojure (messaging/get-msg msg)))

(defn get-recipient
  "An alias for ``clojang.jinterface.otp.messaging/get-recipient`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clojure (messaging/get-recipient msg)))

(defn get-recipient-name
  "An alias for ``clojang.jinterface.otp.messaging/get-recipient-name`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clojure (messaging/get-recipient-name msg)))

(defn get-recipient-pid
  "An alias for ``clojang.jinterface.otp.messaging/get-recipient-pid`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clojure (messaging/get-recipient-pid msg)))

(defn get-sender-pid
  "An alias for ``clojang.jinterface.otp.messaging/get-sender-pid`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (clojang/->clojure (messaging/get-sender-pid msg)))

(defn get-type
  "An alias for ``clojang.jinterface.otp.messaging/get-type`` that returns a
  vector of names on the same node as the given inbox"
  [msg]
  (-> msg
      (messaging/get-type)
      (clojang/->clojure)
      (messaging/msg-type-lookup)))

(defn ->map
  "Convert the JInterface OtpMsg object into a Clojure map."
  [otp-msg]
  {:msg (get-msg otp-msg)
   :recipient (get-recipient otp-msg)
   :recipient-name (get-recipient-name otp-msg)
   :recipient-pid (get-recipient-pid otp-msg)
   :get-sender-pid (get-sender-pid otp-msg)
   :msg-type (get-type otp-msg)})
