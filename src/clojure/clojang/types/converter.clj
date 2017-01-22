(ns clojang.types.converter
  (:require [clojang.types.record]
            [clojure.string :as clj-string]
            [clojure.tools.logging :as log]
            [dire.core :refer [with-handler!]]
            [jiface.erlang.atom :as atom]
            [jiface.erlang.float :as float]
            [jiface.erlang.int :as int]
            [jiface.erlang.map :as map-type]
            [jiface.erlang.pid :as pid]
            [jiface.erlang.port :as port]
            [jiface.erlang.ref :as ref]
            [jiface.erlang.string :as string]
            [jiface.erlang.tuple :as tuple]
            [jiface.erlang.types :as types]
            [jiface.otp.nodes]
            [jiface.otp.messaging :as messaging])
  (:import [clojang.types.record]
           [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean byte float int]))

(declare clj->erl
         erl->clj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Helper Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clj-seq->erl-array [xs]
  "Convert a Clojure seq into a Java array of Erlang JInterface obects."
  (into-array (types/object) (map clj->erl xs)))

(defn erl-tuple->clj-vector [xs]
  (map #(erl->clj %) (into [] xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Clojure to Erlang conversions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti clj->erl
  "Convert Clojure types to Erlang/JInterface types."
  class)

(defmethod clj->erl nil
  [_]
  (types/atom "undefined"))

(defmethod clj->erl clojure.lang.Keyword
  [^clojure.lang.Keyword clj-obj]
  (types/atom (name clj-obj)))

(defmethod clj->erl clojure.lang.Symbol
  [^clojure.lang.Symbol clj-obj]
  (types/atom (str clj-obj)))

(defmethod clj->erl java.lang.Boolean
  [^java.lang.Boolean clj-obj]
  (types/boolean clj-obj))

(defmethod clj->erl clojure.lang.PersistentVector
  [^clojure.lang.PersistentVector clj-obj]
  (types/tuple (clj-seq->erl-array clj-obj)))

(defmethod clj->erl clojure.lang.PersistentVector$ChunkedSeq
  [^clojure.lang.PersistentVector$ChunkedSeq clj-obj]
  (types/tuple (clj-seq->erl-array clj-obj)))

(defmethod clj->erl clojure.lang.PersistentList
  [^clojure.lang.PersistentList clj-obj]
  (types/list (clj-seq->erl-array clj-obj)))

(defmethod clj->erl clojure.lang.PersistentList$EmptyList
  [^clojure.lang.PersistentList$EmptyList clj-obj]
  (types/list))

(defmethod clj->erl java.lang.Character
  [^java.lang.Character clj-obj]
  (types/char clj-obj))

(defmethod clj->erl clojure.lang.PersistentArrayMap
  [^clojure.lang.PersistentArrayMap clj-obj]
  (case clj-obj
    {} (types/map)
    (types/map (clj-seq->erl-array (keys clj-obj))
               (clj-seq->erl-array (vals clj-obj)))))

(defmethod clj->erl java.lang.Long
  [^java.lang.Long clj-obj]
  (types/long clj-obj))

(defmethod clj->erl clojure.lang.BigInt
  [^clojure.lang.BigInt clj-obj]
  (types/long clj-obj))

(defmethod clj->erl java.math.BigInteger
  [^java.math.BigInteger clj-obj]
  (types/long clj-obj))

(defmethod clj->erl java.lang.Byte
  [^java.lang.Byte clj-obj]
  (types/byte clj-obj))

(defmethod clj->erl java.lang.Character
  [^java.lang.Character clj-obj]
  (types/char clj-obj))

(defmethod clj->erl java.lang.Integer
  [^java.lang.Integer clj-obj]
  (types/int clj-obj))

(defmethod clj->erl java.lang.Short
  [^java.lang.Short clj-obj]
  (types/short clj-obj))

; (defmethod clj->erl XXX
;   [clj-obj]
;   (types/uint clj-obj))

; (defmethod clj->erl XXX
;   [clj-obj]
;   (types/ushort clj-obj))

(defmethod clj->erl java.lang.Float
  [^java.lang.Float clj-obj]
  (types/float clj-obj))

(defmethod clj->erl java.lang.Double
  [^java.lang.Double clj-obj]
  (types/double clj-obj))

(defmethod clj->erl java.lang.String
  [^java.lang.String clj-obj]
  (types/string clj-obj))

(defmethod clj->erl clojang.types.record.Pid
  [^clojang.types.record.Pid clj-obj]
  (types/pid (:node clj-obj)
             (:id clj-obj)
             (:serial clj-obj)
             (:creation clj-obj)))

(defmethod clj->erl clojang.types.record.Port
  [^clojang.types.record.Port clj-obj]
  (types/port (:node clj-obj)
              (:id clj-obj)
              (:creation clj-obj)))

(defmethod clj->erl clojang.types.record.Ref
  [^clojang.types.record.Ref clj-obj]
  (types/ref (:node clj-obj)
              (:ids clj-obj)
              (:creation clj-obj)))

(defmethod clj->erl :default
  [clj-obj]
  clj-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Erlang to Clojure conversions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti erl->clj
  "Convert Erlang/JInterface types to Clojure types."
  class)

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangAtom
  [^com.ericsson.otp.erlang.OtpErlangAtom erl-obj]
  (let [trans (atom/->str erl-obj)]
    (if (= trans "undefined")
      nil
      (keyword (clj-string/replace trans "'" "")))))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangBoolean
  [^com.ericsson.otp.erlang.OtpErlangBoolean erl-obj]
  (atom/get-boolean-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangTuple
  [^com.ericsson.otp.erlang.OtpErlangTuple erl-obj]
  (into [] (erl-tuple->clj-vector (tuple/get-elements erl-obj))))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangList
  [^com.ericsson.otp.erlang.OtpErlangList erl-obj]
  (map #(erl->clj %) (into (list) erl-obj)))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangChar
  [^com.ericsson.otp.erlang.OtpErlangChar erl-obj]
  (int/get-char-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangMap
  [^com.ericsson.otp.erlang.OtpErlangMap erl-obj]
  ;; XXX Is it guaranteed that keys/values will be extracted in a correlated
  ;; order? If not, we'll need to go one-by-one ...
  (let [erl-map-keys (map-type/get-keys erl-obj)
        map-keys (erl-tuple->clj-vector erl-map-keys)
        map-vals (erl-tuple->clj-vector (map #(map-type/get erl-obj %)
                                        erl-map-keys))]
    (apply hash-map (interleave map-keys map-vals))))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangLong
  [^com.ericsson.otp.erlang.OtpErlangLong erl-obj]
  (int/get-long-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangByte
  [^com.ericsson.otp.erlang.OtpErlangByte erl-obj]
  (int/get-byte-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangChar
  [^com.ericsson.otp.erlang.OtpErlangChar erl-obj]
  (int/get-char-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangInt
  [^com.ericsson.otp.erlang.OtpErlangInt erl-obj]
  (int/get-int-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangShort
  [^com.ericsson.otp.erlang.OtpErlangShort erl-obj]
  (int/get-short-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangUInt
  [^com.ericsson.otp.erlang.OtpErlangUInt erl-obj]
  (int/get-uint-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangUShort
  [^com.ericsson.otp.erlang.OtpErlangUShort erl-obj]
  (int/get-ushort-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangFloat
  [^com.ericsson.otp.erlang.OtpErlangFloat erl-obj]
  (float/get-float-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangDouble
  [^com.ericsson.otp.erlang.OtpErlangDouble erl-obj]
  (float/get-double-value erl-obj))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangString
  [^com.ericsson.otp.erlang.OtpErlangString erl-obj]
  (clj-string/replace
    (string/->str erl-obj)
    #"\""
    ""))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangPid
  [^com.ericsson.otp.erlang.OtpErlangPid erl-obj]
  (clojang.types.record/map->Pid
    {:node (erl->clj (pid/get-node erl-obj))
     :id (erl->clj (pid/get-id erl-obj))
     :serial (erl->clj (pid/get-serial-num erl-obj))
     :creation (erl->clj (pid/get-creation-num erl-obj))}))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangPort
  [^com.ericsson.otp.erlang.OtpErlangPort erl-obj]
  (clojang.types.record/map->Port
    {:node (erl->clj (port/get-node erl-obj))
     :id (erl->clj (port/get-id erl-obj))
     :creation (erl->clj (port/get-creation-num erl-obj))}))

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangRef
  [^com.ericsson.otp.erlang.OtpErlangRef erl-obj]
  (clojang.types.record/map->Ref
    {:node (erl->clj (ref/get-node erl-obj))
     :ids (erl->clj (ref/get-ids erl-obj))
     :creation (erl->clj (ref/get-creation-num erl-obj))}))

(defmethod erl->clj com.ericsson.otp.erlang.OtpMsg
  [^com.ericsson.otp.erlang.OtpMsg erl-obj]

  (clojang.types.record/map->Msg
    {:msg (erl->clj (messaging/get-msg erl-obj))
     :recipient (erl->clj (messaging/get-recipient erl-obj))
     :recipient-name (erl->clj (messaging/get-recipient-name erl-obj))
     :recipient-pid (erl->clj (messaging/get-recipient-pid erl-obj))
     :sender-pid (erl->clj (messaging/get-sender-pid erl-obj))
     :msg-type (erl->clj (messaging/get-type erl-obj))}))

; (defmethod erl->clj com.ericsson.otp.erlang.OtpErlangRef
;   [^com.ericsson.otp.erlang.OtpErlangRef erl-obj]
;   [erl-obj]
;   erl-obj)

(defmethod erl->clj :default
  [erl-obj]
  erl-obj)
