(ns clojang.types.converter
  (:require [clojang.types.records :as records]
            [clojure.string :as clj-string]
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
  (:import [clojang.types.records Pid
                                  Port
                                  Ref]
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

;;; Atoms

(defmethod clj->erl nil
  [_]
  (types/atom "undefined"))

(defmethod clj->erl clojure.lang.Keyword
  [^clojure.lang.Keyword clj-obj]
  (types/atom (name clj-obj)))

(defmethod clj->erl clojure.lang.Symbol
  [^clojure.lang.Symbol clj-obj]
  (types/atom (str clj-obj)))

;;; Booleans

(defmethod clj->erl java.lang.Boolean
  [^java.lang.Boolean clj-obj]
  (types/boolean clj-obj))

;;; Bytes

(defmethod clj->erl java.lang.Byte
  [^java.lang.Byte clj-obj]
  (types/byte clj-obj))

;;; Chars

(defmethod clj->erl java.lang.Character
  [^java.lang.Character clj-obj]
  (types/char clj-obj))

;;; Doubles

(defmethod clj->erl java.lang.Double
  [^java.lang.Double clj-obj]
  (types/double clj-obj))

;;; Floats

(defmethod clj->erl java.lang.Float
  [^java.lang.Float clj-obj]
  (types/float clj-obj))

;;; Ints

(defmethod clj->erl java.lang.Integer
  [^java.lang.Integer clj-obj]
  (types/int clj-obj))

;;; Lists

(defmethod clj->erl clojure.lang.PersistentList
  [^clojure.lang.PersistentList clj-obj]
  (types/list (clj-seq->erl-array clj-obj)))

(defmethod clj->erl clojure.lang.PersistentList$EmptyList
  [^clojure.lang.PersistentList$EmptyList clj-obj]
  (types/list))

;;; Longs

(defmethod clj->erl clojure.lang.BigInt
  [^clojure.lang.BigInt clj-obj]
  (types/long clj-obj))

(defmethod clj->erl java.lang.Long
  [^java.lang.Long clj-obj]
  (types/long clj-obj))

(defmethod clj->erl java.math.BigInteger
  [^java.math.BigInteger clj-obj]
  (types/long clj-obj))

;;; Maps

(defmethod clj->erl clojure.lang.PersistentArrayMap
  [^clojure.lang.PersistentArrayMap clj-obj]
  (case clj-obj
    {} (types/map)
    (types/map (clj-seq->erl-array (keys clj-obj))
               (clj-seq->erl-array (vals clj-obj)))))

;;; Pids

(defmethod clj->erl Pid
  [^Pid clj-obj]
  (types/pid (:node clj-obj)
             (:id clj-obj)
             (:serial clj-obj)
             (:creation clj-obj)))

;;; Ports

(defmethod clj->erl Port
  [^Port clj-obj]
  (types/port (:node clj-obj)
              (:id clj-obj)
              (:creation clj-obj)))

;;; Refs

(defmethod clj->erl Ref
  [^Ref clj-obj]
  (types/ref (:node clj-obj)
              (:ids clj-obj)
              (:creation clj-obj)))

;;; Shorts

(defmethod clj->erl java.lang.Short
  [^java.lang.Short clj-obj]
  (types/short clj-obj))

;;; Strings

(defmethod clj->erl java.lang.String
  [^java.lang.String clj-obj]
  (types/string clj-obj))

;;; Tuples

(defmethod clj->erl clojure.lang.PersistentVector
  [^clojure.lang.PersistentVector clj-obj]
  (types/tuple (clj-seq->erl-array clj-obj)))

(defmethod clj->erl clojure.lang.PersistentVector$ChunkedSeq
  [^clojure.lang.PersistentVector$ChunkedSeq clj-obj]
  (types/tuple (clj-seq->erl-array clj-obj)))

;;; Default

(defmethod clj->erl :default
  [clj-obj]
  clj-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Erlang to Clojure conversions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti erl->clj
  "Convert Erlang/JInterface types to Clojure types."
  class)

;;; Atoms

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangAtom
  [^com.ericsson.otp.erlang.OtpErlangAtom erl-obj]
  (let [trans (atom/->str erl-obj)]
    (if (= trans "undefined")
      nil
      (keyword (clj-string/replace trans "'" "")))))

;;; Booleans

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangBoolean
  [^com.ericsson.otp.erlang.OtpErlangBoolean erl-obj]
  (atom/get-boolean-value erl-obj))

;;; Bytes

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangByte
  [^com.ericsson.otp.erlang.OtpErlangByte erl-obj]
  (int/get-byte-value erl-obj))

;;; Chars

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangChar
  [^com.ericsson.otp.erlang.OtpErlangChar erl-obj]
  (int/get-char-value erl-obj))

;;; Doubles

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangDouble
  [^com.ericsson.otp.erlang.OtpErlangDouble erl-obj]
  (float/get-double-value erl-obj))

;;; Floats

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangFloat
  [^com.ericsson.otp.erlang.OtpErlangFloat erl-obj]
  (float/get-float-value erl-obj))

;;; Ints

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangInt
  [^com.ericsson.otp.erlang.OtpErlangInt erl-obj]
  (int/get-int-value erl-obj))

;;; Lists

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangList
  [^com.ericsson.otp.erlang.OtpErlangList erl-obj]
  (map #(erl->clj %) (into (list) erl-obj)))

;;; Longs

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangLong
  [^com.ericsson.otp.erlang.OtpErlangLong erl-obj]
  (int/get-long-value erl-obj))

;;; Maps

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangMap
  [^com.ericsson.otp.erlang.OtpErlangMap erl-obj]
  ;; XXX Is it guaranteed that keys/values will be extracted in a correlated
  ;; order? If not, we'll need to go one-by-one ...
  (let [erl-map-keys (map-type/get-keys erl-obj)
        map-keys (erl-tuple->clj-vector erl-map-keys)
        map-vals (erl-tuple->clj-vector (map #(map-type/get erl-obj %)
                                        erl-map-keys))]
    (apply hash-map (interleave map-keys map-vals))))

;;; Messages

(defmethod erl->clj com.ericsson.otp.erlang.OtpMsg
  [^com.ericsson.otp.erlang.OtpMsg erl-obj]
  (records/map->Msg
    {:msg (erl->clj (messaging/get-msg erl-obj))
     :recipient (erl->clj (messaging/get-recipient erl-obj))
     :recipient-name (erl->clj (messaging/get-recipient-name erl-obj))
     :recipient-pid (erl->clj (messaging/get-recipient-pid erl-obj))
     :sender-pid (erl->clj (messaging/get-sender-pid erl-obj))
     :msg-type (erl->clj (messaging/get-type erl-obj))}))

;;; Pids

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangPid
  [^com.ericsson.otp.erlang.OtpErlangPid erl-obj]
  (records/map->Pid
    {:node (erl->clj (pid/get-node erl-obj))
     :id (erl->clj (pid/get-id erl-obj))
     :serial (erl->clj (pid/get-serial-num erl-obj))
     :creation (erl->clj (pid/get-creation-num erl-obj))}))

;;; Ports

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangPort
  [^com.ericsson.otp.erlang.OtpErlangPort erl-obj]
  (records/map->Port
    {:node (erl->clj (port/get-node erl-obj))
     :id (erl->clj (port/get-id erl-obj))
     :creation (erl->clj (port/get-creation-num erl-obj))}))

;;; Refs

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangRef
  [^com.ericsson.otp.erlang.OtpErlangRef erl-obj]
  (records/map->Ref
    {:node (erl->clj (ref/get-node erl-obj))
     :ids (erl->clj (ref/get-ids erl-obj))
     :creation (erl->clj (ref/get-creation-num erl-obj))}))

;;; Shorts

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangShort
  [^com.ericsson.otp.erlang.OtpErlangShort erl-obj]
  (int/get-short-value erl-obj))

;;; Strings

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangString
  [^com.ericsson.otp.erlang.OtpErlangString erl-obj]
  (clj-string/replace
    (string/->str erl-obj)
    #"\""
    ""))

;;; Tuples

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangTuple
  [^com.ericsson.otp.erlang.OtpErlangTuple erl-obj]
  (into [] (erl-tuple->clj-vector (tuple/get-elements erl-obj))))

;;; UInts

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangUInt
  [^com.ericsson.otp.erlang.OtpErlangUInt erl-obj]
  (int/get-uint-value erl-obj))

;;; UShorts

(defmethod erl->clj com.ericsson.otp.erlang.OtpErlangUShort
  [^com.ericsson.otp.erlang.OtpErlangUShort erl-obj]
  (int/get-ushort-value erl-obj))

;;; Default

(defmethod erl->clj :default
  [erl-obj]
  erl-obj)
