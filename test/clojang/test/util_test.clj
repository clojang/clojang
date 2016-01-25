(ns clojang.test.util-test
  (:require [clojure.test :refer :all]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            OtpErlangAtom
            OtpErlangBinary
            OtpErlangBitstr
            OtpErlangBoolean
            OtpErlangByte
            OtpErlangChar
            OtpErlangDouble
            OtpErlangExternalFun
            OtpErlangFloat
            OtpErlangFun
            OtpErlangInt
            OtpErlangList
            OtpErlangList$SubList
            OtpErlangLong
            OtpErlangMap
            OtpErlangObject
            OtpErlangObject$Hash
            OtpErlangPid
            OtpErlangPort
            OtpErlangRef
            OtpErlangShort
            OtpErlangString
            OtpErlangTuple
            OtpErlangUInt
            OtpErlangUShort]))

(deftest ^:unit convert-class-name-test
  (is (= "Atom" (util/convert-class-name 'atom)))
  (is (= "Boolean" (util/convert-class-name 'boolean)))
  (is (= "List" (util/convert-class-name 'list)))
  (is (= "ExternalFun" (util/convert-class-name 'external-fun)))
  (is (= "List$SubList" (util/convert-class-name 'list-sublist)))
  (is (= "Object$Hash" (util/convert-class-name 'object-hash)))
  (is (= "UInt" (util/convert-class-name 'uint)))
  (is (= "UShort" (util/convert-class-name 'ushort)))
  (is (= "LocalNode" (util/convert-class-name 'local-node))))

(deftest ^:unit convert-class-name-test
  (is (= 'OtpErlangAtom (util/make-jinterface-name 'OtpErlang 'atom)))
  (is (= 'OtpErlangBoolean (util/make-jinterface-name 'OtpErlang 'boolean)))
  (is (= 'OtpErlangList (util/make-jinterface-name 'OtpErlang 'list)))
  (is (= 'OtpErlangExternalFun (util/make-jinterface-name 'OtpErlang 'external-fun)))
  (is (= 'OtpErlangList$SubList (util/make-jinterface-name 'OtpErlang 'list-sublist)))
  (is (= 'OtpErlangObject$Hash (util/make-jinterface-name 'OtpErlang 'object-hash)))
  (is (= 'OtpErlangUInt (util/make-jinterface-name 'OtpErlang 'uint)))
  (is (= 'OtpErlangUShort (util/make-jinterface-name 'OtpErlang 'ushort)))
  (is (= 'OtpLocalNode (util/make-jinterface-name 'Otp 'local-node))))

(deftest ^:unit dynamic-init
  (is (= "" (util/dynamic-init #'identity 'String)))
  (is (= "a string" (util/dynamic-init #'identity 'String "a string"))))
