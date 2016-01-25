(ns clojang.test.util-test
  (:require [clojure.test :refer :all]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang]))

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
  (is (= 'com.ericsson.otp.erlang.OtpErlangAtom
         (util/make-jinterface-name 'OtpErlang 'atom)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangBoolean
         (util/make-jinterface-name 'OtpErlang 'boolean)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangList
         (util/make-jinterface-name 'OtpErlang 'list)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangExternalFun
         (util/make-jinterface-name 'OtpErlang 'external-fun)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangList$SubList
         (util/make-jinterface-name 'OtpErlang 'list-sublist)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangObject$Hash
         (util/make-jinterface-name 'OtpErlang 'object-hash)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangUInt
         (util/make-jinterface-name 'OtpErlang 'uint)))
  (is (= 'com.ericsson.otp.erlang.OtpErlangUShort
         (util/make-jinterface-name 'OtpErlang 'ushort)))
  (is (= 'com.ericsson.otp.erlang.OtpLocalNode
         (util/make-jinterface-name 'Otp 'local-node))))

(deftest ^:unit dynamic-init
  (is (= "" (util/dynamic-init #'identity 'String)))
  (is (= "a string" (util/dynamic-init #'identity 'String "a string"))))
