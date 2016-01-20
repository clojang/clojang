(ns clojang.jinterface.erlang.types-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.erlang.types :as types])
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

(deftest ^:unit atom-constructor-test
  (is (= OtpErlangAtom (type (types/atom true))))
  (is (= OtpErlangAtom (type (types/atom false))))
  (is (= OtpErlangAtom (type (types/atom "some-string")))))

(deftest ^:unit atom-object-protocol-test
  (let [an-atom (types/atom "test-atom")
        same-atom (types/atom "test-atom")
        another-atom (types/atom "another-atom")]
    (is (= "'test-atom'" (types/->str an-atom)))
    (is (= true (types/equal? an-atom an-atom)))
    (is (= true (types/equal? an-atom same-atom)))
    (is (= false (types/equal? an-atom another-atom)))
    (is (= -1226849876 (types/hash an-atom)))))

