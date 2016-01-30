(ns clojang.test.jinterface.erlang.types-test
  (:require [clojure.math.numeric-tower :as math]
            [clojure.test :refer :all]
            [clojang.jinterface.erlang :as erlang]
            [clojang.jinterface.erlang.types :as types]
            [clojang.jinterface.erlang.atom :as atom-type]
            [clojang.jinterface.erlang.boolean :as boolean-type]
            [clojang.jinterface.erlang.char :as char-type]
            [clojang.jinterface.erlang.tuple :as tuple-type]
            [clojang.jinterface.erlang.list :as list-type]
            [clojang.jinterface.erlang.long :as long-type]
            [clojang.jinterface.erlang.string :as string-type])
  (:import [com.ericsson.otp.erlang]
           [java.lang Long]
           [java.math BigInteger]))

(deftest ^:unit make-erl-name-test
  (is (= 'com.ericsson.otp.erlang.OtpErlangAtom
         (erlang/make-erl-name 'atom))))

(deftest ^:unit init-test
  (is (= com.ericsson.otp.erlang.OtpErlangAtom
         (type (erlang/init 'atom "a")))))

(deftest ^:unit atom-constructor-test
  (is (= com.ericsson.otp.erlang.OtpErlangAtom
         (type (types/atom "a"))))
  (is (= com.ericsson.otp.erlang.OtpErlangAtom
         (type (types/atom true))))
  (is (= com.ericsson.otp.erlang.OtpErlangAtom
         (type (types/atom false)))))

(deftest ^:unit atom-protocol-test
  (let [an-atom (types/atom "test-atom")
        same-atom (types/atom "test-atom")
        another-atom (types/atom "another-atom")
        true-1 (types/atom true)
        true-2 (types/atom "true")
        false-1 (types/atom false)
        false-2 (types/atom "false")]
    (is (= "'test-atom'" (atom-type/->str an-atom)))
    (is (= true (atom-type/equal? an-atom an-atom)))
    ;; XXX next one isn't working right now - it can't find the static field
    ;; (is (= "" (types/get-max-length an-atom)))
    (is (= true (atom-type/equal? an-atom same-atom)))
    (is (= false (atom-type/equal? an-atom another-atom)))
    (is (= -1226849876 (atom-type/hash an-atom)))
    (is (= "test-atom" (atom-type/get-value an-atom)))
    (is (= "true" (atom-type/get-value true-1)))
    (is (= "true" (atom-type/get-value true-2)))
    (is (= true (atom-type/get-boolean-value true-1)))
    (is (= true (atom-type/get-boolean-value true-2)))
    (is (= "false" (atom-type/get-value false-1)))
    (is (= "false" (atom-type/get-value false-2)))
    (is (= false (atom-type/get-boolean-value false-1)))
    (is (= false (atom-type/get-boolean-value false-2)))))

(deftest ^:unit boolean-constructor-test
  (is (= com.ericsson.otp.erlang.OtpErlangBoolean
         (type (types/boolean true))))
  (is (= com.ericsson.otp.erlang.OtpErlangBoolean
         (type (types/boolean false)))))

(deftest ^:unit boolean-protocol-test
  (let [true-bool (types/boolean true)
        false-bool (types/boolean false)]
    (is (= "true" (boolean-type/get-atom-value true-bool)))
    (is (= "false" (boolean-type/get-atom-value false-bool)))
    (is (= true (boolean-type/get-value true-bool)))
    (is (= false (boolean-type/get-value false-bool)))))

(deftest ^:unit char-constructor-test
  (is (= com.ericsson.otp.erlang.OtpErlangChar
         (type (types/char \A)))))

(deftest ^:unit char-protocol-test
  (let [a-char (types/char \A)
        same-char (types/char \A)
        another-char (types/char \B)]
    (is (= "65" (char-type/->str a-char)))
    (is (= "66" (char-type/->str another-char)))
    (is (= true (char-type/equal? a-char same-char)))
    (is (= false (char-type/equal? a-char another-char)))
    (is (= 65 (char-type/hash a-char)))
    (is (= 66 (char-type/hash another-char)))))

(deftest ^:unit tuple-protocol-test
  (let [tuple-1 (types/tuple (into-array [(types/atom "a")]))
        tuple-2 (types/tuple (into-array [(types/atom "a") (types/atom "b")]))
        same-tuple (types/tuple (into-array [(types/atom "a") (types/atom "b")]))]
    (is (= "{a}" (tuple-type/->str tuple-1)))
    (is (= "{a,b}" (tuple-type/->str tuple-2)))
    (is (= true (tuple-type/equal? tuple-2 same-tuple)))
    (is (= false (tuple-type/equal? tuple-1 same-tuple)))
    (is (= -1934275592 (tuple-type/hash tuple-1)))
    (is (= 943796724 (tuple-type/hash tuple-2)))
    (is (= 1 (tuple-type/get-arity tuple-1)))
    (is (= 2 (tuple-type/get-arity tuple-2)))
    (is (= 1 (tuple-type/get-size tuple-1)))
    (is (= 2 (tuple-type/get-size tuple-2)))
    (is (= "a" (atom-type/->str (tuple-type/get-element tuple-1 0))))
    (is (= "b" (atom-type/->str (tuple-type/get-element tuple-2 1))))
    (is (= 1 (count (tuple-type/get-elements tuple-1))))
    (is (= 2 (count (tuple-type/get-elements tuple-2))))))

(deftest ^:unit list-protocol-test
  (let [list-1 (types/list (into-array [(types/atom "a")]))
        list-2 (types/list (into-array [(types/atom "a") (types/atom "b")]))
        same-list (types/list (into-array [(types/atom "a") (types/atom "b")]))]
    (is (= "[a]" (list-type/->str list-1)))
    (is (= "[a,b]" (list-type/->str list-2)))
    (is (= true (list-type/equal? list-2 same-list)))
    (is (= false (list-type/equal? list-1 same-list)))
    (is (= -695209860 (list-type/hash list-1)))
    (is (= -1517993482 (list-type/hash list-2)))
    (is (= 1 (list-type/get-arity list-1)))
    (is (= 2 (list-type/get-arity list-2)))
    (is (= 1 (list-type/length list-1)))
    (is (= 2 (list-type/length list-2)))
    (is (= "a" (atom-type/->str (list-type/get-element list-1 0))))
    (is (= "b" (atom-type/->str (list-type/get-element list-2 1))))
    (is (= 1 (count (list-type/get-elements list-1))))
    (is (= 2 (count (list-type/get-elements list-2))))))

(deftest ^:unit long-protocol-test
  (let [long-1 (types/long (* -1 java.lang.Long/MAX_VALUE))
        long-2 (types/long java.lang.Long/MAX_VALUE)
        same-long (types/long java.lang.Long/MAX_VALUE)
        int-long (types/long (new Long java.lang.Integer/MAX_VALUE))
        short-long (types/long (new Long java.lang.Short/MAX_VALUE))]
    (is (= "-9223372036854775807" (long-type/->str long-1)))
    (is (= "9223372036854775807" (long-type/->str long-2)))
    (is (= true (long-type/equal? long-2 same-long)))
    (is (= false (long-type/equal? long-1 same-long)))
    (is (= -2147483616 (long-type/hash long-1)))
    (is (= 2147483616 (long-type/hash long-2)))
    (is (= 9223372036854775807 (long-type/get-bigint-value long-2)))
    (is (= 63 (long-type/get-bit-length long-2)))
    (is (= \a (long-type/get-char-value (types/long 97))))
    (is (= 2147483647 (long-type/get-int-value int-long)))
    (is (= true (long-type/long? long-2)))
    (is (= false (long-type/ulong? long-1)))
    (is (= true (long-type/ulong? long-2)))
    (is (= 9223372036854775807 (long-type/get-long-value long-2)))
    (is (= 32767 (long-type/get-short-value short-long)))
    (is (= 1 (long-type/get-signum long-2)))
    (is (= 2147483647 (long-type/get-uint-value int-long)))
    (is (= 32767 (long-type/get-ushort-value short-long)))))
