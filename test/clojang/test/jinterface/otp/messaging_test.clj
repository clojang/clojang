(ns clojang.test.jinterface.otp.messaging-test
  (:require [clojure.core.typed :refer [check-ns]]
            [clojure.test :refer [deftest is]]
            clojang.jinterface.otp.messaging))

(deftest ^:type type-annotations-test
  (is (check-ns 'clojang.jinterface.otp.messaging)))
