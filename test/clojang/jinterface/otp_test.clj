(ns clojang.jinterface.otp-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.otp :as otp]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            AbstractNode
            OtpLocalNode
            OtpNode
            OtpMbox
            OtpPeer
            OtpSelf]))

(deftest ^:unit node-test
  (let [my-name "mynode"
        hostname (util/get-hostname)
        my-fullhost (str my-name "@" hostname)
        my-node (otp/node my-name)]
    (is (= OtpNode (type my-node)))
    (is (= my-fullhost (otp/->str my-node)))
    (is (= my-fullhost (otp/get-name my-node)))
    (is (= hostname (otp/get-hostname my-node)))
    (is (= my-name (otp/get-alivename my-node)))
    (is (= String (type (otp/get-cookie my-node))))
    (is (= Integer (type (otp/get-port my-node))))
    ;; protected
    ; (is (= "" (otp/get-epmd my-node)))
    ))

