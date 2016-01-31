(ns ^{:doc
  "A development namespace that imports other useful namespaces for easy
  prototyping, &c."}
  clojang.dev
  (:require [clojang.jinterface.erlang.types :as ji-types]
            [clojang.jinterface.otp.messaging :as messaging]
            [clojang.jinterface.otp.node :as node]
            [clojang.core :as clojang]
            [clojang.mbox :as mbox]
            [clojang.node :as node]
            [clojang.types :as types]
            [clojang.util :as util]))
