(ns ^{:doc
  "A development namespace that imports other useful namespaces for easy
  prototyping, &c."}
  clojang.dev
  (:require [clojang.jinterface.otp.messaging :as messaging]
            [clojang.core :as clojang]
            [clojang.mbox :as mbox]
            [clojang.node :as node]
            [clojang.util :as util]))
