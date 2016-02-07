(ns ^{:doc
  "A development namespace that imports other useful namespaces for easy
  prototyping, &c. The intended use is for this to be the initial namespace
  when running ``lein repl`` from the Clojang project directory."}
  clojang.dev
  (:require [clojure.core.match :refer [match]]
            [clojang.jinterface.erlang.types :as ji-types]
            [clojang.jinterface.otp.connection :as connection]
            [clojang.jinterface.otp.messaging :as messaging]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.conn :as conn]
            [clojang.core :as clojang]
            [clojang.mbox :as mbox]
            [clojang.msg :as msg]
            [clojang.node :as node]
            [clojang.rpc :as rpc]
            [clojang.types :as types]
            [clojang.util :as util]))
