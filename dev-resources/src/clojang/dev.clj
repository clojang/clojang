(ns ^{:doc
  "A development namespace that imports other useful namespaces for easy
  prototyping, &c. The intended use is for this to be the initial namespace
  when running ``lein repl`` from the Clojang project directory."}
  clojang.dev
  (:require [clojure.core.match :refer [match]]
            [clojure.pprint :refer [print-table]]
            [clojure.reflect :refer [reflect]]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :as repl]
            [dire.core :refer [with-handler! with-finally!]]
            [jiface.erlang.types :as ji-types]
            [jiface.otp.connection :as connection]
            [jiface.otp.messaging :as messaging]
            [jiface.otp.nodes :as nodes]
            [jiface.util :as ji-util]
            [clojang.agent.startup :as startup]
            [clojang.conn :as conn]
            [clojang.core :as clojang]
            [clojang.mbox :as mbox]
            [clojang.msg :as msg]
            [clojang.node :as node]
            [clojang.rpc :as rpc]
            [clojang.types :as types]
            [clojang.util :as util]))

(defn show-methods
  ""
  [obj]
  (print-table
    (sort-by :name
(filter :exception-types (:members (reflect obj))))))

(def reload #'repl/refresh)

