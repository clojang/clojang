(ns clojang.caller
  (:require [clojang.exceptions :as exceptions]
            [clojure.core.match :refer [match]]))

(defn call
  "This function is intended to be used under the hoold by all Clojang
  functions that need to call `jiface` functions, catching all
  exceptions (should they arise), returning either the result of the
  `jiface` call or error data in the event of an underlying exception."
  ([func]
    (call func []))
  ([func args]
    (try
      (apply func args)
      (catch Exception ex
        [:error [(type ex) (exceptions/get-message ex) ex]]))))

(defn call!
  "This function is intended to be used under the hoold by all Clojang
  functions that need to call `jiface` functions but that don't return
  results. The intent is to normalize such calls (which in Erlang sometimes
  return `true` or `ok`) with a consistent side-effects result: `:ok`.
  Since this function calls to `call` above, the same applies for any
  underlying exceptions that arise: they are converted to data and
  returned as a data structure."
  ([func]
    (call! func []))
  ([func args]
    (let [possible-error (call func args)]
      (match possible-error
        [:error _] possible-error
        _ :ok))))
