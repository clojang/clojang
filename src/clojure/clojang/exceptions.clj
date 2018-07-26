(ns clojang.exceptions
  (:require
    [clojang.types.converter :as converter]
    [jiface.exceptions :as exceptions]
    [potemkin :refer [import-vars]]))

(defn get-pid
  "Get the pid of the process that sent this exit."
  [ex]
  (converter/erl->clj (exceptions/get-pid ex)))

(defn get-message
  "A convenience alias for the `getMessage` exception method."
  [ex]
  (.getMessage ex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [exceptions

   ;; get-pid -- see above
   get-reason])
