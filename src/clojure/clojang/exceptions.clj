(ns clojang.exceptions
  (:require [clojang.types.core :as types]
            [jiface.exceptions :as exceptions]
            [potemkin :refer [import-vars]]))

(defn get-pid
  "Get the pid of the process that sent this exit."
  [ex]
  (types/erl->clj (exceptions/get-pid ex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [exceptions

   ;; get-pid -- see above
   get-reason])
