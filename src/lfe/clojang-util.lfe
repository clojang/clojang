(defmodule clojang-util
  (export all))

(include-lib "clj/include/compose.lfe")

(defun get-version ()
  (lutil:get-app-version 'clojang))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(clojang ,(get-version)))))
