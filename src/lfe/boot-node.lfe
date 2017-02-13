(defmodule boot-node
  (behaviour jvm-node)
  (export all))

(defun noop ()
  'noop)
