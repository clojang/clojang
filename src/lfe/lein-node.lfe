(defmodule lein-node
  (behaviour jvm-node)
  (export all))

(defun start (filesystem-path)
  (let ((cmd "lein")
        (opts `(#(cd ,filesystem-path)
                #(args ("run"))
                exit_status)))
    (open_port `#(spawn_executable ,cmd ,opts))))

(defun start_link (filesystem-path)
  (let ((pid (start filesystem-path)))
    (link pid)))
