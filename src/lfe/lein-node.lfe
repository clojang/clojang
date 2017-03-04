(defmodule lein-node
  (behaviour jvm-node)
  (export all))

(defun start ()
  (start "./"))

(defun start (filesystem-path)
  (start filesystem-path '()))

(defun start (filesystem-path args)
  (let ((cmd (os:find_executable "lein"))
        (opts `(#(cd ,filesystem-path)
                #(args ,(++ args '("run")))
                exit_status)))
    (open_port `#(spawn_executable ,cmd) opts))))

; (set pid
;   (start "/alt/home/oubiwann/lab/org-clojang/lfecljapp"
;          '("with-profile" "+app")))

(defun start_link ()
  (start_link "./"))

(defun start_link (filesystem-path)
  (start_link filesystem-path '()))

(defun start_link (filesystem-path args)
  (let ((pid (start filesystem-path)))
    (link pid)))
