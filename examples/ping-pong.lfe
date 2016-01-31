(defmodule ping-pong
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; server API
    (ping 0)
    (get-ping-count 0)))

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;; gen_server implementation

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;; callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_call
  (('ping _caller state-data)
    `#(reply pong ,(+ 1 state-data)))
  (('ping-count _caller state-data)
    `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok ,state-data))
  ((_message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; our server API

(defun ping ()
  (gen_server:call (server-name) 'ping))

(defun get-ping-count ()
  (gen_server:call (server-name) 'ping-count))
