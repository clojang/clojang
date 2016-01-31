# Talking to Servers

## Clojure Client with LFE Server

In the ``examples`` directory of the Clojang source code there is a module containing a variation on the classic "ping-pong" server. It's written in LFE, but it could have been done in any BEAM (Erlang VM) language. Here's the code:

```cl
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
```

We're going to compile and then run that code from the LFE REPL. Then, from a Clojure REPL, we'll talk to it.

Start the LFE REPL using the ``repl`` make target in the Clojang top-level directory:

```bash
$ make repl
LFE Shell V7.2 (abort with ^G)
(clojang-lfe@mndltl01)>
```

Next, compile the example:

```cl
(clojang-lfe@mndltl01)> (c "examples/ping-pong.lfe")
(#(module ping-pong))
```

Now we can start it:

```cl
(clojang-lfe@mndltl01)> (ping-pong:start)
#(ok <0.51.0>)
```

In a separate terminal window (in the Clojang top-level directory, just like the ``make repl`` command), start up the Clojure REPL:

```bash
$ lein repl
nREPL server started on port 58369 on host 127.0.0.1 - nrepl://127.0.0.1:58369
REPL-y 0.3.7, nREPL 0.2.10
Clojure 1.8.0
clojang.dev=>
```

Now we can create nodes for two ends of the connection, and then open a connection between the two:

```clojure
clojang.dev=> (def self (node/self :client))
#'clojang.dev/self
clojang.dev=> (def other (node/peer "clojang-lfe@mndltl01"))
#'clojang.dev/other
clojang.dev=> (def connx (node/connect self other))
#'clojang.dev/connx
```

With a connection established, we're able to execute Erlang module/function calls on the remote LFE/Erlang node:

```clojure
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/send-rpc connx :ping-pong :get-ping-count)
nil
clojang.dev=> (conn/receive-rpc connx)
3
```

## LFE Client with Clojure Server

TBD
