# Apps Tutorial

There are several ways in which applications can be created with Clojang:

* A stand-alone Clojure application that needs to communicate with Erlang/OTP
  applications.
* A Clojure application that is managed by an Erlang/OTP application, with the
  Clojure application being a JVM process child in the Erlang/OTP application's
  supervision tree.
* A Clojure application that manages child processes in its own supervision
  tree.

This document covers examples of all three.


## Stand-alone Clojure Application

One of the best ways to create Clojure applications is by setting up components
using the [Component](https://github.com/stuartsierra/component) library and
bringing these together in a system that can be started, stopped, and
restarted with a simple API. Not only are you able to build and manage large
systems in Clojure with this approach, but you also get a nice, clean way of
managing the components as dependencies and making parts of components
available to other components via the running system.

The Clojang project offers a
[pre-built, reusable component](https://github.com/clojang/component) that can
be put to use as-is, offering up its own system, or it can be included as a
component in a larger system. It takes advantage of another project
([system-manager](https://github.com/clojusc/system-manager)) which builds upon
the Component library to add state transitions for the system as a whole.

Both the development REPL and the production-ready application (run from
`main`) use the system manager and thus both have use to the system data
structure and related APIs. In the case of the REPL, there is some
initialization that takes place when the REPL loads, but the system itself does
not get started, instead letting the developer do that when they are ready.

In the case of the application running from `main`, after the basic
initialization is done, the system _is_ started automatically (as is
appropriate for a stand-alone application).

In both cases, the system provided by the
[Clojang component](https://github.com/clojang/component) has several
components, providing the following:
* basic configuration setup and access
* logging setup
* startup of the `epmd` daemon (if it's not already running)
* the creation of a default Erlang/OTP-in-JVM node and mbox (taking
  advantage of code in the Clojang agent project that is used by
  Clojang REPLs to do the same thing using a Java agent)

This last one is the most crucial (as well as most useful) feature of that
project and highlights the single-most important task a stand-alone OTP Clojure
application needs to peform: set up an OTP node that is registered with `epmd`
so that all OTP applications can see it.

That project's
[main function](https://github.com/clojang/component/blob/master/src/clojang/component/core.clj#L50)
shows the essence of this type of application:

```clj
(defn -main
  "For best results, run with: `lein trampoline run`
  To see the demo output, run: `lein trampoline run demo`"
  [& args]
  (init)
  (startup)
  (java/add-shutdown-handler #(do
                               (log/warn "Shutting down system ...")
                               (shutdown)))
  (case (keyword (first args))
    :demo (log/info "Demo of core function calls:"
                    (logger/pprint {:node-name (node-name)
                                    :node (node)
                                    :mbox-name (mbox-name)
                                    :mbox (mbox)}))
    :unexpected-arg)
	(java/join-current-thread))
```

The steps are:

1. Initialize the system manager.
2. Start the system manager (which will, in turn, start the system for you).
3. Add a handler for shutting down the system manager.
4. Join the current thread to daemonize the JVM process running the system.

This does, of course, imply that you have already set up your system
components. To get started, you can see what this application did here:
* https://github.com/clojang/component/tree/master/src/clojang/component/components
* https://github.com/clojang/component/blob/master/src/clojang/component/system.clj


## Clojure Application as OTP Child Process

TBD


## Clojure Application as OTP Supervision Parent

TBD
