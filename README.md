# clojang

[![][clojang-logo]][clojang-logo-large]

[clojang-logo]: resources/images/clojang-logo-250x.png
[clojang-logo-large]: resources/images/clojang-logo-1000x.png

*Erlang's JInterface in Idiomatic Clojure*

![Clojars Project](http://clojars.org/clojang/latest-version.svg)


#### Table of Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Building](#building-)
* [Shells & REPLs](#shells--repls-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Low-level API](#low-level-api-)
  * [Mid-level API](#mid-level-api-)
  * [Running Tests](#running-tests-)
* [Erlang, Clojure, and JInterface](#erlang-clojure-and-jinterface-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

This project provides a solution to the [aesthetic problem of JInterface](https://github.com/oubiwann/clojang/wiki/Example:-JInterface-in-Clojure). While JInterface is an invaluable tool for projects that need to have JVM and Erlang VM languages communicating with each other, it is rather verbose and cumbersom to do so in Clojure. The syntatical burden is often enough to discourage experimentation and play -- essential ingrediates for innovation. The primary goal of Clojang is to make it not only easy to write for the Clojure/Erlang interface, but fun as well.

**Low-level API**

The first step towards that was to write a Clojure wrapper for JInterface -- a low-level one that is essentially identical to native JInterface. This will be useful for anyone from a functional programming background who wants low-level access to JInterface via idiomatic Clojure.

**Mid-level API**

The second step was to use that low-level API to create a "mid-level" API, one that automatically performned the necessary type conversions of function parameters and returned results, allowing one to write the sort of Clojure one would normally do, without having to cast to Erlang types as is necessary in the low-level Clojure API.

The mid-level Clojang API is intended for Clojure application developers who which to integrate with languages running on the Erlang VM without having to compromise on the Clojure side.

**Erlang VM <-> JVM Utilities**

Finally, for Erlang applications that wish to interact with the JVM, a bit more work is needed: the Erlang VM needs to start up a JVM process (ideally supervised). Clojang aims to provide a basic framework (or at the very least, a set of examples) for doing this. Although the Erlang dialect used by Clojang is LFE (Lisp Flavoured Erlang), this code is 100% Core Erlang compatible and may be used by Erlang proper, Elixir, LFE, Joxa, and any others which run compiled ``.beam`` files.


## Dependencies [&#x219F;](#table-of-contents)

* Java
* Erlang
* lein
* rebar3


## Building [&#x219F;](#table-of-contents)

``rebar3`` is used for the top-level builds of the project. It runs ``lein`` under the covers in order to build the Clojure code and create the Clojang``.jar`` file. As such, to build everything -- LFE, Erlang, and Clojure -- you need only do the following:

* ``rebar3 compile``

If you wish to build your own JInterface ``.jar`` file and not use the one we've uploaded to Clojars, you'll need to follow the instrucations given in the documentation here:

* [Building JInterface for Clojure](http://oubiwann.github.io/clojang/current/80-building-jinterface.html)


## Shells & REPLs [&#x219F;](#table-of-contents)

There are three interactive programming environments you may start, each of which will have full access to the project's libraries/dependencies.

LFE:

```bash
$ make repl
```

Erlang:

```bash
$ rebar3 shell
```

Clojure:

```bash
$ lein repl
```

## Documentation [&#x219F;](#table-of-contents)

Project documentation, including Clojang API reference docs, Javadocs for JInterface, and the Erlang JInterface User's Guide, is availble here:

* [http://oubiwann.github.io/clojang/current/](http://oubiwann.github.io/clojang/current/)

Quick links for the other docs:

* Clojang User Guides:
  * [Low-level API](http://oubiwann.github.io/clojang/current/10-low-level.html) - A translation of the *JInterface User's Guide* (Erlang documantaion) from Java into Clojure
  * [Mid-level API](http://oubiwann.github.io/clojang/current/20-mid-level.html) - An adaptation of the *Low-level API User's Guide* for even more idiomatic Clojure use
* [JInterface User's Guide](http://oubiwann.github.io/clojang/current/erlang/jinterface_users_guide.html) - The JInterface documentation provided in Erlang distributions
* [Jinterface Javadocs](http://oubiwann.github.io/clojang/current/erlang/java) - Javadoc-generated API documentation built from the JInterface source code


## Usage [&#x219F;](#table-of-contents)

Using Clojang in a project is just like any other Clojure library. Just add the following to the ``:dependencies`` in your ``project.clj`` file:

![Clojars Project](http://clojars.org/clojang/latest-version.svg)

For the Erlang/LFE side of things, you just need to add the Github URL to your ``rebar.config`` file, as with any other rebar-based Erlang VM project.

As for actual code usage, the documentation section provides links to developer guides and API references, but below are also provided two quick examples, one each in the low- and mid-level APIs.

### Low-level API [&#x219F;](#table-of-contents)

```clojure
(require '[clojang.jinterface.otp.messaging :as messaging]
         '[clojang.jinterface.otp.nodes :as nodes]
         '[clojang.jinterface.erlang.types :as types]
         '[clojang.jinterface.erlang.tuple :as tuple-type])
(def node (nodes/node "gurka"))
(def mbox (messaging/mbox node))
(messaging/register-name mbox "echo")
(def msg (into-array
           (types/object)
           [(messaging/self mbox)
            (types/atom "hello, world")]))
(messaging/! mbox "echo" "gurka" (types/tuple msg))
(messaging/receive mbox)
#object[com.ericsson.otp.erlang.OtpErlangTuple
        0x4c9e3fa6
        "{#Pid<gurka@mndltl01.1.0>,'hello, world'}"]
```

From LFE:

```cl
(lfe@mndltl01)> (! #(echo gurka@mndltl01) `#(,(self) hej!))
#(<0.35.0> hej!)
```

Then back in Clojure:

```clojure
(def data (messaging/receive mbox))
(def lfe-pid (tuple-type/get-element data 0))
(messaging/! mbox lfe-pid (types/tuple msg))
```

Then, back in LFE:

```cl
(lfe@mndltl01)> (c:flush)
Shell got {<5926.1.0>,'hello, world'}
```


### Mid-level API [&#x219F;](#table-of-contents)

```clojure
(require '[clojang.mbox :as mbox]
         '[clojang.node :as node])

(def gurka (node/new :gurka))
(def inbox (mbox/new gurka :echo))

(def msg [(mbox/get-pid inbox) :hello-world])

(mbox/! inbox :echo :gurka msg)
(mbox/receive inbox)

[#object[com.ericsson.otp.erlang.OtpErlangPid
         0x1fe20514
         "#Pid<gurka@mndltl01.1.0>"]
 :hello-world]
```

From LFE:

```cl
(clojang-lfe@mndltl01)> (! #(echo gurka@mndltl01) `#(,(self) hej!))
#(<0.35.0> hej!)
```

Then back in Clojure:

```clojure
(let [[lfe-pid _] (mbox/receive inbox)]
  (mbox/! inbox lfe-pid msg))
```

Then, back in LFE:

```cl
(clojang-lfe@mndltl01)> (c:flush)
Shell got {<5926.1.0>,'hello-world'}
```


### Running Tests [&#x219F;](#table-of-contents)

All the tests may be run with just one command:

```bash
$ rebar3 eunit
```

This will not only run Erlang and LFE unit tests, it also runs the Clojure unit tests for Clojang.

**Clojure Test Selectors**

If you would like to be more selective in the types of Clojang tests which get run, you may be interested in reading this section.

The Clojang tests use metadata annotations to indicate whether they are unit, system, or integration tests. to run just the unit tests, you can do any one of the following, depending upon what you're used to:

```bash
$ lein test
$ lein test :unit
$ lein test :default
```

To run just the system tests:

```bash
$ lein test :system
```

And, similarly, just the integration tests:

```bash
$ lein test :integtration
```

To run everything:

```bash
$ lein test :all
```

This is what is used by the ``rebar3`` configuration to run the Clojang tests.


## Erlang, Clojure, and JInterface [&#x219F;](#table-of-contents)

If you are interested in building your own JInterface ``.jar`` file for use with a Clojure project, be sure fo check out the page [Building JInterface for Clojure](https://oubiwann.github.io/clojang/current/80-building-jinterface.html) on the Clojang docs site.


## License [&#x219F;](#table-of-contents)

```
Copyright © 2015-2016 Duncan McGreggor

Distributed under the Apache License Version 2.0.
```
