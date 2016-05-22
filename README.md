# clojang [![Build Status][travis-badge]][travis][![Clojars Project][clojars-badge]][clojars]

[![Clojang logo][clojang-logo]][clojang-logo-large]

*Erlang's JInterface in Idiomatic Clojure*


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Building](#building-)
* [Shells & REPLs](#shells--repls-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [Running Tests](#running-tests-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This project provides a solution to the [aesthetic problem of JInterface](https://github.com/oubiwann/clojang/wiki/Example:-JInterface-in-Clojure). While JInterface is an invaluable tool for projects that need to have JVM and Erlang VM languages communicating with each other, it is rather verbose and cumbersom to do so in Clojure.

The [jiface low-level API](https://github.com/clojang/jiface) solves this to a certain extent, but it requires that programmers perform all their own type conversions, manual creation of notes, etc.

The Clojang library, however, provides an interface that is not only syntactically idiomatic Clojure (not unlike the jiface library), but even more so, provides developers with the same level of convenience they have come to expect when using Clojure libraries in general, without the need to perform many manual operations in order to handle Erlang data.


## Dependencies [&#x219F;](#contents)

* Java
* Erlang
* lein
* rebar3


## Building [&#x219F;](#contents)

``rebar3`` is used for the top-level builds of the project. It runs ``lein`` under the covers in order to build the Clojure code and create the Clojang``.jar`` file. As such, to build everything -- LFE, Erlang, and Clojure -- you need only do the following:

* ``rebar3 compile``

If you wish to build your own JInterface ``.jar`` file and not use the one we've uploaded to Clojars, you'll need to follow the instrucations given in the [jinterface-builder Clojang project](https://github.com/clojang/jinterface-builder).


## Shells & REPLs [&#x219F;](#contents)

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


## Documentation [&#x219F;](#contents)

Project documentation, including Clojang API reference docs, Javadocs for JInterface, and the Erlang JInterface User's Guide, is availble here:

* [http://clojang.github.io/clojang/current/](http://clojang.github.io/clojang/current/)

Quick links for the other docs:

* Clojang User Guides:
  * [jiface User's Guide](http://clojang.github.io/jiface/current/10-low-level-api.html/) - A translation of the *JInterface User's Guide* (Erlang documantaion) from Java into Clojure; this is refered to as the Clojang low-level API.
  * [Clojang User's Guide](http://clojang.github.io/clojang/current/20-mid-level-api.html) - An adaptation of the *jiface User's Guide* which demonstrates the improved API that Clojang offers
* [JInterface User's Guide](http://clojang.github.io/clojang/current/erlang/jinterface_users_guide.html) - The JInterface (Java support for Erlang Ports) documentation provided in Erlang distributions
* [Jinterface Javadocs](http://clojang.github.io/clojang/current/erlang/java) - Javadoc-generated API documentation built from the JInterface source code


## Usage [&#x219F;](#contents)

Using Clojang in a project is just like any other Clojure library. Just add the following to the ``:dependencies`` in your ``project.clj`` file:

[![Clojars Project][clojars-badge]][clojars]

For the Erlang/LFE side of things, you just need to add the Github URL to your ``rebar.config`` file, as with any other rebar-based Erlang VM project.

As for actual code usage, the documentation section provides links to developer guides and API references, but below are also provided two quick examples, one each in the low- and mid-level APIs.



```clojure
(require '[clojang.mbox :as mbox]
         '[clojang.node :as node])

(mbox/! inbox :echo :gurka [(mbox/self) :hello-word])
(mbox/receive)
```

The last function

```
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


## Running Tests [&#x219F;](#contents)

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


## License [&#x219F;](#contents)

```
Copyright © 2015-2016 Duncan McGreggor

Distributed under the Apache License Version 2.0.
```


<!-- Named page links below: /-->

[travis]: https://travis-ci.org/clojang/clojang
[travis-badge]: https://travis-ci.org/clojang/clojang.png?branch=master
[clojang-logo]: resources/images/clojang-logo-250x.png
[clojang-logo-large]: resources/images/clojang-logo-1000x.png
[clojars]: https://clojars.org/clojang/clojang
[clojars-badge]: https://img.shields.io/clojars/v/clojang/clojang.svg
