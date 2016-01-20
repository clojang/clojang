# clojang

[![][clojang-logo]][clojang-logo-large]

[clojang-logo]: resources/images/clojang-logo-250x.png
[clojang-logo-large]: resources/images/clojang-logo-1000x.png

*Erlang's JInterface in Idiomatic Clojure*

![Clojars Project](http://clojars.org/clojang/latest-version.svg)


#### Table of Contents

* [Introduction](#introduction-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Running Tests](#running-tests-)
* [Erlang and JInterface](#erlang-and-jinterface-)
  * [Matching Versions](#matching-versions-)
  * [Building JInterface for Clojure](#building-jinterface-for-clojure-)
    * [Finding Your Root Dir](#finding-your-root-dir-)
    * [Getting Versions](#getting-versions-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

TBD


## Documentation [&#x219F;](#table-of-contents)

Project documentation, including both API reference docs as well as tutorials and user guides, is availble here:

* [http://oubiwann.github.io/clojang/current/](http://oubiwann.github.io/clojang/current/)


## Usage [&#x219F;](#table-of-contents)

TBD


### Running Tests [&#x219F;](#table-of-contents)

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


## Erlang and JInterface [&#x219F;](#table-of-contents)

You almost certainly do not want to use the JInterface dependencies that
are provided in Clojars. An explanation is given below with simple instructions
for making the JInterface for your Erlang version available to Clojure.


### A Note on Versions [&#x219F;](#table-of-contents)

JInterface is only guaranteed to work with the version of Erlang with which it
was released. The following version numbers are paired:

| Erlang Release | Erlang Version (erts) | JInterface |
|----------------|-----------------------|------------|
| 18.2           | 7.2                   | 1.6.1      |
| 18.1           | 7.1                   | 1.6        |
| 18.0           | 7.0                   | 1.6        |
| 17.5           | 6.4                   | 1.5.12     |
| 17.4           | 6.3                   | 1.5.11     |
| 17.3           | 6.2                   | 1.5.10     |
| 17.2           | 6.1                   | 1.5.9      |
| 17.1           | 6.1                   | 1.5.9      |
| 17.0           | 6.0                   | 1.5.9      |
| R16B03         | 5.10.4                | 1.5.8      |
| R16B02         | 5.10.3                | 1.5.8      |
| R16B01         | 5.10.2                | 1.5.8      |
| R16B           | 5.10.1                | 1.5.8      |
| R15B03         | 5.9.3                 | 1.5.6      |
| R15B02         | 5.9.2                 | 1.5.6      |
| R15B01         | 5.9.1                 | 1.5.6      |
| R15B           | 5.9                   | 1.5.5      |


### Building JInterface for Clojure [&#x219F;](#table-of-contents)

To ensure that your version of JInterface is ready for use by Clojure with your
version of Erlang, simply do this:

```bash
$ make jinterface-local
```

This will discover the Erlang root directory for the first ``erl`` found in your
``PATH``. It will also location the JInterface ``.jar`` file for that version
of Erlang.

If you wish to override these, you may do the following:

```
make jinterface-local ERL_LIBS=/opt/erlang/15.3.1
```

This ``make`` target (which depends upon Maven being installed) will
generate a ``lein``-friendly ``.jar`` file for you in your
``~/.m2/repository`` directory, just like ``lein`` does with downloaded Clojars.


#### Finding Your Root Dir [&#x219F;](#table-of-contents)

If you don't know what your Erlang root directory is, just fire up Erlang and
do the following:


```
$ erl
```
```erlang
1> code:root_dir().
"/opt/erlang/18.2"
```

The ``Makefile`` uses this to get the default Erlang root directory:

```
ERL_LIBS=$(erl -eval "io:format(code:root_dir()),halt()" -noshell)
```

#### Getting Versions [&#x219F;](#table-of-contents)

The ``Makefile`` provides a convenience target for getting some important version numbers:

```bash
$ make show-versions
Project: clojang, 0.1.0
Erlang: Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
JInterface: 1.6.1
Clojure: 1.7.0
lein/JVM: Leiningen 2.5.3 on Java 1.7.0_91 OpenJDK 64-Bit Server VM
```


## License [&#x219F;](#table-of-contents)

```
Copyright © 2015-2016 Duncan McGreggor

Distributed under the Apache License Version 2.0.
```
