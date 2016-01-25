# Building JInterface for Clojure

## Erlang and JInterface

Getting Erlang, JInterface, and Clojure working together can be tricky just for the simple fact that the JInterface ``.jar`` files tend to be a mismatched mess. I've *tried* to help out with this by publishing what *seem* to be working JInterface ``.jar`` files on Clojars. I don't have the infrastructure available for testing all of these, so feedback is welcome.

To make this easier for devs to do themselves, this project includes some ``make`` targets for building JInterface ``.jar`` files. Also, a table is proivded below to help match Erlang VM with the right JInterface relase.


### A Note on Versions

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


### Building JInterface

To ensure that your version of JInterface is ready for use by Clojure with your
version of Erlang, simply do this:

```bash
$ make jinterface-local
```

This will discover the Erlang root directory for the first ``erl`` found in your
``PATH``. It will also location the JInterface ``.jar`` file for that version
of Erlang.

If you wish to override these, you may do the following:

```bash
$ make jinterface-local ERL_LIBS=/opt/erlang/15.3.1
```

This ``make`` target (which depends upon Maven being installed) will
generate a ``lein``-friendly ``.jar`` file for you in your
``~/.m2/repository`` directory, just like ``lein`` does with downloaded Clojars.


#### Finding Your Root Dir

If you don't know what your Erlang root directory is, just fire up Erlang and
do the following:


```bash
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

#### Getting Versions

The ``Makefile`` provides a convenience target for getting some important version numbers:

```bash
$ make show-versions
Project: clojang, 0.1.0
Erlang: Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
JInterface: 1.6.1
Clojure: 1.7.0
lein/JVM: Leiningen 2.5.3 on Java 1.7.0_91 OpenJDK 64-Bit Server VM
```
