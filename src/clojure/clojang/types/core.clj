(ns clojang.types.core
  (:require [clojang.core :as clojang]
            [clojang.types.constructor]
            [clojang.types.predicate]
            [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [atom boolean byte char char? double float float?
                            int list list? long map map? short string?]))

(import-vars
  [clojang.types.constructor
    atom
    boolean
    ;binary
    ;bitstr
    byte
    char
    double
    float
    ;fun
    int
    list
    long
    map
    ;object
    ;pid
    ;port
    ;ref
    short
    string
    ;sublist
    tuple
    undefined
    uint
    ushort])

(import-vars
  [clojang.types.predicate
    atom?
    boolean?
    binary?
    bitstr?
    byte?
    char?
    double?
    float?
    fun?
    int?
    list?
    long?
    map?
    object?
    pid?
    port?
    ref?
    short?
    string?
    sublist?
    tuple?
    undefined?
    uint?
    ushort?])
