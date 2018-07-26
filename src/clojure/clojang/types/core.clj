(ns clojang.types.core
  (:require
    [clojang.types.constructor]
    [clojang.types.converter]
    [clojang.types.predicate]
    [potemkin :refer [import-vars]])
  (:refer-clojure :exclude [atom boolean boolean? byte char char?
                            double double? float float? int int?
                            list list? long map map? ref short string?]))

(import-vars
  [clojang.types.constructor
    atom
    boolean
    ;;binary
    ;;bitstr
    byte
    char
    double
    float
    ;fun
    int
    list
    long
    map
    ;;object
    pid
    port
    ref
    short
    string
    ;;sublist
    tuple
    undefined
    uint
    ushort])

(import-vars
  [clojang.types.converter
    clj->erl
    erl->clj])

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
    ;;undefined?
    uint?
    ushort?])
