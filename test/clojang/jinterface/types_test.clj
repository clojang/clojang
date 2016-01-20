(ns clojang.jinterface.types-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.types :refer :all]))

(deftest encoding-test
  (testing "Keyword encoding"
    (is (= (-> :toto encode decode) :toto)))
  (testing "Map encoding"
    (is (= (-> {:a :b :c :d} encode decode) {:a :b :c :d})))
  (testing "Vector encoding"
    (is (= (-> [:a :b :c :d] encode decode) [:a :b :c :d])))
  (testing "List encoding"
    (is (= (-> '(:a :b :c :d) encode decode) '(:a :b :c :d))))
  (testing "Set encoding : not bijective, set is list"
    (is (= (-> #{:a :b :c :d} encode decode set) #{:a :b :c :d})))
  (testing "Float encoding"
    (is (= (-> 4.3 encode decode) 4.3)))
  (testing "Bool encoding"
    (is (= (-> true encode decode) true)))
  (testing "Binary encoding"
    (is (= (-> (byte-array (repeat 4 0)) encode decode seq) (seq (byte-array (repeat 4 0))))))
  (testing "String encoding not reflective : string is binary"
    (is (= (-> "toto" encode decode String.) "toto"))))
