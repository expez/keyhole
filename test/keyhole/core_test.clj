(ns keyhole.core-test
  (:require [clojure.test :as t]
            [keyhole.core :as k]))

(defmacro defexamples [name & expected-actual-pairs]
  (when-not (even? (count expected-actual-pairs))
    (throw (ex-info "defexamples requires an even number of expected-actual-pairs"
                    {:expected-actual-pairs expected-actual-pairs})))
  `(t/deftest ~name
     ~@(for [[expected actual] (partition 2 expected-actual-pairs)]
         `(t/is (= ~expected ~actual)))))

(defexamples keyword-test
  1 (k/select {:foo {:bar 1}} [:foo :bar])
  {:foo {:bar 2}} (k/transform {:foo {:bar 1}} [:foo :bar] inc))

(defexamples range-test
  [0 1] (k/select [0 1 2] [(range 0 2)])
  [0 1 2] (k/transform [-1 0 2] [(range 0 2)] inc))

(defexamples all*-test
  [0 1 2] (k/select [{:foo 0} {:foo 1} {:foo 2}] [all* :foo])
  [{:foo 1} {:foo 2} {:foo 3}]
  (k/transform [{:foo 0} {:foo 1} {:foo 2}] [all* :foo] inc))

(defexamples first*-test
  0 (k/select [{:foo 0} {:foo 1} {:foo 2}] [first* :foo])
  [{:foo 1} {:foo 1} {:foo 2}]
  (k/transform [{:foo 0} {:foo 1} {:foo 2}] [first* :foo] inc))

(defexamples last*-test
  2 (k/select [{:foo 0} {:foo 1} {:foo 2}] [last* :foo])
  [{:foo 0} {:foo 1} {:foo 3}]
  (k/transform [{:foo 0} {:foo 1} {:foo 2}] [last* :foo] inc))

(defexamples butlast*-test
  [0 1] (k/select [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo])
  [{:foo 1} {:foo 2} {:foo 2}]
  (k/transform [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo] inc))

(defexamples rest*-test
  [1 2] (k/select [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo])
  [{:foo 0} {:foo 2} {:foo 3}]
  (k/transform [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo] inc))

(defexamples pred-test
  [2 4] (k/select [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?])
  [{:foo 1} {:foo 1} {:foo 3}]
  (k/transform [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?] dec)
  [{:foo 0} {:foo 0} {:foo 3}]
  (k/transform [{:foo 0} {:foo 1} {:foo 3}] [all* :foo #(= % 1)] dec)
  )
