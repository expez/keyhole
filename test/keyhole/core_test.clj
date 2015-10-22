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
  [1] (k/get-in {:foo {:bar 1}} [:foo :bar])
  {:foo {:bar 2}} (k/update-in {:foo {:bar 1}} [:foo :bar] inc))

(defexamples range*-test
  [[0 1]] (k/get-in [0 1 2] [(range* 0 2)])
  [0 1 2] (k/update-in [-1 0 2] [(range* 0 2) all*] inc))

(defexamples all*-test
  [0 1 2] (k/get-in [{:foo 0} {:foo 1} {:foo 2}] [all* :foo])
  [{:foo 1} {:foo 2} {:foo 3}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 2}] [all* :foo] inc))

(defexamples first*-test
  [0] (k/get-in [{:foo 0} {:foo 1} {:foo 2}] [first* :foo even?])
  [{:foo 1} {:foo 1} {:foo 2}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 2}] [first* :foo] inc))

(defexamples last*-test
  [2] (k/get-in [{:foo 0} {:foo 1} {:foo 2}] [last* :foo])
  [{:foo 0} {:foo 1} {:foo 3}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 2}] [last* :foo] inc))

(defexamples butlast*-test
  [0 1] (k/get-in [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo])
  [{:foo 1} {:foo 2} {:foo 2}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo] inc))

(defexamples rest*-test
  [1 2] (k/get-in [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo])
  [{:foo 0} {:foo 2} {:foo 3}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo] inc))

(defexamples pred-test
  [2 4] (k/get-in [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?])
  [{:foo 1} {:foo 1} {:foo 3}]
  (k/update-in [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?] dec)
  [{:foo 0} {:foo 0} {:foo 3}]
  (k/update-in [{:foo 0} {:foo 1} {:foo 3}] [all* :foo #(= % 1)] dec))

(defexamples filter*-test
  [2] (k/get-in [{:foo 1} {:foo 2 :bar 1} {:foo 4}] [(filter* #(:bar %)) first* :foo])
  [2 1 3 6 10 4 8] (k/update-in [2 1 3 6 9 4 8] [(filter* odd?) last*] inc)
  [0 1 2 3 10 5 8 7 6 9 4 11 12 13 14 15]
  (k/update-in [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
               [(range* 4 11) (filter* even?)] reverse))

(defexamples generalized-kw-test
  [3] (k/get-in [{"foo" {[1 2] 3}}] [first* (key "foo") (key [1 2])])
  [{"foo" {[1 2] 4}}]
  (k/update-in [{"foo" {[1 2] 3}}] [first* (key "foo") (key [1 2])] inc))

(defexamples nth*-test
  [4] (k/get-in [{:foo 1} {:foo 2 :bar 1} {:foo 4}] [(nth* 2) :foo])
  [{:foo 1} {:foo 3 :bar 1} {:foo 4}]
  (k/update-in [{:foo 1} {:foo 2 :bar 1} {:foo 4}] [(nth* 1) :foo] inc))

(defexamples nthrest*-test
  [4] (k/get-in [{:foo 1} {:foo 2 :bar 1} {:foo 4}] [(nthrest* 2) first* :foo])
  [{:foo 1} {:foo 3 :bar 1} {:foo 4}]
  (k/update-in [{:foo 1} {:foo 2 :bar 1} {:foo 3}] [(nthrest* 1) :foo] inc))
