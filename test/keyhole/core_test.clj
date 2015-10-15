(ns keyhole.core-test
  (:require [clojure.test :as t]
            [keyhole.core :as k]))

(t/deftest keyword-test
  (t/is (= 1 (k/select {:foo {:bar 1}} [:foo :bar])))
  (t/is (= {:foo {:bar 2}}
           (k/transform {:foo {:bar 1}} [:foo :bar] inc))))

(t/deftest range-test
  (t/is (= [0 1] (k/select [0 1 2] [(range 0 2)])))
  (t/is (= [0 1 2] (k/transform [-1 0 2] [(range 0 2)] inc))))

(t/deftest all*-test
  (t/is (= [0 1 2] (k/select [{:foo 0} {:foo 1} {:foo 2}] [all* :foo])))
  (t/is (= [{:foo 1} {:foo 2} {:foo 3}]
           (k/transform [{:foo 0} {:foo 1} {:foo 2}] [all* :foo] inc))))

(t/deftest first*-test
  (t/is (= 0 (k/select [{:foo 0} {:foo 1} {:foo 2}] [first* :foo])))
  (t/is (= [{:foo 1} {:foo 1} {:foo 2}]
           (k/transform [{:foo 0} {:foo 1} {:foo 2}] [first* :foo] inc))))

(t/deftest last*-test
  (t/is (= 2 (k/select [{:foo 0} {:foo 1} {:foo 2}] [last* :foo])))
  (t/is (= [{:foo 0} {:foo 1} {:foo 3}]
           (k/transform [{:foo 0} {:foo 1} {:foo 2}] [last* :foo] inc))))

(t/deftest butlast*-test
  (t/is (= [0 1] (k/select [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo])))
  (t/is (= [{:foo 1} {:foo 2} {:foo 2}]
           (k/transform [{:foo 0} {:foo 1} {:foo 2}] [butlast* :foo] inc))))

(t/deftest rest*-test
  (t/is (= [1 2] (k/select [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo])))
  (t/is (= [{:foo 0} {:foo 2} {:foo 3}]
           (k/transform [{:foo 0} {:foo 1} {:foo 2}] [rest* :foo] inc))))

(t/deftest pred-test
  (t/is (= [2 4]
           (k/select [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?])))
  (t/is (= [{:foo 1} {:foo 1} {:foo 3}]
           (k/transform [{:foo 1} {:foo 2} {:foo 4}] [all* :foo even?] dec))))
