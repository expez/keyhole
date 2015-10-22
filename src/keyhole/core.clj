(ns keyhole.core
  (:refer-clojure :exclude [update-in get-in])
  (:require [keyhole
             [impl :as impl]
             [parser :as parser]
             [protocols :as protocols]]))

(defmacro update-in [coll spec f]
  (let [spec (parser/parse-spec spec f)]
    `(~(protocols/transformer spec) ~coll)))

(defmacro get-in [coll spec]
  (let [spec (parser/parse-spec spec identity)]
    `(impl/remove-sentinels (~(protocols/selector spec) ~coll))))
