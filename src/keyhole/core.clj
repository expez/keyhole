(ns keyhole.core
  (:require [keyhole
             [impl :as impl]
             [parser :as parser]
             [protocols :as protocols]]))

(defmacro transform [coll spec f]
  (let [spec (parser/parse-spec spec f)]
    `(~(protocols/transformer spec) ~coll)))

(defmacro select [coll spec]
  (let [spec (parser/parse-spec spec identity)]
    `(impl/remove-sentinels (~(protocols/selector spec) ~coll))))
