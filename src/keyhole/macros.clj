(ns keyhole.macros
  (:require [clojure.string :as str]
            [keyhole
             [parser :as parser]
             [protocols :as protocols]]))

(defmacro defkeyhole
  "A keyhole is a way to look into a datastructure.  By composing
  various keyholes we can extract values or perform keyhole surgery to
  change them.

  name is used to give a name to the keyhole we're creating.

  fields are the data fields needed to perform transformations or selections.

  dispatch-val is used to recognize instances of this keyhole in the spec.
  See the docstring for parse-dispatcher about which values to use here.

  parser is a function which will be passed the spec and should return
  an ordered list matching the entries in fields.

  selector is the form we should emit to lookup a value.
  See the docstring for the Selector protocol.

  transformer is the form we should emit to transform a value.
  See the docstring for the Transformer protocol.

  transformer-basis is required to provide hints to other transformations.
  See the docstring for the Transformer protocol.

  Here is the keyhole for keywords:

  (defkeyhole kw [k] ::keyword list
  :selector `(comp ~next-selector ~k)
  :transformer `(partial update* ~next-transformer ~k))"
  [name fields dispatch-val parser
   & {:keys [selector transformer transformer-basis]}]
  (let [record-name (-> name str str/capitalize symbol)
        constructor (symbol (str "map->" record-name))]
    `(do
       (defrecord ~record-name
           ~(into '[next-transformer next-selector
                    transformer-basis next-transformer-basis] fields)
         protocols/Transformer
         (transformer [this#] ~transformer)
         (transformer-basis [this#] ~transformer-basis)
         protocols/Selector
         (selector [this#] ~selector))
       (defmethod parser/parse ~dispatch-val ~(symbol (str name "-parser-method"))
         [spec#]
         (~constructor (merge {:next-transformer nil :next-selector nil
                               :transformer-basis nil
                               :next-transformer-basis nil}
                              (zipmap (map keyword '~fields) (~parser spec#))))))))
