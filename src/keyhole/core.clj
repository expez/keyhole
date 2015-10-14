(ns keyhole.core
  (:require [clojure
             [string :as str]
             [walk :as walk]]))

(defprotocol Transformer
  "The transformer code is used to perform some transformation on a
  value in a nested datastructure.

  The emitted form should be a function which is ready to consume a value.

  When emitting code, in the transformer function, all the data fields
  of the corresponding keyhole is available.

  The placeholder ::next is used to indicate where the next
  transformer should go.

  Here's the transformer code for the keyword keyhole:
  `(partial update* ::next ~k)

  Where update* is update with the parameters re-ordered to
  afford partial application."
  (transformer [this] "Emit transformer code."))

(defprotocol Selector
  "The selector code is used to perform some a value lookup in a
  nested datastructure.

  The emitted form should be a function which is ready to consume a
  value.

  When emitting code, in the selector function, all the data fields
  of the corresponding keyhole is available.

  The placeholder ::next is used to indicate where the next
  selector should go.

  Here's the selector code for the keyword keyhole:
  `(comp ::next ~k)"
  (selector [this] "Emit selector code."))

(defmacro defkeyhole
  "A keyhole is a way to look into a datastructure.  By composing
  various keyholes we can extract values or use keyhole surgery to change
  them.

  name is used to give a name to the keyhole we're creating.

  fields are the data fields needed to perform transformations or selections.

  dispatch-val is used recognize instances of this keyhole in the spec.
  See the docstring for parse-dispatcher.

  parser is a function which will be passed the spec and should return
  an ordered list matching the entries in fields.

  selector is the form we should emit to lookup a value.
  See the docstring for the Selector protocol.

  transformer is the form we should emit to transform a value.
  See the docstring for the Selector protocol.

  Here is the keyhole for keywords:

  (defkeyhole keyword [k] clojure.lang.Keyword list
   :selector `(comp ::next ~k)
   :transformer `(partial update* ::next ~k))"
  [name fields dispatch-val parser & {:keys [selector transformer]}]
  (let [record-name (-> name str str/capitalize symbol)
        constructor (symbol (str "->" record-name)) ]
    `(do
       (defrecord ~record-name ~fields
         Transformer
         (transformer [this#] ~transformer)
         Selector
         (selector [this#] ~selector))
       (defmethod parse ~dispatch-val ~(symbol (str name "-parser-method"))
         [spec#] (apply ~constructor (~parser  spec#))))))

(defn parse-dispatcher
  ":foo => clojure.lang.keyword
  (range 0 2) => range
  foo => [clojure.lang.Symbol foo]

  Anything else maps to itself."
  [spec]
  (cond
    (sequential? spec) (first spec)
    (keyword? spec) (type spec)
    (symbol? spec) [(type spec) spec]
    :else spec))

(defmulti parse "Parse a spec." #'parse-dispatcher)

(defmethod parse :default [spec]
  (throw (ex-info "Uknown spec" {:spec spec})))

(defn- parse-spec [spec]
  (map parse spec))

(defn- same-collection-type
  "Coerce new to the same type as old."
  [old new]
  (if (vector? old)
    (into [] new)
    new))

(defn- slice
  "Extract the elements between start and end (exclusive) by step.

  (slice [1 2 3 4] 0 3 2) => [1 3]

  The type of xs is preserved."
  [start end step xs]
  (let [xs' (some->> xs (drop start) (take (- end start)) (take-nth step))]
    (same-collection-type xs xs')))

(defn- map-slice
  "Apply f to every value in the slice created by start end and step
  on xs."
  [start end step f xs]
  (map f (slice start end step xs)))

(defmacro do1
  "Like do but return the value of the first form instead of the
  last."
  [res & exprs*]
  `(let [res# ~res]
     `(do ~~@exprs*)
     res#))

(defn- splice
  "Splice the elements of ys into xs by replacing every step element
  between start ane end (exclusive).

  Splice is the reverse of slice.

  (splice [1 2 3 4] (map (constantly 0) (slice [1 2 3 4])) 0 3 2) => [0 2 0 4]

  The type of the result is that of xs."
  [xs ys start end step]
  (let [xs' (into [] xs)
        ys (into [] ys)
        yi (volatile! 0)
        res (for [i (range (count xs'))]
              (if (and (>= i start)
                       (< i end)
                       (= (rem i step) 0))
                (do1 (nth ys @yi) (vswap! yi inc))
                (nth xs' i)))]
    (same-collection-type xs res)))

(defn- update-slice
  "Apply f to every step element of xs between start and end (exclusive.)"
  [f start end step xs]
  (splice xs (map f (slice start end step xs)) start end step))

(defn update* [f k m]
  (update m k f))

(defn- combine [forms form]
  (walk/postwalk (fn [f] (if (= f ::next) form f)) forms))

(defn- combine-forms
  [forms coll f]
  (let [forms (conj forms f)]
    (reduce combine (list (first forms) coll) (rest forms))))

(defmacro transform [coll spec f]
  (let [spec (parse-spec spec)
        transformer-forms (mapv transformer spec)]
    (combine-forms transformer-forms coll (eval f))))

(defmacro select [coll spec]
  (let [spec (parse-spec spec)
        selector-forms (mapv selector spec)]
    (combine-forms selector-forms coll identity)))

(defn- range-parser [[_ start end step]]
  [start end (or step 1)])

(defkeyhole range [start end step] 'range range-parser
  :selector `(partial map-slice ~start ~end ~step ::next)
  :transformer `(partial update-slice ::next ~start ~end ~step))

(defkeyhole kw [k] clojure.lang.Keyword list
  :selector `(comp ::next ~k)
  :transformer `(partial update* ::next ~k))

(defn- update-all [f xs]
  (->> xs (map f) (same-collection-type xs)))

(defkeyhole all* [] [clojure.lang.Symbol 'all*] (constantly [])
  :selector `(partial map ::next)
  :transformer `(partial update-all ::next))

(defn- update-first [f [x & xs]]
  (same-collection-type xs (cons (f x) xs)))

(defkeyhole first* [] [clojure.lang.Symbol 'first*] (constantly [])
  :selector `(comp ::next first)
  :transformer `(partial update-first ::next))

(defn- update-last [f xs]
  (same-collection-type xs (concat (butlast xs) [(f (last xs))])))

(defkeyhole last* [] [clojure.lang.Symbol 'last*] (constantly [])
  :selector `(comp ::next last)
  :transformer `(partial update-last ::next))

(defn- update-butlast [f xs]
  (same-collection-type xs (concat (map f (butlast xs)) [(last xs)])))

(defkeyhole butlast* [] [clojure.lang.Symbol 'butlast*] (constantly [])
  :selector `(comp (partial map ::next) butlast)
  :transformer `(partial update-butlast ::next))

(println
 (select  [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo]))

(println
 (transform [{:foo [1 2 3]} {:foo [4 5 6]} {:foo [7 8 9]} {:foo [10 11 12]}]
            [(range 0 2) :foo (range 2 3)] inc))

(println
 (select [{:a 1} {:a 2} {:a 3} {:a 4}] [butlast* :a]))


;; (def DATA {:a {:b {:c 1}}})

;; (defn benchmark [iters afn]
;;   (time
;;    (dotimes [_ iters]
;;      (afn))))

;; (transform DATA [:a :b :c] inc )
;; (benchmark 1000000 #(get-in DATA [:a :b :c]))
;; => "Elapsed time: 77.018 msecs"

;; (benchmark 1000000 #(select DATA [:a :b :c]))
;; => "Elapsed time: 4143.343 msecs"

;; (benchmark 1000000 #(-> DATA :a :b :c vector))
;; => "Elapsed time: 34.235 msecs"

;; (benchmark 1000000 #(update-in DATA [:a :b :c] inc))
;; => "Elapsed time: 1037.94 msecs"

;; (benchmark 1000000 #(transform DATA [:a :b :c] inc))
;; => "Elapsed time: 4305.429 msecs"
;; (transform DATA [:a :b :c] inc )
;; (defn manual-transform [data]
;;   (update data
;;           :a
;;           (fn [d1]
;;             (update d1
;;                     :b
;;                     (fn [d2]
;;                       (update d2 :c inc))))))

;; (benchmark 1000000 #(manual-transform DATA))
;;=> "Elapsed time: 161.945 msecs"
