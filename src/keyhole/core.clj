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

  The field next-transformer can be used to place the subsequent
  transformer in the right place.

  transformer-basis is used to give some transformations, earlier in
  the chain, a hint about what's going to change so they can
  re-construct properly.  E.g. filter* needs last* to hint that
  filter* only needs to update the last of the filtered values it
  produces.  The basis is a pair of ::val or ::seq and a basis
  function.  The first half of the the pair indicates whether or not
  the return value should be spliced in or not.

  Here's the transformer code for the keyword keyhole:
  `(partial update* ~next-transformer ~k)

  Where update* is update with the parameters re-ordered to
  afford partial application."
  (transformer [this] "Emit transformer code.")
  (transformer-basis [this] "Emit the basis transformer."))

(defprotocol Selector
  "The selector code is used to perform a value lookup in a
  nested datastructure.

  The emitted form should be a function which is ready to consume a
  value.

  When emitting code, in the selector function, all the data fields
  of the corresponding keyhole is available.

  Additionally the field next-selector can be used to place the
  subsequent transformer in the right place.

  Since a selector is a form which evaluates to a partial ready to
  consume a value conditional selection is tricky.  This is solved by
  returning the value ::nothing, which will be removed from the final
  result set.

  Here's the selector code for the keyword keyhole:
  `(comp ~next-selector ~k)"
  (selector [this] "Emit selector code."))

(defrecord Fin [f]
  Transformer
  (transformer [this] f)
  (transformer-basis [this] [::val f])
  Selector
  (selector [this] f))

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
         Transformer
         (transformer [this#] ~transformer)
         (transformer-basis [this#] ~transformer-basis)
         Selector
         (selector [this#] ~selector))
       (defmethod parse ~dispatch-val ~(symbol (str name "-parser-method"))
         [spec#]
         (~constructor (merge {:next-transformer nil :next-selector nil
                               :transformer-basis nil
                               :next-transformer-basis nil}
                              (zipmap (map keyword '~fields) (~parser spec#))))))))

(defn parse-dispatcher
  ":foo => ::keyword
  (range 0 2) => range
  foo => [::fn foo]
  symbol? ::predicate

  Anything else maps to itself."
  [spec]
  (letfn [(predicate? [spec]
            (or (and (sequential? spec) (= (first spec) 'fn*)) ; #(= 1 %)
                (and (symbol? spec) (resolve spec) (fn? (deref (resolve spec))))))]
    (cond
      (predicate? spec) ::predicate
      (sequential? spec) (first spec)
      (keyword? spec) ::keyword
      (symbol? spec) [::fn spec]
      :else spec)))

(defmulti parse "Parse a spec." #'parse-dispatcher)

(defmethod parse :default [spec]
  (throw (ex-info "Uknown spec" {:spec spec})))

(defn- same-collection-type
  "Coerce new to the same type as old."
  [old new]
  (if (vector? old)
    (into [] new)
    new))

(defn- update-first [f [x & xs]]
  (same-collection-type xs (cons (f x) xs)))

(defn- link-steps [spec]
  (let [[fin & spec] (reverse spec)]
    (reduce (fn [n s]
              (merge s {:next-transformer (transformer n)
                        :next-transformer-basis (transformer-basis n)
                        :next-selector (selector n)}))
            fin
            spec)))

(defn- parse-spec [spec transformer]
  (link-steps (conj (mapv parse spec) (Fin. transformer))))

(defn- slice
  "Extract the elements between start and end (exclusive) by step.

  (slice [1 2 3 4] 0 3 2) => [1 3]

  The type of xs is preserved."
  [start end step xs]
  (let [xs' (some->> xs (drop start) (take (- end start)) (take-nth step))]
    (same-collection-type xs xs')))

(defn map-slice
  "Apply f to every value in the slice created by start end and step
  on xs."
  [start end step f xs]
  (map f (slice start end step xs)))

(defmacro ^:private do1
  "Like do but return the value of the first form instead of the
  last."
  [res & exprs*]
  `(let [res# ~res]
     `(do ~~@exprs*)
     res#))

(defn- splice
  "Splice the elements of ys into xs by replacing every step element
  between start and end (exclusive).

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

(defn update-slice
  "Apply f to every step element of xs between start and end (exclusive.)"
  [f start end step xs]
  (splice xs (map f (slice start end step xs)) start end step))

(defn update* [f k m]
  (update m k f))

(defmacro transform [coll spec f]
  (let [spec (parse-spec spec f)]
    `(~(transformer spec) ~coll)))

(defn remove-sentinels [res]
  (cond
    (sequential? res) (remove #{::nothing} res)
    (= res ::nothing) nil
    :else res))

(defmacro select [coll spec]
  (let [spec (parse-spec spec identity)]
    `(remove-sentinels (~(selector spec) ~coll))))

(defn- range-parser [[_ start end step]]
  [start end (or step 1)])

(defkeyhole range* [start end step] 'range* range-parser
  :selector `(partial map-slice ~start ~end ~step ~next-selector)
  :transformer `(partial update-slice ~next-transformer ~start ~end ~step)
  :transformer-basis [::seq `(partial slice ~start ~end ~step)])

(defkeyhole kw [k] ::keyword list
  :selector `(comp ~next-selector ~k)
  :transformer `(partial update* ~next-transformer ~k))

(defkeyhole generalized-kw [k] 'key (fn [[_ k]] (list k))
  :selector `(comp ~next-selector (fn [m#] (get m# ~k)))
  :transformer `(partial update* ~next-transformer ~k))

(defn update-all [f xs]
  (same-collection-type xs (map f xs)))

(defkeyhole all* [] [::fn 'all*] (constantly [])
  :selector `(partial map ~next-selector)
  :transformer `(partial update-all ~next-transformer)
  :transformer-basis [::seq identity])

(defn update-first [f [x & xs]]
  (same-collection-type xs (cons (f x) xs)))

(defkeyhole first* [] [::fn 'first*] (constantly [])
  :selector `(comp ~next-selector first)
  :transformer `(partial update-first ~next-transformer)
  :transformer-basis [::val first])

(defn update-last [f xs]
  (same-collection-type xs (concat (butlast xs) [(f (last xs))])))

(defkeyhole last* [] [::fn 'last*] (constantly [])
  :selector `(comp ~next-selector last)
  :transformer `(partial update-last ~next-transformer)
  :transformer-basis [::val last])

(defn update-butlast [f xs]
  (same-collection-type xs (concat (map f (butlast xs)) [(last xs)])))

(defkeyhole butlast* [] [::fn 'butlast*] (constantly [])
  :selector `(comp (partial map ~next-selector) butlast)
  :transformer `(partial update-butlast ~next-transformer)
  :transformer-basis [::seq butlast])

(defn update-rest [f [x & xs]]
  (same-collection-type xs (cons x (map f xs))))

(defkeyhole rest* [] [::fn 'rest*] (constantly [])
  :selector `(comp (partial map ~next-selector) rest)
  :transformer `(partial update-rest ~next-transformer)
  :transformer-basis [::seq rest])

(defn- parse-predicate [spec]
  ;; A predicate is either a symbol resolving to a predicate function
  ;; or a function literal
  (if (symbol? spec)
    [(resolve spec)]
    [spec]))

(defn fif [pred fthen felse v]
  (if (pred v)
    (fthen v)
    (felse v)))

(defkeyhole predicate [f] ::predicate parse-predicate
  :selector `(partial fif ~f ~next-selector (constantly ::nothing))
  :transformer `(partial fif ~f ~next-transformer identity))

(defn- parse-filter [[_ pred]]
  [pred])

(defn- ensure-list [x]
  (if (sequential? x)
    x
    (list x)))

(defn- find-changed-indexes [xs pred transformer-basis]
  (->> xs
       (map-indexed (fn [i v] (when (pred v) i)))
       (remove nil?)
       transformer-basis
       ensure-list))

(defn update-filtered [pred tf [ret-type transformer-basis] xs]
  (let [new-vals (->> xs (filter pred) tf vector)
        new-vals (if (= ret-type ::seq) (mapcat identity new-vals) new-vals)
        changed-indexes (find-changed-indexes xs pred transformer-basis)
        m (zipmap changed-indexes new-vals)]
    (same-collection-type xs (reduce (fn [acc [i v]] (assoc acc i v)) (vec xs) m))))

(defn filter* [f xs]
  (same-collection-type xs (filter f xs)))

(defkeyhole filter* [f] 'filter* parse-filter
  :selector `(comp ~next-selector (partial filter* ~f))
  :transformer `(partial update-filtered ~f ~next-selector ~next-transformer-basis))

;; (println (transform [{:a 1} {:a 2} {:a 3} {:a 4}] [rest* :a] inc))
;; (println (select [{:a 1} {:a 2} {:a 3} {:a 4}] [all* :a even?]))

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
;; => "Elapsed time: 161.945 msecs"
;; (select [{:foo 1} {:foo 2 :bar 1} {:foo 4}] [(filter* #(:bar %)) first*])
