(ns keyhole.core
  (:require [clojure.walk :as walk]))

(defn- range-spec?
  [spec]
  (and (sequential? spec) (= (first spec) 'range)))

(defprotocol Transformer
  (transformer [this] "Emit transformer code."))

(extend-protocol Transformer

  clojure.lang.Keyword
  (transformer [this] `(partial update* ::next-fn ~this)))

(defprotocol Selector
  (selector [this] "Emit selector code."))

(extend-protocol Selector

  clojure.lang.Keyword
  (selector [this] `(comp ::next-fn ~this)))

(defrecord RangeSpec [start end step]
  Transformer
  (transformer [this] `(partial update-seq ::next-fn ~start ~end ~step))
  Selector
  (selector [this] `(partial update-slice ~start ~end ~step ::next-fn)))

(defn make-RangeSpec [[_ start end step]]
  (RangeSpec. start end (or step 1)))

(defn parse-dispatcher
  "Return the function name of the spec or its type.

  :foo => clojure.lang.keyword
  (range 0 2) => range"
  [spec]
  (if (sequential? spec)
    (first spec)
    (type spec)))

(defmulti parse "Parse a spec." #'parse-dispatcher)

(defmethod parse 'range [spec] (make-RangeSpec spec))

(defmethod parse clojure.lang.Keyword [spec] spec)

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

(defn- update-slice [start end step f xs]
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

(defn- update-seq
  "Apply f to every step element of xs between start and end (exclusive.)"
  [f start end step xs]
  (splice xs (map f (slice start end step xs)) start end step))

(defn update* [f k m]
  (update m k f))

(defn- combine [forms form]
  (walk/postwalk (fn [f] (if (= f ::next-fn) form f)) forms))

(defn- combine-forms
  [forms coll f]
  (let [forms (conj forms f)]
    (reduce combine (list (first forms) coll) (rest forms))))

(defmacro transform [coll spec f]
  (let [spec (parse-spec spec)
        transformer-forms (mapv transformer spec)]
    (combine-forms transformer-forms coll (eval f))))

(defn benchmark [iters afn]
  (time
   (dotimes [_ iters]
     (afn))))

(defmacro select [coll spec]
  (let [spec (parse-spec spec)
        selector-forms (mapv selector spec)]
    (combine-forms selector-forms coll identity)))

(println
 (select  [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo]))

(println
 (transform [{:foo [1 2 3]} {:foo [4 5 6]} {:foo [7 8 9]} {:foo [10 11 12]}]
            [(range 0 2) :foo (range 2 3)] inc))

;; (def DATA {:a {:b {:c 1}}})


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
