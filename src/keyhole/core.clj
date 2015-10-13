(ns keyhole.core
  (:require [clojure.walk :as walk]))

(defprotocol TypePreservingMapper
  (map* [coll f] "Map f over coll, preserving coll's type."))

(extend-protocol TypePreservingMapper

  clojure.lang.APersistentVector
  (map* [v f] (mapv f v))

  clojure.lang.Sequential
  (map* [v f] (map f v))

  clojure.lang.APersistentSet
  (map* [v f] (into #{} (map f v))))

(defprotocol Selector
  (select* [this next-fn structure] "Select value(s) in structure."))

(defprotocol Transformer
  (transform* [this next-fn structure] "Transform value(s) in structure."))

(defrecord SeqTransformer [f]
  Transformer
  (transform* [this next-fn structure] (map* structure f))
  clojure.lang.IFn
  (invoke [this structure] (map* structure f)))

(defrecord ValTransformer [f]
  Transformer
  (transform* [this next-fn v] (f v))
  clojure.lang.IFn
  (invoke [this arg] (f arg)))

(defn- range-spec?
  [spec]
  (and (sequential? spec) (= (first spec) 'range)))

(extend-protocol Transformer

  clojure.lang.Keyword
  (transform* [k next-fn m] (assoc m k (next-fn (get m k))))

  java.lang.Long
  (transform* [i next-fn v] (next-fn (assoc v i))))

(extend-protocol Selector

  clojure.lang.Keyword
  (select* [k next-fn m] (next-fn (get m k)))

  java.lang.Long
  (select* [i next-fn v] (next-fn (get v i)))

  clojure.lang.ISeq
  (filter [v spec] (throw (ex-info "Not implememented" {:spec spec :v v}))))

(defprotocol TypePreservingMapper
  (map* [coll f] "Map f over coll, preserving coll's type."))

(extend-protocol TypePreservingMapper

  clojure.lang.APersistentVector
  (map* [v f] (mapv f v))

  clojure.lang.Sequential
  (map* [v f] (map f v))

  clojure.lang.APersistentSet
  (map* [v f] (into #{} (map f v))))

(defprotocol Transformer
  (emit [this] "Emit transformer code."))

(extend-protocol Transformer

  clojure.lang.Keyword
  (emit [this] `(partial update* ::next-fn ~this)))

(defrecord RangeSpec [start end step]
  Transformer
  (emit [this] `(partial update-seq ::next-fn ~start ~end ~step)))

(defn make-RangeSpec [[_ start end step]]
  (RangeSpec. start end (or step 1)))

(defn parse-filter [f]
  (cond
    (range-spec? f) (make-RangeSpec f)
    :else f))

(defn- parse-filter-spec [spec]
  (map parse-filter spec))

;; (transform [{:foo [1 2 3]} {:foo [4 5 6]} {:foo [7 8 9]} {:foo [10 11 12]}]
;;            [(range 0 2) :foo (range 2 3)] inc)
;; (select  [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo])

(defn- slice
  "Extract the elements between start and end (exclusive) by step.

  (slice [1 2 3 4] 0 3 2) => [1 3]

  The type of xs is preserved."
  [xs start end step]
  (let [xs' (some->> xs (drop start) (take (- end start)) (take-nth step))]
    (if (vector? xs)
      (into [] xs')
      xs')))

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
    (if (vector? xs)
      (into [] res)
      res)))

(defn- update-seq
  "Apply f to every step element of xs between start and end (exclusive.)"
  [f start end step xs]
  (splice xs (map f (slice xs start end step)) start end step))

(defn update* [f k m]
  (let [m' (update {} k (constantly (f (get m k))))]
    (merge m m')))

(defn- combine [forms form]
  (walk/postwalk (fn [f] (if (= f ::next-fn) form f)) forms))

(defn- make-transformer [coll spec transformer]
  (let [forms (reverse (conj (map emit spec) transformer))]
    (reduce combine (list (first forms) coll)
            (rest forms))))

(defmacro transform [coll spec transformer]
  (let [spec (reverse (parse-filter-spec spec))]
    (make-transformer coll spec (eval transformer))))

(defn benchmark [iters afn]
  (time
   (dotimes [_ iters]
     (afn))))

(transform [{:foo [1 2 3]} {:foo [4 5 6]} {:foo [7 8 9]} {:foo [10 11 12]}]
           [(range 0 2) :foo (range 2 3)] inc)

(def DATA {:a {:b {:c 1}}})
;; (transform DATA [:a :b :c] inc )
;; (benchmark 1000000 #(get-in DATA [:a :b :c]))
;; ;; => "Elapsed time: 77.018 msecs"

;; (benchmark 1000000 #(select DATA [:a :b :c]))
;; ;; => "Elapsed time: 4143.343 msecs"

;; (benchmark 1000000 #(-> DATA :a :b :c vector))
;; ;; => "Elapsed time: 34.235 msecs"

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
