(ns keyhole.core
  (:refer-clojure :rename {filter cfilter range crange}))

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

(defrecord RangeSelector [start end step]
  Selector
  (select* [this next-fn xs]
    (map next-fn (some->> xs (drop start) (take (- end start)) (take-nth step))))
  Transformer
  (transform* [this next-fn xs]
    (map* (some->> xs (drop start) (take (- end start)) (take-nth step))
          next-fn)))

(defn ->RangeSelector [[_ start end step]]
  (RangeSelector. start end (or step 1)))

(defn parse-filter [f]
  (cond
    (range-spec? f) (->RangeSelector f)
    :else f))

(defn- parse-filter-spec [spec]
  (map parse-filter spec))

(defmacro select [coll spec]
  (let [spec (partition 2 2 [identity] (parse-filter-spec spec))]
    `(some->> ~coll
              ~@(for [[s1 s2] spec]
                  `(select* ~s1 ~s2)))))

(defn- pick-transformer [spec]
  (if (some (some-fn (partial instance? RangeSelector)) spec)
    ->SeqTransformer
    ->ValTransformer))

(defmacro transform [coll spec transformer]
  (let [spec (reverse (partition 2 1 [identity] (parse-filter-spec spec)))
        transformer ((pick-transformer spec) (eval transformer))]
    `(some->> (some->> ~coll ~@(for [[s1 s2] spec]
                                 `(select* ~s1 ~s2)))
              ~@(for [[s1 s2] (conj spec [transformer identity])]
                  `(transform* ~s1 ~s2)))))

(transform [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2)] #(assoc % :bar 2))
;; (select  [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo])

(defn benchmark [iters afn]
  (time
   (dotimes [_ iters]
     (afn))))

(def DATA {:a {:b {:c 1}}})

;; (benchmark 1000000 #(get-in DATA [:a :b :c]))
;; ;; => "Elapsed time: 77.018 msecs"

;; (benchmark 1000000 #(select DATA [:a :b :c]))
;; ;; => "Elapsed time: 4143.343 msecs"

;; (benchmark 1000000 #(-> DATA :a :b :c vector))
;; ;; => "Elapsed time: 34.235 msecs"

(benchmark 1000000 #(update-in DATA [:a :b :c] inc))
;; => "Elapsed time: 1037.94 msecs"

(benchmark 1000000 #(transform DATA [:a :b :c] inc ))
;; => "Elapsed time: 4305.429 msecs"
(transform DATA [:a :b :c] inc )
(defn manual-transform [data]
  (update data
          :a
          (fn [d1]
            (update d1
                    :b
                    (fn [d2]
                      (update d2 :c inc))))))

(benchmark 1000000 #(manual-transform DATA))
;; => "Elapsed time: 161.945 msecs"
