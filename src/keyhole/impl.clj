(ns keyhole.impl
  (:require [keyhole
             [macros :as macros]
             [util :as util]]))

(defn slice
  "Extract the elements between start and end (exclusive) by step.

  (slice [1 2 3 4] 0 3 2) => [1 3]

  The type of xs is preserved."
  [start end step xs]
  (let [xs' (some->> xs (drop start) (take (- end start)) (take-nth step))]
    (util/same-collection-type xs xs')))

(defn select-slice
  "Apply f to the slice created by start end and step on xs."
  [start end step f xs]
  (f (slice start end step xs)))

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
    (util/same-collection-type xs res)))

(defn update-slice
  "Apply f to the slice of xs created with start end and step."
  [f start end step xs]
  (splice xs (f (slice start end step xs)) start end step))

(defn update* [f k m]
  (update m k f))

(defn- range-parser [[_ start end step]]
  [start end (or step 1)])

(macros/defkeyhole range* [start end step] 'range* range-parser
  :selector `(partial select-slice ~start ~end ~step ~next-selector)
  :transformer `(partial update-slice ~next-transformer ~start ~end ~step)
  :transformer-basis [::seq `(partial slice ~start ~end ~step)])

(defn- kw-parser [spec]
  (if (sequential? spec)
    (list (second spec))
    (list spec)))

(macros/defkeyhole kw [k] 'key kw-parser
  :selector `(comp ~next-selector (fn [m#] (get m# ~k)))
  :transformer `(partial update* ~next-transformer ~k))

(defn update-all [f xs]
  (util/same-collection-type xs (map f xs)))

(macros/defkeyhole all* [] [:keyhole.parser/fn 'all*] (constantly [])
  :selector `(partial mapcat ~next-selector)
  :transformer `(partial update-all ~next-transformer)
  :transformer-basis [::seq identity])

(defn update-first [f [x & xs]]
  (util/same-collection-type xs (cons (f x) xs)))

(macros/defkeyhole first* [] [:keyhole.parser/fn 'first*] (constantly [])
  :selector `(comp ~next-selector first)
  :transformer `(partial update-first ~next-transformer)
  :transformer-basis [::val first])

(defn update-last [f xs]
  (util/same-collection-type xs (concat (butlast xs) [(f (last xs))])))

(macros/defkeyhole last* [] [:keyhole.parser/fn 'last*] (constantly [])
  :selector `(comp ~next-selector last)
  :transformer `(partial update-last ~next-transformer)
  :transformer-basis [::val last])

(defn update-butlast [f xs]
  (util/same-collection-type xs (concat (map f (butlast xs)) [(last xs)])))

(macros/defkeyhole butlast* [] [:keyhole.parser/fn 'butlast*] (constantly [])
  :selector `(comp (partial mapcat ~next-selector) butlast)
  :transformer `(partial update-butlast ~next-transformer)
  :transformer-basis [::seq butlast])

(defn update-rest [f [x & xs]]
  (util/same-collection-type xs (cons x (map f xs))))

(macros/defkeyhole rest* [] [:keyhole.parser/fn 'rest*] (constantly [])
  :selector `(comp (partial mapcat ~next-selector) rest)
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

(macros/defkeyhole predicate [f] :keyhole.parser/predicate parse-predicate
  :selector `(partial fif ~f ~next-selector (constantly [::nothing]))
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
  (let [new-vals (->> xs (filter pred) tf transformer-basis vector)
        new-vals (if (= ret-type :keyhole.parser/seq) (mapcat identity new-vals) new-vals)
        changed-indexes (find-changed-indexes xs pred transformer-basis)
        m (zipmap changed-indexes new-vals)]
    (util/same-collection-type xs (reduce (fn [acc [i v]] (assoc acc i v)) (vec xs) m))))

(defn filter* [f xs]
  (util/same-collection-type xs (filter f xs)))

(macros/defkeyhole filter* [f] 'filter* parse-filter
  :selector `(comp ~next-selector (partial filter* ~f))
  :transformer `(partial update-filtered ~f ~next-transformer ~next-transformer-basis))

(defn- parse-nth [[_ n]]
  [n])

(defn update-nth [n transformer coll]
  (let [c (into [] coll)]
    (util/same-collection-type coll (assoc c n (transformer (get c n))))))

(macros/defkeyhole nth* [n] 'nth* parse-nth
  :selector `(comp ~next-selector (fn [coll#] (nth coll# ~n)))
  :transformer `(partial update-nth ~n ~next-transformer)
  :transformer-basis [::val nth])

(defn- parse-nthrest [[_ n]]
  [n])

(defn update-nthrest [n f xs]
  (util/same-collection-type xs (into (vec (take n xs)) (map f (nthrest xs n)))))

(macros/defkeyhole nthrest* [n] 'nthrest* parse-nthrest
  :selector `(comp ~next-selector (fn [coll#] (nthrest coll# ~n)))
  :transformer `(partial update-nthrest ~n ~next-transformer)
  :transformer-basis [::seq nth])

(defn remove-sentinels [res]
  (cond
    (sequential? res) (remove #{::nothing} res)
    (= res ::nothing) nil
    :else res))
