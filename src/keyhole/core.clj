(ns keyhole.core
  (:refer-clojure :rename {filter cfilter range crange}))

(defprotocol Filter
  (filter [this spec] "Apply the filter spec to produce a value.")
  (create-breadcrumb [this spec] "Create a breadcrumb to recreate a
  transformed datastructure."))

(defprotocol Combine
  "Combine is used to merge a Breadcrumb with some value when
  rebuilding a datastructure which has undergone keyhole surgery."
  (combine [breadcrumb v] "Combine a Breadcrumb and a value to rebuild
  a datastructure."))

(defrecord Breadcrumb [v spec combinator]
  Combine
  (combine [this t] (combinator v spec t)))

(defn- range-spec?
  [spec]
  (and (sequential? spec) (= (first spec) 'range)))

(defn- vector-combinator [v spec t]
  (vec
   (concat (subvec v 0 (nth spec 1))
           t
           (subvec v (nth spec 2)))))

(extend-protocol Filter

  clojure.lang.APersistentMap
  (filter [m k] (get m k))
  (create-breadcrumb [m k] (Breadcrumb. m k assoc))

  clojure.lang.APersistentVector
  (filter [v spec] (cond
                     (integer? spec) (get v spec)
                     (range-spec? spec) (subvec v (nth spec 1) (nth spec 2))))
  (create-breadcrumb [v spec] (cond
                                (integer? spec) (partial assoc v spec)
                                (range-spec? spec)
                                (Breadcrumb. v spec vector-combinator)))

  clojure.lang.ISeq
  (filter [v spec] (throw (ex-info "Not implememented" {:spec spec :v v})))
  (create-breadcrumb [v spec] (throw (ex-info "Not implememented" {:spec spec :v v}))))

(defn- parse-filter-spec [spec]
  (for [f spec]
    (cond
      (keyword? f) f
      (range-spec? f) (conj (next f) 'range)
      :else (IllegalArgumentException. (str "Unknown filter: " f)))))

(defn rebuild [acc breadcrumb]
  (combine breadcrumb acc))

(defn- create-breadcrumbs [values spec]
  (vec (reverse (map (fn [v f] (create-breadcrumb v f)) values spec))))

(defn- transform-last-value [transformer vs]
  (let [v (last vs)
        vs (pop vs)
        transformed (vec (if (sequential? v) (map transformer v) (transformer v)))]
    (conj vs transformed)))

(defn- filter-results
  [{:keys [v vs one-to-many?]} f]
  (let [v (if one-to-many?
            (for [e v] (filter e f))
            (filter v f))]
    {:v v
     :vs (conj vs v)
     :one-to-many? (range-spec? f)}))

(defn -select [coll spec]
  (:v (reduce filter-results {:v coll :one-to-many? false :vs []} spec)))

(defn- all-filter-results [coll spec]
  (:vs (reduce filter-results {:v coll :one-to-many? false :vs [coll]} spec)))

(defmacro select [coll spec]
  (let [spec (parse-filter-spec spec)]
    `(-select ~coll '~spec)))

(defmacro transform [coll spec transformer]
  (let [spec (parse-filter-spec spec)
        values (all-filter-results coll spec)
        breadcrumbs (create-breadcrumbs values spec)]
    `(reduce rebuild ~(transformer (last values)) ~breadcrumbs)))

(transform [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo] identity)
;; (select  [{:foo 1} {:foo 2} {:foo 3} {:foo 4}] [(range 0 2) :foo])
