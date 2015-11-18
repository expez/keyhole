(ns keyhole.parser
  (:require [keyhole.protocols :as protocols]))

(defn parse-dispatcher
  ":foo => 'key
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
      (keyword? spec) 'key
      (symbol? spec) [::fn spec]
      :else spec)))

(defmulti parse "Parse a spec." #'parse-dispatcher)

(defmethod parse :default [spec]
  (throw (ex-info "Uknown spec" {:spec spec})))

(defn- link-steps [spec]
  (let [[fin & spec] (reverse spec)]
    (reduce (fn [n s]
              (merge s {:next-transformer (protocols/transformer n)
                        :next-transformer-basis (protocols/transformer-basis n)
                        :next-selector (protocols/selector n)}))
            fin
            spec)))

(defn- spec-type [step]
  (letfn [(spec-type [n] (if (.endsWith (str n) "*") ::seq ::val))]
    (if (sequential? step)
      (spec-type (first step))
      (spec-type step))))

;; Wrapper for the final selector / transformer.
(defrecord Fin [f args t]
  protocols/Transformer
  (protocols/transformer [this] `(fn [v#] (~f v# ~@args)))
  (protocols/transformer-basis [this] [t `(fn [v#] (~f v# ~@args))])
  protocols/Selector
  (protocols/selector [this] `(comp list ~f)))

(defn parse-spec [spec transformer args]
  (link-steps (conj (mapv parse spec)
                    (Fin. transformer args (spec-type (last spec))))))
