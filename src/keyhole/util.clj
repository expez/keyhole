(ns keyhole.util)

(defn same-collection-type
  "Coerce new to the same type as old."
  [old new]
  (if (vector? old)
    (into [] new)
    new))

(defn update-first [f [x & xs]]
  (same-collection-type xs (cons (f x) xs)))
