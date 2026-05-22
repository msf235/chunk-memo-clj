(ns chunk-memo.coord.ops
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.types :as types]
            [clojure.set :as set]))

(defn axis-intersection [a b]
  (let [xs (set/intersection
            (set (axis/values a))
            (set (axis/values b)))]
    (when (seq xs)
      (axis/int-set-axis xs))))

(defn intersect-products [a b]
  (when-not (= (count (:axes a))
               (count (:axes b)))
    (throw (ex-info "rank mismatch" {})))

  (let [axes (map axis-intersection
                  (:axes a)
                  (:axes b))]
    (if (some nil? axes)
      types/empty-selection
      (types/coord-product axes))))

(defn try-union-products [a b]
  (when-not (= (count (:axes a))
               (count (:axes b)))
    (throw (ex-info "rank mismatch" {})))

  (loop [pairs (map vector (:axes a) (:axes b))
         differing 0
         out []]
    (if-let [[ax bx] (first pairs)]
      (if (= (vec (axis/values ax))
             (vec (axis/values bx)))
        (recur (rest pairs)
               differing
               (conj out ax))

        (if (> differing 0)
          nil
          (recur
           (rest pairs)
           1
           (conj out
                 (axis/int-set-axis
                  (concat (axis/values ax)
                          (axis/values bx)))))))

      (types/coord-product out))))

(defn product-overlap-axes [base remove]
  (mapv
   (fn [base-axis remove-axis]
     (set/intersection
      (set (axis/values base-axis))
      (set (axis/values remove-axis))))
   (:axes base)
   (:axes remove)))

(defn covers-product? [base overlap-axes]
  (= (mapv #(set (axis/values %)) (:axes base))
     overlap-axes))

(defn slab-products [base overlap-axes]
  (let [base-axes (:axes base)]
    (loop [i 0
           prefix []
           out []]
      (if (= i (count base-axes))
        out
        (let [base-axis (nth base-axes i)
              overlap   (nth overlap-axes i)
              remainder (remove overlap (axis/values base-axis))
              slab      (when (seq remainder)
                          (types/coord-product
                           (concat prefix
                                   [(axis/int-set-axis remainder)]
                                   (drop (inc i) base-axes))))]
          (recur (inc i)
                 (conj prefix (axis/int-set-axis overlap))
                 (cond-> out slab (conj slab))))))))

(defn difference-products [base remove]
  (let [overlap-axes (product-overlap-axes base remove)]
    (cond
      ;; No overlap at all: base - remove = base
      (some empty? overlap-axes)
      base

      ;; Remove covers all of base.
      (covers-product? base overlap-axes)
      types/empty-selection

      :else
      (let [parts (slab-products base overlap-axes)]
        (case (count parts)
          0 types/empty-selection
          1 (first parts)
          (types/simplify (apply types/coord-union parts)))))))

;; -----------------------------------------------------------------------------
;; Simplification algebra
;; -----------------------------------------------------------------------------
