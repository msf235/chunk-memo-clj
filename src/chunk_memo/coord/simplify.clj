(ns chunk-memo.coord.simplify
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.types :as types]
            [clojure.set :as set])
  (:import
   [chunk_memo.coord.types
    CoordEmpty
    CoordProduct
    CoordUnion
    CoordIntersection
    CoordDifference]))

(defmulti simplify type)

(defn axis-intersection [a b]
  (let [xs (set/intersection
            (set (axis/axis-values a))
            (set (axis/axis-values b)))]
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
      (if (= (vec (axis/axis-values ax))
             (vec (axis/axis-values bx)))
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
                  (concat (axis/axis-values ax)
                          (axis/axis-values bx)))))))

      (types/coord-product out))))

(defn product-overlap-axes [base remove]
  (mapv
   (fn [base-axis remove-axis]
     (set/intersection
      (set (axis/axis-values base-axis))
      (set (axis/axis-values remove-axis))))
   (:axes base)
   (:axes remove)))

(defn covers-product? [base overlap-axes]
  (= (mapv #(set (axis/axis-values %)) (:axes base))
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
              remainder (remove overlap (axis/axis-values base-axis))
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
          (simplify (apply types/coord-union parts)))))))

(defn simplify-empty [sel]
  sel)

(defn simplify-product [sel]
  sel)

(defn simplify-union [sel]
  (let [parts
        (->> (:parts sel)
             (map simplify)
             (remove types/coord-empty?)
             (mapcat #(if (types/coord-union? %)
                        (:parts %)
                        [%]))
             distinct)]

    (cond
      (empty? parts)
      types/empty-selection

      (= 1 (count parts))
      (first parts)

      :else
      (loop [remaining parts
             out []]

        (if-let [x (first remaining)]
          (let [[merged leftovers]
                (reduce
                 (fn [[candidate rest] y]
                   (if-let [m (and (types/coord-product? candidate)
                                   (types/coord-product? y)
                                   (try-union-products candidate y))]
                     [m rest]
                     [candidate (conj rest y)]))
                 [x []]
                 (rest remaining))]

            (recur leftovers
                   (conj out merged)))

          (if (= 1 (count out))
            (first out)
            (types/->CoordUnion out)))))))

(defn simplify-intersection [sel]
  (let [parts
        (->> (:parts sel)
             (map simplify)
             (mapcat #(if (instance? CoordIntersection %)
                        (:parts %)
                        [%])))

        products (filter types/coord-product? parts)
        others   (remove types/coord-product? parts)]

    (if (some types/coord-empty? parts)
      types/empty-selection

      (let [product
            (when (seq products)
              (reduce intersect-products products))

            final-parts
            (cond-> (vec others)
              product (conj product))]

        (cond
          (some types/coord-empty? final-parts)
          types/empty-selection

          (= 1 (count final-parts))
          (first final-parts)

          :else
          (types/->CoordIntersection final-parts))))))

(defn simplify-difference [sel]
  (let [base   (simplify (:base sel))
        remove (simplify (:remove sel))]

    (cond
      (types/coord-empty? base)
      types/empty-selection

      (types/coord-empty? remove)
      base

      (= base remove)
      types/empty-selection

      (and (types/coord-product? base)
           (types/coord-product? remove))
      (simplify
       (difference-products base remove))

      (types/coord-union? base)
      (simplify
       (apply types/coord-union
              (map #(types/->CoordDifference % remove)
                   (:parts base))))

      :else
      (types/->CoordDifference base remove))))

(defmethod simplify CoordEmpty [sel]
  (simplify-empty sel))

(defmethod simplify CoordProduct [sel]
  (simplify-product sel))

(defmethod simplify CoordUnion [sel]
  (simplify-union sel))

(defmethod simplify CoordIntersection [sel]
  (simplify-intersection sel))

(defmethod simplify CoordDifference [sel]
  (simplify-difference sel))
