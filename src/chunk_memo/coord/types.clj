(ns chunk-memo.coord.types
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.algebra :as algebra]
            [clojure.math.combinatorics :as combo]))

;; -----------------------------------------------------------------------------
;; Coord selection protocol
;; -----------------------------------------------------------------------------
(defprotocol CoordSelection
  (contains-coord? [sel coord])
  (coords [sel])
  (simplify [sel]))

(defn selection-size [sel]
  (count (coords sel)))

;; -----------------------------------------------------------------------------
;; Empty
;; -----------------------------------------------------------------------------

(defrecord CoordEmpty []
  CoordSelection
  (contains-coord? [_ _]
    false)

  (coords [_]
    '())

  (simplify [this]
    this))

(def empty-selection
  (->CoordEmpty))

;; -----------------------------------------------------------------------------
;; Product
;; -----------------------------------------------------------------------------

(defrecord CoordProduct [axes]
  CoordSelection
  (contains-coord? [_ coord]
    (and (= (count coord) (count axes))
         (every?
          true?
          (map axis/contains-value? axes coord))))

  (coords [_]
    (apply clojure.math.combinatorics/cartesian-product
           (map axis/values axes)))

  (simplify [this]
    this))

(defn coord-product [axes]
  (->CoordProduct
   (mapv axis/normalize-axis axes)))

; (defn product-size [^CoordProduct p]
;   (reduce * 1 (map axis/size (:axes p))))

;; -----------------------------------------------------------------------------
;; Union
;; -----------------------------------------------------------------------------

(defrecord CoordUnion [parts]
  CoordSelection
  (contains-coord? [_ coord]
    (some #(contains-coord? % coord) parts))

  (coords [_]
    (distinct
     (mapcat coords parts)))

  (simplify [_]
    (let [parts
          (->> parts
               (map simplify)
               (remove #(instance? CoordEmpty %))
               (mapcat #(if (instance? CoordUnion %)
                          (:parts %)
                          [%]))
               distinct)]

      (cond
        (empty? parts)
        empty-selection

        (= 1 (count parts))
        (first parts)

        :else
        (loop [remaining parts
               out []]

          (if-let [x (first remaining)]
            (let [[merged leftovers]
                  (reduce
                   (fn [[candidate rest] y]
                     (if-let [m (and (instance? CoordProduct candidate)
                                     (instance? CoordProduct y)
                                     (algebra/try-union-products candidate y))]
                       [m rest]
                       [candidate (conj rest y)]))
                   [x []]
                   (rest remaining))]

              (recur leftovers
                     (conj out merged)))

            (if (= 1 (count out))
              (first out)
              (->CoordUnion out))))))))

(defn coord-union [& parts]
  (->CoordUnion (vec parts)))

;; -----------------------------------------------------------------------------
;; Intersection
;; -----------------------------------------------------------------------------

(defrecord CoordIntersection [parts]
  CoordSelection
  (contains-coord? [_ coord]
    (every? #(contains-coord? % coord)
            parts))

  (coords [_]
    (let [[smallest & rest]
          (sort-by selection-size parts)]
      (filter
       (fn [coord]
         (every?
          #(contains-coord? % coord)
          rest))
       (coords smallest))))

  (simplify [_]
    (let [parts
          (->> parts
               (map simplify)
               (mapcat #(if (instance? CoordIntersection %)
                          (:parts %)
                          [%])))

          products (filter #(instance? CoordProduct %) parts)
          others   (remove #(instance? CoordProduct %) parts)]

      (if (some #(instance? CoordEmpty %) parts)
        empty-selection

        (let [product
              (when (seq products)
                (reduce algebra/intersect-products products))

              final-parts
              (cond-> (vec others)
                product (conj product))]

          (cond
            (some #(instance? CoordEmpty %) final-parts)
            empty-selection

            (= 1 (count final-parts))
            (first final-parts)

            :else
            (->CoordIntersection final-parts)))))))

(defn coord-intersection [& parts]
  (when (empty? parts)
    (throw (ex-info "intersection requires at least one part" {})))

  (->CoordIntersection (vec parts)))

;; -----------------------------------------------------------------------------
;; Difference
;; -----------------------------------------------------------------------------

(defrecord CoordDifference [base remove]
  CoordSelection
  (contains-coord? [_ coord]
    (and (contains-coord? base coord)
         (not (contains-coord? remove coord))))

  (coords [_]
    (remove
     #(contains-coord? remove %)
     (coords base)))

  (simplify [_]
    (let [base   (simplify base)
          remove (simplify remove)]

      (cond
        (instance? CoordEmpty base)
        empty-selection

        (instance? CoordEmpty remove)
        base

        (= base remove)
        empty-selection

        (and (instance? CoordProduct base)
             (instance? CoordProduct remove))
        (simplify
         (algebra/difference-products base remove))

        (instance? CoordUnion base)
        (simplify
         (apply coord-union
                (map #(->CoordDifference % remove)
                     (:parts base))))

        :else
        (->CoordDifference base remove)))))

(defn coord-difference [base remove]
  (->CoordDifference base remove))

(defn coord-product? [x]
  (instance? CoordProduct x))

(defn coord-union? [x]
  (instance? CoordUnion x))

(defn coord-empty? [x]
  (instance? CoordEmpty x))
