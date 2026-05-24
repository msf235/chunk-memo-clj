(ns chunk-memo.coord.ops
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.types]
            [clojure.math.combinatorics :as combo])
  (:import
   [chunk_memo.coord.types
    CoordEmpty
    CoordProduct
    CoordUnion
    CoordIntersection
    CoordDifference]))

(defmulti contains-coord?
  (fn [sel _coord]
    (type sel)))

(defmulti coords type)

;; -----------------------------------------------------------------------------
;; Coord enumeration
;; -----------------------------------------------------------------------------

(defn selection-size [sel]
  (count (coords sel)))

(defn empty-coords [_sel]
  '())

(defn product-coords [sel]
  (apply combo/cartesian-product
         (map axis/axis-values (:axes sel))))

(defn union-coords [sel]
  (distinct
   (mapcat coords (:parts sel))))

(defn intersection-coords [sel]
  (let [[smallest & rest]
        (sort-by selection-size (:parts sel))]
    (filter
     (fn [coord]
       (every?
        #(contains-coord? % coord)
        rest))
     (coords smallest))))

(defn difference-coords [sel]
  (remove
   #(contains-coord? (:remove sel) %)
   (coords (:base sel))))

(defmethod coords CoordEmpty [sel]
  (empty-coords sel))

(defmethod coords CoordProduct [sel]
  (product-coords sel))

(defmethod coords CoordUnion [sel]
  (union-coords sel))

(defmethod coords CoordIntersection [sel]
  (intersection-coords sel))

(defmethod coords CoordDifference [sel]
  (difference-coords sel))

;; -----------------------------------------------------------------------------
;; Coord containment
;; -----------------------------------------------------------------------------

(defn contains-empty? [_sel _coord]
  false)

(defn contains-product? [sel coord]
  (let [axes (:axes sel)]
    (and (= (count coord) (count axes))
         (every?
          true?
          (map axis/contains-value? axes coord)))))

(defn contains-union? [sel coord]
  (some #(contains-coord? % coord) (:parts sel)))

(defn contains-intersection? [sel coord]
  (every? #(contains-coord? % coord) (:parts sel)))

(defn contains-difference? [sel coord]
  (and (contains-coord? (:base sel) coord)
       (not (contains-coord? (:remove sel) coord))))

(defmethod contains-coord? CoordEmpty [sel coord]
  (contains-empty? sel coord))

(defmethod contains-coord? CoordProduct [sel coord]
  (contains-product? sel coord))

(defmethod contains-coord? CoordUnion [sel coord]
  (contains-union? sel coord))

(defmethod contains-coord? CoordIntersection [sel coord]
  (contains-intersection? sel coord))

(defmethod contains-coord? CoordDifference [sel coord]
  (contains-difference? sel coord))
