(ns chunk-memo.coord
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.types :as sel]))

(def int-set-axis axis/int-set-axis)
(def range-axis axis/range-axis)
(def strided-axis axis/strided-axis)

(def coord-product sel/coord-product)
(def coord-union sel/coord-union)
(def coord-intersection sel/coord-intersection)
(def coord-difference sel/coord-difference)

(def contains-coord? sel/contains-coord?)
(def coords sel/coords)
(def simplify sel/simplify)
