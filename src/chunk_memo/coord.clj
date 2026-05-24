(ns chunk-memo.coord
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.ops :as ops]
            [chunk-memo.coord.types :as types]))

(def int-set-axis axis/int-set-axis)
(def range-axis axis/range-axis)
(def strided-axis axis/strided-axis)

(def coord-product types/coord-product)
(def coord-union types/coord-union)
(def coord-intersection types/coord-intersection)
(def coord-difference types/coord-difference)

(def contains-coord? ops/contains-coord?)
(def coords ops/coords)
(def simplify ops/simplify)
