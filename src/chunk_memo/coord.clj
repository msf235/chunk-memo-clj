(ns chunk-memo.coord
  (:require [chunk-memo.coord.axis :as axis]
            [chunk-memo.coord.ops :as ops]
            [chunk-memo.coord.simplify :as simplify]
            [chunk-memo.coord.types :as types]))

(def int-set-axis axis/int-set-axis)
(def range-axis axis/range-axis)
(def strided-axis axis/strided-axis)

(def axis-values axis/axis-values)
(def axis-size axis/axis-size)
(def min-value axis/min-value)
(def max-value axis/max-value)

(def coord-product types/coord-product)
(def coord-union types/coord-union)
(def coord-intersection types/coord-intersection)
(def coord-difference types/coord-difference)

(def coord-empty? types/coord-empty?)
(def coord-product? types/coord-product?)
(def coord-union? types/coord-union?)
(def coord-intersection? types/coord-intersection?)
(def coord-difference? types/coord-difference?)

(def axes types/axes)
(def parts types/parts)
(def difference-base types/difference-base)
(def difference-remove types/difference-remove)

(def contains-coord? ops/contains-coord?)
(def coords ops/coords)
(def simplify simplify/simplify)
