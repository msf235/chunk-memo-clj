(ns chunk-memo.index
  (:require [chunk-memo.index.selection :as sel]))

(def empty-set sel/empty-set)
(def interval-set sel/interval-set)
(def strided-set sel/strided-set)
(def union-set sel/union-set)
(def intersection-set sel/intersection-set)
(def difference-set sel/difference-set)

(def contains-index? sel/contains-index?)
(def intersect-range sel/intersect-range)
(def bounds sel/bounds)
(def iter-intervals sel/iter-intervals)
(def covers-range? sel/covers-range?)
(def count-between sel/count-between)
(def rank-between sel/rank-between)
(def simplify sel/simplify)
