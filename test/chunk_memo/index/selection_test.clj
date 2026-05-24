(ns chunk-memo.index.selection-test
  (:require [chunk-memo.index.selection :as selection]
            [clojure.test :refer [deftest is testing]]))

(deftest empty-set-test
  (testing "empty sets contain no indices and only cover empty ranges"
    (is (selection/empty-set? selection/empty-set))
    (is (not (selection/contains-index? selection/empty-set 0)))
    (is (= [] (selection/intersect-range selection/empty-set 0 10)))
    (is (nil? (selection/bounds selection/empty-set)))
    (is (= [] (selection/iter-intervals selection/empty-set)))
    (is (selection/covers-range? selection/empty-set 3 3))
    (is (not (selection/covers-range? selection/empty-set 3 4)))
    (is (= 0 (selection/count-between selection/empty-set 0 10)))
    (is (= 0 (selection/rank-between selection/empty-set 0 10)))))

(deftest interval-set-test
  (testing "interval sets normalize unordered and overlapping half-open ranges"
    (let [sel (selection/interval-set [[5 8] [1 3] [3 5] [10 10] [7 9]])]
      (is (selection/interval-set? sel))
      (is (= [[1 9]] (selection/iter-intervals sel)))
      (is (= [1 9] (selection/bounds sel)))
      (is (selection/contains-index? sel 1))
      (is (selection/contains-index? sel 8))
      (is (not (selection/contains-index? sel 9)))
      (is (= [[2 6]] (selection/intersect-range sel 2 6)))
      (is (selection/covers-range? sel 2 6))
      (is (not (selection/covers-range? sel 0 2)))
      (is (= 5 (selection/count-between sel 2 7)))
      (is (= 3 (selection/rank-between sel 2 5)))))

  (testing "interval helpers add ranges and points"
    (let [sel (-> (selection/interval-set [[1 3]])
                  (selection/with-range 5 7)
                  (selection/with-point 4))]
      (is (= [[1 3] [4 7]] (selection/iter-intervals sel)))))

  (testing "interval sets validate ranges and simplify empty interval records"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"start must be <= end"
                          (selection/interval-set [[5 4]])))
    (is (selection/empty-set? (selection/simplify (selection/interval-set))))))

(deftest strided-set-test
  (let [sel (selection/strided-set 2 10 3)]
    (testing "strided sets expose sparse points as one-wide intervals"
      (is (selection/strided-set? sel))
      (is (= [2 10] (selection/bounds sel)))
      (is (= [[2 3] [5 6] [8 9]] (selection/iter-intervals sel)))
      (is (selection/contains-index? sel 5))
      (is (not (selection/contains-index? sel 6)))
      (is (= [[5 6] [8 9]] (selection/intersect-range sel 3 9)))
      (is (selection/covers-range? sel 5 6))
      (is (not (selection/covers-range? sel 5 8)))
      (is (= 2 (selection/count-between sel 3 9)))
      (is (= 1 (selection/rank-between sel 3 6)))))

  (testing "step-one strided sets cover contiguous ranges"
    (let [sel (selection/strided-set 2 10 1)]
      (is (= [[4 8]] (selection/intersect-range sel 4 8)))
      (is (selection/covers-range? sel 4 8))))

  (testing "strided sets validate bounds and step"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"step must be positive"
                          (selection/strided-set 0 10 0)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"start must be < end"
                          (selection/strided-set 2 2 1)))))

(deftest union-set-test
  (testing "unions merge overlapping parts for range operations"
    (let [sel (selection/union-set
               (selection/interval-set [[1 4] [8 10]])
               (selection/interval-set [[3 6]])
               (selection/strided-set 12 17 2))]
      (is (selection/contains-index? sel 5))
      (is (selection/contains-index? sel 14))
      (is (not (selection/contains-index? sel 11)))
      (is (= [1 17] (selection/bounds sel)))
      (is (= [[1 6] [8 10] [12 13] [14 15] [16 17]]
             (selection/iter-intervals sel)))
      (is (= [[3 6] [8 9]] (selection/intersect-range sel 3 9)))
      (is (selection/covers-range? sel 2 5))
      (is (not (selection/covers-range? sel 2 9)))
      (is (= 9 (selection/count-between sel 0 15)))
      (is (= 4 (selection/rank-between sel 0 5)))))

  (testing "unions simplify empty and nested parts"
    (is (selection/empty-set? (selection/simplify (selection/union-set selection/empty-set))))
    (is (= [[1 5]]
           (selection/iter-intervals
            (selection/simplify
             (selection/union-set
              (selection/interval-set [[1 3]])
              (selection/union-set (selection/interval-set [[3 5]])))))))))

(deftest intersection-set-test
  (testing "intersections keep only ranges present in every part"
    (let [sel (selection/intersection-set
               (selection/interval-set [[1 8]])
               (selection/interval-set [[3 10]])
               (selection/strided-set 2 9 2))]
      (is (selection/contains-index? sel 4))
      (is (not (selection/contains-index? sel 5)))
      (is (= [3 8] (selection/bounds sel)))
      (is (= [[4 5] [6 7]] (selection/iter-intervals sel)))
      (is (= [[4 5] [6 7]] (selection/intersect-range sel 3 8)))
      (is (selection/covers-range? sel 4 5))
      (is (not (selection/covers-range? sel 4 7)))
      (is (= 2 (selection/count-between sel 0 10)))
      (is (= 1 (selection/rank-between sel 0 5)))))

  (testing "intersections require at least one part"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"IntersectionSet requires at least one part"
                          (selection/intersection-set)))))

(deftest difference-set-test
  (testing "differences remove intervals from the base selection"
    (let [sel (selection/difference-set
               (selection/interval-set [[1 10]])
               (selection/union-set
                (selection/interval-set [[3 5]])
                (selection/strided-set 7 10 2)))]
      (is (selection/contains-index? sel 2))
      (is (not (selection/contains-index? sel 3)))
      (is (not (selection/contains-index? sel 7)))
      (is (= [1 10] (selection/bounds sel)))
      (is (= [[1 3] [5 7] [8 9]] (selection/iter-intervals sel)))
      (is (= [[2 3] [5 7] [8 9]] (selection/intersect-range sel 2 9)))
      (is (selection/covers-range? sel 5 7))
      (is (not (selection/covers-range? sel 2 6)))
      (is (= 5 (selection/count-between sel 0 10)))
      (is (= 3 (selection/rank-between sel 0 6))))))
