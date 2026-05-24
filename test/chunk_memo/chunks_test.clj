(ns chunk-memo.chunks-test
  (:require [chunk-memo.bitmap :as bitmap]
            [chunk-memo.chunks :as chunks]
            [chunk-memo.index.selection :as selection]
            [clojure.test :refer [deftest is testing]]))

(defn bitmap-values [bm]
  (vec (bitmap/bitmap-values bm)))

(deftest chunk-spec-test
  (testing "validates chunk specs"
    (is (= {:chunk-size 3 :total-size 8}
           (select-keys (chunks/chunk-spec 3 8)
                        [:chunk-size :total-size])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"chunk-size must be positive"
                          (chunks/chunk-spec 0 8)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"total-size must be non-negative"
                          (chunks/chunk-spec 3 -1)))))

(deftest chunk-layout-test
  (let [spec (chunks/chunk-spec 3 8)]
    (testing "computes chunk counts, bounds, and capacities"
      (is (= 3 (chunks/num-chunks spec)))
      (is (= [0 3] (chunks/chunk-bounds spec 0)))
      (is (= [3 6] (chunks/chunk-bounds spec 1)))
      (is (= [6 8] (chunks/chunk-bounds spec 2)))
      (is (= 2 (chunks/chunk-capacity spec 2))))

    (testing "converts flat indices to chunk-local offsets"
      (is (= [0 0] (chunks/index->chunk-offset spec 0)))
      (is (= [1 2] (chunks/index->chunk-offset spec 5)))
      (is (= [2 1] (chunks/index->chunk-offset spec 7))))

    (testing "validates chunk ids and sweep ids"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"chunk-id out of bounds"
                            (chunks/chunk-bounds spec 3)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"sweep-id out of bounds"
                            (chunks/index->chunk-offset spec 8))))))

(deftest add-flat-range-test
  (let [spec (chunks/chunk-spec 3 8)]
    (testing "adds ranges across chunk boundaries"
      (let [chunk-map (chunks/add-flat-range {} 2 7 spec)]
        (is (= [2] (bitmap-values (get chunk-map 0))))
        (is (= [0 1 2] (bitmap-values (get chunk-map 1))))
        (is (= [0] (bitmap-values (get chunk-map 2))))))

    (testing "validates flat ranges"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"start must be <= end"
                            (chunks/add-flat-range {} 4 2 spec)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"range outside total size"
                            (chunks/add-flat-range {} -1 2 spec)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"range outside total size"
                            (chunks/add-flat-range {} 0 9 spec))))))

(deftest index-selection-chunks-test
  (let [spec (chunks/chunk-spec 3 8)
        sel  (selection/interval-set [[1 4] [6 8]])]
    (testing "compiles index selections to chunk bitmaps"
      (let [chunk-map (chunks/index-selection->chunk-bitmaps sel spec)]
        (is (= [1 2] (bitmap-values (get chunk-map 0))))
        (is (= [0] (bitmap-values (get chunk-map 1))))
        (is (= [0 1] (bitmap-values (get chunk-map 2))))))

    (testing "reports chunk status"
      (is (= :empty (chunks/chunk-status bitmap/empty-bitmap spec 0)))
      (is (= :partial (chunks/chunk-status (bitmap/bitmap [0 2]) spec 0)))
      (is (= :complete (chunks/chunk-status (bitmap/bitmap [0 1]) spec 2))))

    (testing "merges and extends chunk bitmap maps"
      (let [base    {0 (bitmap/bitmap [0])}
            update  {0 (bitmap/bitmap [2]) 1 (bitmap/bitmap [0])}
            merged  (chunks/merge-chunk-bitmaps base update)
            extended (chunks/add-index-selection base sel spec)]
        (is (= [0 2] (bitmap-values (get merged 0))))
        (is (= [0] (bitmap-values (get merged 1))))
        (is (= [0 1 2] (bitmap-values (get extended 0))))
        (is (= [0] (bitmap-values (get extended 1))))
        (is (= [0 1] (bitmap-values (get extended 2))))))))
