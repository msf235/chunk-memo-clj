(ns chunk-memo.cache-test
  (:refer-clojure :exclude [contains?])
  (:require [chunk-memo.bitmap :as bitmap]
            [chunk-memo.cache :as cache]
            [chunk-memo.coord :as coord]
            [chunk-memo.params :as params]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory "chunk-memo-cache-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn delete-recursive! [file]
  (when (.exists (io/file file))
    (doseq [f (reverse (file-seq (io/file file)))]
      (.delete f))))

(defn test-space []
  (params/run-parameter-space [(params/param-axis :x 10 13)
                               (params/param-axis :y 20 22)]))

(deftest chunk-cache-construction-test
  (let [root (temp-dir)]
    (try
      (let [c (cache/chunk-cache root (test-space) 3)]
        (testing "creates cache directories and metadata"
          (is (.isDirectory (:index-root c)))
          (is (.isDirectory (:payload-root c)))
          (is (.exists (io/file root "meta.json")))
          (is (= 3 (:chunk-size c))))

        (testing "builds deterministic paths"
          (is (= "chunk_00000000000000000001.bin"
                 (.getName (cache/chunk-path c 1))))
          (is (= "x=11__y=21.dat"
                 (.getName (cache/result-path c [11 21] ".dat"))))))
      (finally
        (delete-recursive! root)))))

(deftest cache-coordinate-translation-test
  (let [root (temp-dir)]
    (try
      (let [c (cache/chunk-cache root (test-space) 3)]
        (is (= [1 1] (cache/params->pos c [11 21])))
        (is (= 3 (cache/params->index c [11 21])))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"wrong parameter rank"
                              (cache/params->pos c [11]))))
      (finally
        (delete-recursive! root)))))

(deftest cache-marking-test
  (let [root (temp-dir)]
    (try
      (let [c (cache/chunk-cache root (test-space) 3)]
        (testing "missing chunks load as empty and can be stored"
          (is (= 0 (bitmap/cardinality (cache/load-chunk c 0))))
          (cache/store-chunk! c 0 (bitmap/bitmap [1 2]))
          (is (= [1 2] (vec (bitmap/bitmap-values (cache/load-chunk c 0))))))

        (testing "marks and queries individual parameter points"
          (cache/store-chunk! c 0 bitmap/empty-bitmap)
          (is (not (cache/contains? c [10 20])))
          (cache/mark-complete! c [10 20])
          (is (cache/contains? c [10 20]))
          (is (= :partial (cache/status-for-chunk c 0))))

        (testing "marks and queries coordinate selections across chunks"
          (let [selection (params/axis-range (:space c) :x 10 13 {:y 21})]
            (is (not (cache/contains-selection? c selection)))
            (cache/mark-selection-complete! c selection)
            (is (cache/contains-selection? c selection))
            (is (= [] (cache/missing c [[10 20] [10 21] [11 21]]))))))
      (finally
        (delete-recursive! root)))))

(deftest cache-selection-completeness-test
  (let [root (temp-dir)]
    (try
      (let [c (cache/chunk-cache root (test-space) 3)]
        (testing "requires every point in a selection to be complete"
          (cache/mark-selection-complete!
           c
           (coord/coord-product [[0 1] [0]]))
          (is (cache/contains-selection? c (coord/coord-product [[0 1] [0]])))
          (is (not (cache/contains-selection? c (coord/coord-product [[0 1] [0 1]]))))))
      (finally
        (delete-recursive! root)))))
