(ns chunk-memo.bitmap-test
  (:require [chunk-memo.bitmap :as bitmap]
            [clojure.test :refer [deftest is testing]]))

(defn values [bm]
  (vec (bitmap/bitmap-values bm)))

(deftest bitmap-construction-test
  (testing "creates empty and populated bitmaps"
    (is (= [] (values (bitmap/bitmap))))
    (is (= [1 3 5] (values (bitmap/bitmap [5 1 3 1]))))
    (is (= 3 (bitmap/cardinality (bitmap/bitmap [5 1 3 1])))))

  (testing "adds single values"
    (let [bm (-> (bitmap/bitmap)
                 (bitmap/add 2)
                 (bitmap/add 8))]
      (is (bitmap/contains-value? bm 2))
      (is (bitmap/contains-value? bm 8))
      (is (not (bitmap/contains-value? bm 3)))
      (is (not (bitmap/contains-value? bm -1)))
      (is (= [2 8] (values bm)))))

  (testing "rejects negative values"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"bitmap values must be non-negative"
                          (bitmap/add (bitmap/bitmap) -1)))))

(deftest bitmap-range-test
  (testing "adds half-open ranges"
    (let [bm (-> (bitmap/bitmap [1 8])
                 (bitmap/add-range 3 7))]
      (is (= [1 3 4 5 6 8] (values bm)))
      (is (= 6 (bitmap/cardinality bm)))))

  (testing "empty ranges are no-ops"
    (let [bm (bitmap/bitmap [2 4])]
      (is (= bm (bitmap/add-range bm 3 3)))))

  (testing "validates range bounds"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"start must be <= end"
                          (bitmap/add-range (bitmap/bitmap) 5 3)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"bitmap values must be non-negative"
                          (bitmap/add-range (bitmap/bitmap) -1 3)))))

(deftest bitmap-set-ops-test
  (let [a (bitmap/bitmap [1 2 4])
        b (bitmap/bitmap [2 3 4 7])]
    (is (= [1 2 3 4 7]
           (values (bitmap/union a b))))
    (is (= [2 4]
           (values (bitmap/intersection a b))))
    (is (= [1]
           (values (bitmap/difference a b))))))

(deftest bitmap-serialization-test
  (testing "round trips serialized bitmaps"
    (let [bm (bitmap/bitmap [0 7 8 31])]
      (is (= [0 7 8 31]
             (values (bitmap/deserialize (bitmap/serialize bm)))))))

  (testing "honors explicit byte widths"
    (let [data (bitmap/serialize (bitmap/bitmap [0 15]) 3)]
      (is (= 3 (alength data)))
      (is (= [0 15]
             (values (bitmap/deserialize data)))))))
