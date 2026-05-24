(ns chunk-memo.layout-test
  (:require [chunk-memo.coord :as coord]
            [chunk-memo.index.selection :as index]
            [chunk-memo.layout :as layout]
            [clojure.test :refer [deftest is testing]]))

(defn selected-indices [layout-size selection]
  (->> (range layout-size)
       (filter #(index/contains-index? selection %))
       vec))

(deftest row-major-layout-test
  (testing "computes row-major metadata"
    (is (= {:shape [2 3 4]
            :strides [12 4 1]
            :size 24
            :rank 3}
           (select-keys (layout/row-major-layout [2 3 4])
                        [:shape :strides :size :rank]))))

  (testing "requires at least one positive dimension"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"shape must have at least one dimension"
                          (layout/row-major-layout [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"all dimensions must be positive"
                          (layout/row-major-layout [2 0 4])))))

(deftest coord-index-conversion-test
  (let [l (layout/row-major-layout [2 3 4])]
    (testing "converts coordinates to row-major indices"
      (is (= 0 (layout/coord->index l [0 0 0])))
      (is (= 6 (layout/coord->index l [0 1 2])))
      (is (= 23 (layout/coord->index l [1 2 3]))))

    (testing "converts row-major indices to coordinates"
      (is (= [0 0 0] (layout/index->coord l 0)))
      (is (= [0 1 2] (layout/index->coord l 6)))
      (is (= [1 2 3] (layout/index->coord l 23))))

    (testing "round trips every index in the layout"
      (doseq [i (range (:size l))]
        (is (= i (layout/coord->index l (layout/index->coord l i))))))

    (testing "validates coordinate rank and bounds"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"coord rank mismatch"
                            (layout/coord->index l [0 0])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"coord out of bounds"
                            (layout/coord->index l [0 3 0]))))

    (testing "validates index bounds"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"sweep id out of bounds"
                            (layout/index->coord l -1)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"sweep id out of bounds"
                            (layout/index->coord l 24))))))

(deftest coord-selection->index-product-test
  (let [l (layout/row-major-layout [2 3 4])]
    (testing "compiles the whole layout to one interval"
      (let [selection (coord/coord-product [(coord/range-axis 0 2)
                                            (coord/range-axis 0 3)
                                            (coord/range-axis 0 4)])
            compiled (layout/coord-selection->index l selection)]
        (is (= [[0 24]] (index/iter-intervals compiled)))
        (is (= (vec (range 24))
               (selected-indices (:size l) compiled)))))

    (testing "compiles full-prefix fixed-last-axis products to a strided index selection"
      (let [selection (coord/coord-product [(coord/range-axis 0 2)
                                            (coord/range-axis 0 3)
                                            [2]])
            compiled (layout/coord-selection->index l selection)]
        (is (index/strided-set? compiled))
        (is (= [2 6 10 14 18 22]
               (selected-indices (:size l) compiled)))))

    (testing "compiles sparse products to the matching row-major indices"
      (let [selection (coord/coord-product [[1]
                                            [0 2]
                                            [1 3]])
            compiled (layout/coord-selection->index l selection)]
        (is (= [13 15 21 23]
               (selected-indices (:size l) compiled)))))))

(deftest coord-selection->index-composite-test
  (let [l (layout/row-major-layout [3 3])]
    (testing "compiles unions"
      (let [selection (coord/coord-union
                       (coord/coord-product [[0 1] [1]])
                       (coord/coord-product [[1 2] [2]]))
            compiled (layout/coord-selection->index l selection)]
        (is (= [1 4 5 8]
               (selected-indices (:size l) compiled)))))

    (testing "compiles intersections"
      (let [selection (coord/coord-intersection
                       (coord/coord-product [(coord/range-axis 1 3)
                                             (coord/range-axis 0 3)])
                       (coord/coord-product [(coord/range-axis 0 3)
                                             [0 2]]))
            compiled (layout/coord-selection->index l selection)]
        (is (= [3 5 6 8]
               (selected-indices (:size l) compiled)))))

    (testing "compiles differences"
      (let [selection (coord/coord-difference
                       (coord/coord-product [(coord/range-axis 0 3)
                                             (coord/range-axis 0 3)])
                       (coord/coord-product [[1]
                                             (coord/range-axis 0 3)]))
            compiled (layout/coord-selection->index l selection)]
        (is (= [0 1 2 6 7 8]
               (selected-indices (:size l) compiled)))))))

(deftest validate-coord-selection-test
  (let [l (layout/row-major-layout [2 3])]
    (testing "rejects rank mismatches"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"product rank does not match layout rank"
                            (layout/coord-selection->index
                             l
                             (coord/coord-product [[0]])))))

    (testing "rejects axes outside the layout shape"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"axis bounds out of shape"
                            (layout/coord-selection->index
                             l
                             (coord/coord-product [[0 2] [0]])))))))
