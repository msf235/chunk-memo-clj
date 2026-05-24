(ns chunk-memo.params-test
  (:require [chunk-memo.coord :as coord]
            [chunk-memo.index.selection :as index]
            [chunk-memo.params :as params]
            [clojure.test :refer [deftest is testing]]))

(defn coord-set [selection]
  (set (coord/coords selection)))

(defn selected-indices [space selection]
  (->> (range (get-in space [:layout :size]))
       (filter #(index/contains-index? (params/to-index space selection) %))
       vec))

(deftest param-axis-test
  (testing "creates semantic axes"
    (let [axis (params/param-axis :x 10 14)]
      (is (= 4 (params/axis-size axis)))
      (is (= 0 (params/value->pos axis 10)))
      (is (= 3 (params/value->pos axis 13)))))

  (testing "validates axes and values"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"parameter axis must be non-empty"
                          (params/param-axis :x 2 2)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"parameter value outside axis bounds"
                          (params/value->pos (params/param-axis :x 10 14) 14)))))

(deftest axis->pos-test
  (let [axis (params/param-axis :x 10 16)]
    (testing "translates supported semantic axis specs"
      (is (= #{[2]}
             (coord-set (coord/coord-product [(params/axis->pos axis 12)]))))
      (is (= #{[1] [2] [3]}
             (coord-set (coord/coord-product [(params/axis->pos axis [11 14])]))))
      (is (= #{[1] [2] [3]}
             (coord-set (coord/coord-product [(params/axis->pos axis [:range 11 14])]))))
      (is (= #{[0] [2] [4]}
             (coord-set (coord/coord-product [(params/axis->pos axis [:stride 10 16 2])]))))
      (is (= #{[1] [3] [5]}
             (coord-set (coord/coord-product [(params/axis->pos axis {:start 11 :stop 16 :step 2})]))))
      (is (= #{[0] [3] [5]}
             (coord-set (coord/coord-product [(params/axis->pos axis '(10 13 15))])))))

    (testing "validates semantic axis specs"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"range selection must be non-empty"
                            (params/axis->pos axis [12 12])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"step must be positive"
                            (params/axis->pos axis [:stride 10 16 0])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"map axis spec requires :start and :stop"
                            (params/axis->pos axis {:start 10})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Cannot translate axis spec"
                            (params/axis->pos axis :bad))))))

(deftest parameter-space-test
  (let [space (params/run-parameter-space [(params/param-axis :x 10 13)
                                           (params/param-axis :y 20 22)])]
    (testing "builds a row-major layout from axis sizes"
      (is (= [3 2] (get-in space [:layout :shape])))
      (is (= 6 (get-in space [:layout :size]))))

    (testing "creates products and points from semantic values"
      (is (= #{[0 0] [0 1] [1 0] [1 1]}
             (coord-set (params/product space [[10 12] [20 22]]))))
      (is (= #{[2 1]}
             (coord-set (params/point space [12 21])))))

    (testing "creates axis ranges with fixed values"
      (is (= #{[0 1] [1 1] [2 1]}
             (coord-set (params/axis-range space :x 10 13 {:y 21})))))

    (testing "compiles parameter selections to layout indices"
      (is (= [1 3 5]
             (selected-indices space (params/axis-range space :x 10 13 {:y 21})))))

    (testing "combines selections"
      (let [left  (params/product space [[10 12] 20])
            right (params/product space [11 [20 22]])]
        (is (= #{[0 0] [1 0] [1 1]}
               (coord-set (params/union left right))))
        (is (= #{[1 0]}
               (coord-set (params/intersection left right))))
        (is (= #{[0 0]}
               (coord-set (params/difference left right)))))))

  (testing "validates spaces and selection rank"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"parameter space requires at least one axis"
                          (params/run-parameter-space [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"wrong rank"
                          (params/product
                           (params/run-parameter-space [(params/param-axis :x 0 2)])
                           [0 1])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing fixed value for axis"
                          (params/axis-range
                           (params/run-parameter-space [(params/param-axis :x 0 2)
                                                        (params/param-axis :y 0 2)])
                           :x 0 2 {})))))
