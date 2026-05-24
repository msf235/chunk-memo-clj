(ns chunk-memo.coord-test
  (:require [chunk-memo.coord :as coord]
            [clojure.test :refer [deftest is testing]]))

(defn coord-set [selection]
  (set (coord/coords selection)))

(deftest axis-constructors-test
  (testing "integer set axes sort and deduplicate values"
    (is (= #{[1] [2] [3]}
           (coord-set (coord/coord-product [(coord/int-set-axis [3 1 2 1])])))))

  (testing "range axes include start and exclude stop"
    (let [selection (coord/coord-product [(coord/range-axis 2 5)])]
      (is (= #{[2] [3] [4]}
             (coord-set selection)))
      (is (coord/contains-coord? selection [2]))
      (is (not (coord/contains-coord? selection [5])))))

  (testing "strided axes include values by step before stop"
    (let [selection (coord/coord-product [(coord/strided-axis 1 8 3)])]
      (is (= #{[1] [4] [7]}
             (coord-set selection)))
      (is (coord/contains-coord? selection [4]))
      (is (not (coord/contains-coord? selection [5])))))

  (testing "invalid axes throw"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"axis cannot be empty"
                          (coord/int-set-axis [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"RangeAxis must be non-empty"
                          (coord/range-axis 3 3)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"step must be positive"
                          (coord/strided-axis 0 10 0)))))

(deftest coord-product-test
  (testing "products normalize axis literals and enumerate the cartesian product"
    (let [selection (coord/coord-product [[2 1 1] 4 [:stride 10 15 2]])]
      (is (= #{[1 4 10] [1 4 12] [1 4 14]
               [2 4 10] [2 4 12] [2 4 14]}
             (coord-set selection)))
      (is (coord/contains-coord? selection [2 4 12]))
      (is (not (coord/contains-coord? selection [2 4 13])))
      (is (not (coord/contains-coord? selection [2 4]))))))

(deftest coord-union-test
  (testing "unions contain and enumerate distinct coordinates from all parts"
    (let [left  (coord/coord-product [[1 2] [10]])
          right (coord/coord-product [[2 3] [10]])
          union (coord/coord-union left right)]
      (is (= #{[1 10] [2 10] [3 10]}
             (coord-set union)))
      (is (coord/contains-coord? union [1 10]))
      (is (coord/contains-coord? union [3 10]))
      (is (not (coord/contains-coord? union [4 10]))))))

(deftest coord-intersection-test
  (testing "intersections keep coordinates present in every part"
    (let [a (coord/coord-product [(coord/range-axis 0 4) [10 20]])
          b (coord/coord-product [[2 3 4] [20]])
          intersection (coord/coord-intersection a b)]
      (is (= #{[2 20] [3 20]}
             (coord-set intersection)))
      (is (coord/contains-coord? intersection [2 20]))
      (is (not (coord/contains-coord? intersection [2 10])))))

  (testing "intersections require at least one part"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"intersection requires at least one part"
                          (coord/coord-intersection)))))

(deftest coord-difference-test
  (testing "differences remove coordinates from the base selection"
    (let [base (coord/coord-product [[1 2 3] [10 20]])
          remove (coord/coord-product [[2] [20]])
          difference (coord/coord-difference base remove)]
      (is (= #{[1 10] [1 20] [2 10] [3 10] [3 20]}
             (coord-set difference)))
      (is (coord/contains-coord? difference [2 10]))
      (is (not (coord/contains-coord? difference [2 20]))))))

(deftest simplify-test
  (testing "simplify merges compatible product unions"
    (let [selection (coord/coord-union
                     (coord/coord-product [[1] [10]])
                     (coord/coord-product [[2] [10]]))
          simplified (coord/simplify selection)]
      (is (= #{[1 10] [2 10]}
             (coord-set simplified)))
      (is (coord/contains-coord? simplified [2 10]))))

  (testing "simplify reduces product intersections"
    (let [selection (coord/coord-intersection
                     (coord/coord-product [[1 2 3] [10 20]])
                     (coord/coord-product [[2 3 4] [20]]))
          simplified (coord/simplify selection)]
      (is (= #{[2 20] [3 20]}
             (coord-set simplified)))
      (is (coord/contains-coord? simplified [3 20]))))

  (testing "simplify removes product differences"
    (let [selection (coord/coord-difference
                     (coord/coord-product [[1 2] [10 20]])
                     (coord/coord-product [[2] [20]]))
          simplified (coord/simplify selection)]
      (is (= #{[1 10] [1 20] [2 10]}
             (coord-set simplified)))
      (is (not (coord/contains-coord? simplified [2 20]))))))
