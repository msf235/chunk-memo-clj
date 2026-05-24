(ns chunk-memo.memo-test
  (:refer-clojure :exclude [memoize])
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.memo :as memo]
            [chunk-memo.params :as params]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory "chunk-memo-memo-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn delete-recursive! [file]
  (when (.exists (io/file file))
    (doseq [f (reverse (file-seq (io/file file)))]
      (.delete f))))

(deftest stable-cache-id-test
  (testing "canonicalizes unordered data before hashing"
    (is (= (memo/stable-cache-id {:a 1 :b #{3 2 1}})
           (memo/stable-cache-id {:b #{1 3 2} :a 1}))))

  (testing "changes when params change"
    (is (not= (memo/stable-cache-id {:model "a"})
              (memo/stable-cache-id {:model "b"})))))

(deftest axis-spec-coercion-test
  (testing "coerces mappings of contiguous values"
    (let [space (memo/coerce-axis-spec (array-map :x (range 10 13)
                                                  :y [20 21]))]
      (is (= [:x :y] (mapv :name (:axes space))))
      (is (= [3 2] (get-in space [:layout :shape])))))

  (testing "passes through parameter spaces"
    (let [space (params/run-parameter-space [(params/param-axis :x 0 2)])]
      (is (identical? space (memo/coerce-axis-spec space)))))

  (testing "coerces sequences of ParamAxis records"
    (let [space (memo/coerce-axis-spec [(params/param-axis :x 0 2)])]
      (is (= [2] (get-in space [:layout :shape])))))

  (testing "validates unsupported axis specs"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"axis cannot be empty"
                          (memo/coerce-axis-spec {:x []})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"axis must be contiguous for now"
                          (memo/coerce-axis-spec {:x [1 3]})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"axis-spec must be"
                          (memo/coerce-axis-spec [:bad])))))

(deftest chunk-memo-cache-test
  (let [root (temp-dir)]
    (try
      (let [m (memo/chunk-memo root
                               (array-map :x (range 0 2)
                                          :y (range 10 12))
                               {:chunk-size 2
                                :params {:run "base"}})]
        (testing "creates and reuses caches by stable params"
          (let [a (memo/cache-for-params m {:model "a"})
                b (memo/cache-for-params m {:model "a"})
                c (memo/cache-for-params m {:model "b"})]
            (is (identical? a b))
            (is (not= (:root a) (:root c)))
            (is (.exists (:root a)))
            (is (.exists (:root c)))))

        (testing "extracts axis values in rank order"
          (is (= [1 11] (memo/extract-axis-values m {:y 11 :x 1})))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"missing axis argument"
                                (memo/extract-axis-values m {:x 1})))))
      (finally
        (delete-recursive! root)))))

(deftest memoize-test
  (let [root (temp-dir)]
    (try
      (let [m     (memo/chunk-memo root
                                   (array-map :x (range 0 2)
                                              :y (range 10 12))
                                   {:chunk-size 2})
            calls (atom [])
            f     (memo/memoize
                   m
                   (fn [arguments]
                     (swap! calls conj arguments)
                     {:sum (+ (:x arguments) (:y arguments))}))]
        (testing "computes, stores, and reuses cached results"
          (is (= {:sum 11} (f {:x 1 :y 10 :params {:model "a"}})))
          (is (= {:sum 11} (f {:x 1 :y 10 :params {:model "a"}})))
          (is (= 1 (count @calls))))

        (testing "cache params select independent caches"
          (is (= {:sum 11} (f {:x 1 :y 10 :params {:model "b"}})))
          (is (= 2 (count @calls))))

        (testing "missing reports uncached argument maps"
          (is (= [{:x 0 :y 10 :params {:model "a"}}]
                 (memo/missing m [{:x 1 :y 10 :params {:model "a"}}
                                  {:x 0 :y 10 :params {:model "a"}}]))))

        (testing "validates wrapper arguments"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"memoized functions expect a single argument map"
                                (f [:not :a :map])))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"cache params must be a map"
                                (f {:x 1 :y 10 :params :bad})))))
      (finally
        (delete-recursive! root)))))
