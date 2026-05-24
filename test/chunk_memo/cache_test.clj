(ns chunk-memo.cache-test
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.params :as params]
            [clojure.test :refer [deftest is testing]]))

(defn test-space []
  (params/run-parameter-space [(params/param-axis :x 10 13)
                               (params/param-axis :y 20 22)]))

(deftest logical-chunk-cache-test
  (let [c (cache/logical-chunk-cache (test-space) 3)]
    (testing "stores logical cache geometry only"
      (is (= 3 (:chunk-size c)))
      (is (= [3 2] (get-in c [:space :layout :shape])))
      (is (= nil (:root c)))
      (is (= nil (:index-root c)))
      (is (= nil (:payload-root c))))

    (testing "translates semantic params and positions"
      (is (= [1 1] (cache/params->pos c [11 21])))
      (is (= [11 21] (cache/pos->params c [1 1])))
      (is (= 3 (cache/params->index c [11 21])))
      (is (= [1 1] (cache/index->pos c 3)))
      (is (= [11 21] (cache/index->params c 3)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"wrong parameter rank"
                            (cache/params->pos c [11])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"wrong position rank"
                            (cache/pos->params c [1]))))

    (testing "derives chunks and offsets from logical geometry"
      (is (= [0 1] (vec (cache/chunk-ids c))))
      (is (= [0 1 2] (vec (cache/chunk-offsets c 0))))
      (is (= [0 1 2] (vec (cache/chunk-offsets c 1))))
      (is (= 4 (cache/chunk-offset->index c 1 1)))
      (is (= [1 1] (cache/index->chunk-offset c 4)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"chunk offset out of bounds"
                            (cache/chunk-offset->index c 2 0))))))

(deftest logical-cache-universe-test
  (let [base   (cache/logical-chunk-cache (test-space) 3)
        append (cache/logical-chunk-cache (test-space) 4)
        u      (cache/logical-cache-universe {:base base
                                              :append append})]
    (testing "looks up logical caches by universe id"
      (is (identical? base (cache/universe-cache u :base)))
      (is (identical? append (cache/universe-cache u :append)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"unknown cache universe"
                            (cache/universe-cache u :missing))))

    (testing "addresses are interpreted within their universe"
      (is (= base (cache/address-cache u {:universe :base
                                          :chunk-id 1
                                          :offset 0})))
      (is (= 3 (cache/address->index u {:universe :base
                                        :chunk-id 1
                                        :offset 0})))
      (is (= 4 (cache/address->index u {:universe :append
                                        :chunk-id 1
                                        :offset 0})))
      (is (= {:universe :base :chunk-id 1 :offset 1}
             (cache/index->address u :base 4)))
      (is (= [11 21]
             (cache/address->params u {:universe :base
                                       :chunk-id 1
                                       :offset 0})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"address requires :universe"
                            (cache/address-cache u {:chunk-id 0
                                                    :offset 0}))))))

(deftest cache-item-enumeration-test
  (let [base (cache/logical-chunk-cache (test-space) 3)
        u    (cache/logical-cache-universe {:base base})]
    (testing "enumerates items for a chunk"
      (is (= [{:address {:universe :base :chunk-id 1 :offset 0}
               :index 3
               :params [11 21]}
              {:address {:universe :base :chunk-id 1 :offset 1}
               :index 4
               :params [12 20]}
              {:address {:universe :base :chunk-id 1 :offset 2}
               :index 5
               :params [12 21]}]
             (cache/chunk-items base :base 1))))

    (testing "enumerates a whole universe"
      (is (= 6 (count (cache/universe-items u))))
      (is (= {:address {:universe :base :chunk-id 0 :offset 0}
              :index 0
              :params [10 20]}
             (first (cache/universe-items u)))))))

(deftest mapped-chunk-cache-test
  (let [base   (cache/logical-chunk-cache (test-space) 3)
        u      (cache/logical-cache-universe {:base base})
        m      (cache/mapped-chunk-cache u)
        item   {:address {:universe :base :chunk-id 1 :offset 2}
                :index 5
                :params [12 21]}]
    (testing "identity map preserves addresses"
      (is (= (:address item)
             (cache/logical->mapped (:cache-map m) (:universe m) (:address item))))
      (is (= (:address item)
             (cache/mapped->logical (:cache-map m) (:universe m) (:address item))))
      (is (= (assoc item :mapped-address (:address item))
             (cache/mapped-item m item))))

    (testing "mapped cache enumerates logical and mapped addresses"
      (is (= 6 (count (cache/mapped-items m))))
      (is (= {:address {:universe :base :chunk-id 0 :offset 0}
              :index 0
              :params [10 20]
              :mapped-address {:universe :base :chunk-id 0 :offset 0}}
             (first (cache/mapped-items m)))))))
