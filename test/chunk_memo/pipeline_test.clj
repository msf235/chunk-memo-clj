(ns chunk-memo.pipeline-test
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.pipeline :as pipeline]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir []
  (.toFile (Files/createTempDirectory "chunk-memo-pipeline-test"
                                      (make-array FileAttribute 0))))

(def axis-spec
  (array-map :x (range 10 13)
             :y (range 20 22)))

(deftest parameter-space-test
  (testing "creates semantic parameter axes from ordered axis specs"
    (let [space (pipeline/parameter-space axis-spec)]
      (is (= [:x :y] (mapv :name (:axes space))))
      (is (= [[10 13] [20 22]]
             (mapv (juxt :start :stop) (:axes space))))
      (is (= [3 2] (get-in space [:layout :shape])))))

  (testing "returns an existing parameter space unchanged"
    (let [space (pipeline/parameter-space axis-spec)]
      (is (identical? space (pipeline/parameter-space space)))))

  (testing "rejects non-contiguous axis values"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"axis values must be a non-empty contiguous integer sequence"
                          (pipeline/parameter-space
                           (array-map :x [10 12]))))))

(deftest mapped-cache-from-params-test
  (let [mapped-cache (pipeline/mapped-cache-from-params axis-spec {:chunk-size 3})
        items        (cache/mapped-items mapped-cache)]
    (testing "builds normalized addresses while preserving semantic params"
      (is (= 6 (count items)))
      (is (= [{:address {:universe :base :chunk-id 0 :offset 0}
               :index 0
               :params [10 20]
               :mapped-address {:universe :base :chunk-id 0 :offset 0}}
              {:address {:universe :base :chunk-id 0 :offset 1}
               :index 1
               :params [10 21]
               :mapped-address {:universe :base :chunk-id 0 :offset 1}}
              {:address {:universe :base :chunk-id 0 :offset 2}
               :index 2
               :params [11 20]
               :mapped-address {:universe :base :chunk-id 0 :offset 2}}]
             (subvec items 0 3))))))

(deftest param-map-test
  (let [space (pipeline/parameter-space axis-spec)
        item  {:params [11 21]}]
    (testing "maps item parameter vectors back to semantic parameter maps"
      (is (= {:x 11 :y 21}
             (pipeline/param-map space item))))))

(deftest realize-from-params-test
  (let [root     (temp-dir)
        executed (atom [])
        result   (pipeline/realize-from-params!
                  root
                  axis-spec
                  (fn [{:keys [x y] :as param-map}]
                    (swap! executed conj param-map)
                    {:sum (+ x y)})
                  {:chunk-size 3})]
    (testing "computes missing payloads from semantic parameter maps"
      (is (= 6 (:planned-count result)))
      (is (= 6 (:executed-count result)))
      (is (= [:computed :computed :computed :computed :computed :computed]
             (mapv :source (:results result))))
      (is (= [{:x 10 :y 20}
              {:x 10 :y 21}
              {:x 11 :y 20}
              {:x 11 :y 21}
              {:x 12 :y 20}
              {:x 12 :y 21}]
             @executed))
      (is (= [{:sum 30} {:sum 31} {:sum 31} {:sum 32} {:sum 32} {:sum 33}]
             (mapv :payload (:results result)))))

    (testing "loads existing payloads on later calls"
      (let [loaded (pipeline/realize-from-params!
                    root
                    axis-spec
                    (fn [_]
                      (throw (ex-info "should not run" {})))
                    {:chunk-size 3})]
        (is (= 6 (:present-count loaded)))
        (is (= 0 (:planned-count loaded)))
        (is (= 0 (:executed-count loaded)))
        (is (= [:stored :stored :stored :stored :stored :stored]
               (mapv :source (:results loaded)))))))

  (testing "supports the underscore alias"
    (let [root (temp-dir)]
      (is (= [{:value 10}]
             (mapv :payload
                   (:results (pipeline/realize_from_params!
                              root
                              (array-map :x [10])
                              (fn [{:keys [x]}]
                                {:value x})))))))))
