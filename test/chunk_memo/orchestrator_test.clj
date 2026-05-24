(ns chunk-memo.orchestrator-test
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.orchestrator :as orchestrator]
            [chunk-memo.params :as params]
            [chunk-memo.store :as store]
            [chunk-memo.store.filesystem :as fs]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir []
  (.toFile (Files/createTempDirectory "chunk-memo-orchestrator-test"
                                      (make-array FileAttribute 0))))

(defn test-space []
  (params/run-parameter-space [(params/param-axis :x 10 13)
                               (params/param-axis :y 20 22)]))

(defn test-mapped-cache []
  (-> {:base (cache/logical-chunk-cache (test-space) 3)}
      cache/logical-cache-universe
      cache/mapped-chunk-cache))

(deftest plan-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)]
    (testing "delegates planning to cache status"
      (is (= (store/cache-status store mapped-cache)
             (orchestrator/plan store mapped-cache))))))

(deftest run-missing-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)]
    (testing "executes missing items and stores returned payloads"
      (let [result (orchestrator/run-missing!
                    store
                    mapped-cache
                    (fn [{:keys [index params]}]
                      {:index index
                       :params params}))]
        (is (= 6 (:planned-count result)))
        (is (= 6 (:executed-count result)))
        (is (= [] (:missing (orchestrator/plan store mapped-cache))))
        (doseq [{:keys [item payload]} (:results result)]
          (is (= payload
                 (store/read-payload store mapped-cache item))))))
    (testing "does not rerun items already present"
      (let [result (orchestrator/run-missing!
                    store
                    mapped-cache
                    (fn [_]
                      (throw (ex-info "should not run" {}))))]
        (is (= 0 (:planned-count result)))
        (is (= 0 (:executed-count result)))
        (is (= [] (:results result)))))))

(deftest run-plan-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)
        execution-plan (orchestrator/plan store mapped-cache)
        result       (orchestrator/run-plan! store
                                             mapped-cache
                                             execution-plan
                                             (fn [item]
                                               (:mapped-address item)))]
    (testing "executes a provided plan"
       (is (= execution-plan (:plan result)))
       (is (= 6 (:executed-count result)))
       (is (= [] (:missing (orchestrator/plan store mapped-cache)))))))

(deftest load-present-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)
        item         (first (cache/mapped-items mapped-cache))
        payload      {:loaded (:index item)}]
    (store/write-payload! store mapped-cache item payload)
    (let [result (orchestrator/load-present store mapped-cache)]
      (testing "loads rich results for present items"
        (is (= 1 (:present-count result)))
        (is (= [{:item item
                 :payload payload
                 :source :stored}]
               (:results result)))))))

(deftest realize-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)
        items        (cache/mapped-items mapped-cache)
        stored-item  (first items)
        stored-payload {:stored (:index stored-item)}
        executed     (atom [])]
    (store/write-payload! store mapped-cache stored-item stored-payload)
    (let [result (orchestrator/realize!
                  store
                  mapped-cache
                  (fn [item]
                    (swap! executed conj (:index item))
                    {:computed (:index item)}))]
      (testing "loads present items, computes missing items, and returns mapped-cache order"
        (is (= 1 (:present-count result)))
        (is (= 5 (:planned-count result)))
        (is (= 5 (:executed-count result)))
        (is (= (mapv :index items)
               (mapv (comp :index :item) (:results result))))
        (is (= [:stored :computed :computed :computed :computed :computed]
               (mapv :source (:results result))))
        (is (= stored-payload
               (:payload (first (:results result)))))
        (is (= (mapv :index (rest items))
               @executed))
        (is (= [] (:missing (orchestrator/plan store mapped-cache))))))))
