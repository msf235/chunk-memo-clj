(ns chunk-memo.store.filesystem-test
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.params :as params]
            [chunk-memo.store :as store]
            [chunk-memo.store.filesystem :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir []
  (.toFile (Files/createTempDirectory "chunk-memo-filesystem-test"
                                      (make-array FileAttribute 0))))

(defn test-space []
  (params/run-parameter-space [(params/param-axis :x 10 13)
                               (params/param-axis :y 20 22)]))

(defn test-mapped-cache []
  (-> {:base (cache/logical-chunk-cache (test-space) 3)}
      cache/logical-cache-universe
      cache/mapped-chunk-cache))

(defn file-lines [file]
  (str/split-lines (slurp file)))

(deftest write-mapped-cache-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)]
    (testing "writes mapped universes as directories and chunks as CSV files"
      (fs/write-mapped-cache! store mapped-cache)
      (is (= ["base,0,0" "base,0,1" "base,0,2"]
             (file-lines (fs/chunk-file store :base 0))))
      (is (= ["base,1,0" "base,1,1" "base,1,2"]
             (file-lines (fs/chunk-file store :base 1))))
      (is (= 6 (count (store/present-items store mapped-cache))))
      (is (= [] (store/missing-items store mapped-cache))))))

(deftest scan-cache-status-test
  (let [store        (fs/filesystem-store (temp-dir))
        mapped-cache (test-mapped-cache)
        file         (fs/chunk-file store :base 0)]
    (testing "reports present, missing, and extra mapped addresses"
      (.mkdirs (.getParentFile file))
      (spit file "base,0,0\nbase,1,2\nbase,99,0\n")
      (let [status (store/cache-status store mapped-cache)]
        (is (= [{:universe :base :chunk-id 99 :offset 0}]
               (:extra status)))
        (is (= [{:universe :base :chunk-id 0 :offset 0}
                {:universe :base :chunk-id 1 :offset 2}]
               (mapv :mapped-address (:present status))))
        (is (= [{:universe :base :chunk-id 0 :offset 1}
                {:universe :base :chunk-id 0 :offset 2}
                {:universe :base :chunk-id 1 :offset 0}
                {:universe :base :chunk-id 1 :offset 1}]
               (mapv :mapped-address (:missing status)))))))

  (testing "reports mapped chunk completion summaries"
    (let [store        (fs/filesystem-store (temp-dir))
          mapped-cache (test-mapped-cache)
          file         (fs/chunk-file store :base 0)]
      (.mkdirs (.getParentFile file))
      (spit file "base,0,0\nbase,0,1\nbase,0,2\nbase,1,0\n")
      (is (= [{:universe :base
               :chunk-id 0
               :total-count 3
               :present-count 3
               :missing-count 0
               :status :complete}
              {:universe :base
               :chunk-id 1
               :total-count 3
               :present-count 1
               :missing-count 2
               :status :partial}]
             (store/chunk-statuses store mapped-cache))))))

(deftest filesystem-validation-test
  (testing "rejects unsafe universe directory names"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"universe id is not safe"
                          (fs/filesystem-name "../bad")))))
