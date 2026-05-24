(ns chunk-memo.parallel-test
  (:require [chunk-memo.chunks :as chunks]
            [chunk-memo.parallel :as parallel]
            [clojure.test :refer [deftest is testing]]))

(deftest run-work-test
  (testing "runs work and returns results in input order"
    (let [work [(chunks/chunk-ref 0 [0 1])
                (chunks/chunk-ref 1 [2])
                (chunks/chunk-ref 2)]
          result (parallel/run-work!
                  work
                  (fn [{:keys [chunk-id offsets]}]
                    (when (= 0 chunk-id)
                      (Thread/sleep 20))
                    [chunk-id offsets])
                  {:threads 2})]
      (is (= [[0 [0 1]] [1 [2]] [2 nil]] result))))

  (testing "handles empty work"
    (is (= [] (parallel/run-work! [] identity {:threads 2}))))

  (testing "propagates worker exceptions"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"worker failed"
                          (parallel/run-work!
                           [(chunks/chunk-ref 0)]
                           (fn [_]
                             (throw (ex-info "worker failed" {})))
                           {:threads 1})))))
