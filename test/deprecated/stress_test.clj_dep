(ns chunk-memo.stress-test
  (:refer-clojure :exclude [memoize])
  (:require [chunk-memo.memo :as memo]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer [deftest is testing]]))

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory "chunk-memo-stress-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn delete-recursive! [file]
  (when (.exists (io/file file))
    (doseq [f (reverse (file-seq (io/file file)))]
      (.delete f))))

(def stress-axes
  (array-map :model (range 0 5)
             :fold (range 0 5)
             :window (range 0 3)
             :seed (range 0 4)))

(defn stress-arguments []
  (let [axis-names  (vec (keys stress-axes))
        axis-values (map stress-axes axis-names)]
    (mapv (fn [values]
            (assoc (zipmap axis-names values)
                   :params {:dataset "stress"}))
          (apply combo/cartesian-product axis-values))))

(defn slow-stress-result
  [{:keys [model fold window seed]}]
  (Thread/sleep 1)
  {:score (+ (* 1000 model)
             (* 100 fold)
             (* 10 window)
             seed)
   :label (str "m" model "-f" fold "-w" window "-s" seed)})

(deftest cold-and-hot-cache-stress-test
  (let [root      (temp-dir)
        arguments (stress-arguments)]
    (try
      (let [m          (memo/chunk-memo root stress-axes {:chunk-size 17})
            cold-calls (atom 0)
            cold-f     (memo/memoize
                        m
                        (fn [arguments]
                          (swap! cold-calls inc)
                          (slow-stress-result arguments)))]
        (testing "cold cache computes and stores every point"
          (is (= (count arguments)
                 (count (memo/missing m arguments))))
          (let [results (mapv cold-f arguments)]
            (is (= (count arguments) (count results)))
            (is (= (count arguments) @cold-calls))
            (is (= [] (memo/missing m arguments)))))

        (testing "hot cache reuses persisted results without recomputing"
          (let [hot-m      (memo/chunk-memo root stress-axes {:chunk-size 17})
                hot-calls  (atom 0)
                hot-f      (memo/memoize
                            hot-m
                            (fn [arguments]
                              (swap! hot-calls inc)
                              (slow-stress-result arguments)))
                hot-result (mapv hot-f arguments)]
            (is (= (count arguments) (count hot-result)))
            (is (= 0 @hot-calls))
            (is (= [] (memo/missing hot-m arguments))))))
      (finally
        (delete-recursive! root)))))
