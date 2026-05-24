(ns chunk-memo.test-runner
  (:require [clojure.test :as test]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.java.io :as io]))

(defn test-namespaces []
  (->> (ns-find/find-namespaces-in-dir (io/file "test"))
       (filter #(re-find #"-test$" (name %)))
       sort))

(defn -main [& _args]
  (let [namespaces (test-namespaces)]
    (doseq [namespace namespaces]
      (require namespace))
    (let [{:keys [fail error]} (apply test/run-tests namespaces)]
      (when (pos? (+ fail error))
        (System/exit 1)))))
