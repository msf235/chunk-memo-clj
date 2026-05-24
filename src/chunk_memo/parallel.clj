(ns chunk-memo.parallel
  (:import [java.util.concurrent Callable ExecutionException Executors]))

(defn- available-processors []
  (.availableProcessors (Runtime/getRuntime)))

(defn- executor [threads]
  (Executors/newFixedThreadPool threads))

(defn- submit-work! [executor worker work]
  (.submit executor
           (reify Callable
             (call [_]
               (worker work)))))

(defn- future-value [future]
  (try
    (.get future)
    (catch ExecutionException e
      (throw (.getCause e)))))

(defn run-work!
  "Run `worker` over `work-items` using a bounded thread pool.

  This function does not interpret work items. Callers are expected to pass
  already-resolved values such as `chunk-memo.chunks/Chunk` records. Results are
  returned in input order."
  ([work-items worker]
   (run-work! work-items worker {}))
  ([work-items worker {:keys [threads]
                       :or   {threads (available-processors)}}]
   (let [work-items (vec work-items)
         executor   (executor threads)]
     (try
       (let [futures (mapv #(submit-work! executor worker %) work-items)]
         (mapv future-value futures))
       (finally
         (.shutdown executor))))))
