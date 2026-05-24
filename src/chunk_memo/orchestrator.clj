(ns chunk-memo.orchestrator
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.parallel :as parallel]
            [chunk-memo.store :as store]))

(defn plan
  "Plan cache work by comparing expected mapped-cache items with `store`."
  [store mapped-cache]
  (store/cache-status store mapped-cache))

(defn- run-item!
  [store mapped-cache f item]
  (let [payload (f item)]
    (store/write-payload! store mapped-cache item payload)
    {:item item
     :payload payload
     :source :computed}))

(defn- load-item
  [store mapped-cache item]
  {:item item
   :payload (store/read-payload store mapped-cache item)
   :source :stored})

(defn run-plan!
  "Execute `f` for every missing item in `execution-plan` and store payloads.

  `f` receives a mapped-cache item and returns the payload to persist. Results
  are returned in missing-item order."
  ([store mapped-cache execution-plan f]
   (run-plan! store mapped-cache execution-plan f {}))
  ([store mapped-cache {:keys [missing] :as execution-plan} f {:keys [threads]
                                                               :or   {threads 1}}]
   (let [worker  #(run-item! store mapped-cache f %)
         results (if (= 1 threads)
                   (mapv worker missing)
                   (parallel/run-work! missing worker {:threads threads}))]
     {:plan execution-plan
      :planned-count (count missing)
      :executed-count (count results)
      :results results})))

(defn run-missing!
  "Plan cache work, execute `f` for missing items, and store returned payloads."
  ([store mapped-cache f]
   (run-missing! store mapped-cache f {}))
  ([store mapped-cache f opts]
   (run-plan! store mapped-cache (plan store mapped-cache) f opts)))

(defn load-present
  "Load payloads for items already present in `store`."
  ([store mapped-cache]
   (load-present store mapped-cache (plan store mapped-cache)))
  ([store mapped-cache {:keys [present] :as execution-plan}]
   {:plan execution-plan
    :present-count (count present)
    :results (mapv #(load-item store mapped-cache %) present)}))

(defn- result-address
  [{:keys [item]}]
  (:mapped-address item))

(defn realize!
  "Ensure every mapped-cache item has a stored payload and return all payloads.

  Stored payloads are loaded for present items. Missing items are computed with
  `f`, stored, and included in the returned results. Results are returned in
  mapped-cache item order."
  ([store mapped-cache f]
   (realize! store mapped-cache f {}))
  ([store mapped-cache f opts]
   (let [execution-plan (plan store mapped-cache)
         loaded         (load-present store mapped-cache execution-plan)
         executed       (run-plan! store mapped-cache execution-plan f opts)
         results-by-address (into {}
                                  (map (juxt result-address identity))
                                  (concat (:results loaded)
                                          (:results executed)))]
     {:plan execution-plan
      :present-count (:present-count loaded)
      :planned-count (:planned-count executed)
      :executed-count (:executed-count executed)
      :results (mapv #(get results-by-address (:mapped-address %))
                     (cache/mapped-items mapped-cache))})))
