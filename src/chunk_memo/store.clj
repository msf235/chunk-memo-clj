(ns chunk-memo.store
  (:require [chunk-memo.cache :as cache]
            [clojure.set]))

(defprotocol CacheStore
  (present-mapped-addresses [store mapped-cache]
    "Return a set of mapped address maps observed in `store`."))

(defn expected-mapped-items
  "Return a map of mapped address -> item for every expected item."
  [mapped-cache]
  (into {}
        (map (juxt :mapped-address identity))
        (cache/mapped-items mapped-cache)))

(defn cache-status
  "Compare expected mapped-cache items against addresses present in `store`.

  Returns item maps for expected addresses and raw address maps for extras that
  exist in the store but are not part of the mapped cache."
  [store mapped-cache]
  (let [expected-by-address (expected-mapped-items mapped-cache)
        expected-addresses  (set (keys expected-by-address))
        present-addresses   (present-mapped-addresses store mapped-cache)
        present-expected    (clojure.set/intersection expected-addresses present-addresses)
        missing-addresses   (clojure.set/difference expected-addresses present-addresses)
        extra-addresses     (clojure.set/difference present-addresses expected-addresses)]
    {:present (mapv expected-by-address (sort-by (juxt :universe :chunk-id :offset) present-expected))
     :missing (mapv expected-by-address (sort-by (juxt :universe :chunk-id :offset) missing-addresses))
     :extra   (vec (sort-by (juxt :universe :chunk-id :offset) extra-addresses))}))

(defn present-items
  "Return expected mapped-cache items that are present in `store`."
  [store mapped-cache]
  (:present (cache-status store mapped-cache)))

(defn missing-items
  "Return expected mapped-cache items that are missing from `store`."
  [store mapped-cache]
  (:missing (cache-status store mapped-cache)))

(defn chunk-statuses
  "Return mapped chunk completion summaries.

  Each summary is keyed by mapped universe and chunk id and reports `:empty`,
  `:partial`, or `:complete` based on expected items in that mapped chunk."
  [store mapped-cache]
  (let [{:keys [present missing]} (cache-status store mapped-cache)
        items                    (concat (map #(assoc % :present? true) present)
                                         (map #(assoc % :present? false) missing))]
    (->> items
         (group-by (fn [item]
                     (select-keys (:mapped-address item) [:universe :chunk-id])))
         (map (fn [[chunk-address chunk-items]]
                (let [total         (count chunk-items)
                      present-count (count (filter :present? chunk-items))]
                  (assoc chunk-address
                         :total-count total
                         :present-count present-count
                         :missing-count (- total present-count)
                         :status (cond
                                   (zero? present-count) :empty
                                   (= present-count total) :complete
                                   :else :partial)))))
         (sort-by (juxt :universe :chunk-id))
         vec)))
