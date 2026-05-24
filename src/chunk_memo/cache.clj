(ns chunk-memo.cache
  (:require [chunk-memo.chunks :as chunks]
            [chunk-memo.layout :as layout]
            [chunk-memo.params :as params]))

(def default-chunk-size 200)

(defrecord LogicalChunkCache [space chunk-size chunk-spec])
(defrecord LogicalCacheUniverse [caches])
(defrecord IdentityCacheMap [])
(defrecord MappedChunkCache [universe cache-map])

(defprotocol CacheMap
  (logical->mapped [cache-map universe address]
    "Translate a logical address into the mapped address space.")
  (mapped->logical [cache-map universe address]
    "Translate a mapped address back into the logical address space."))

(extend-type IdentityCacheMap
  CacheMap
  (logical->mapped [_ _ address]
    address)
  (mapped->logical [_ _ address]
    address))

(defn logical-chunk-cache
  "Create a logical chunk cache.

  A LogicalChunkCache owns only the parameter-space geometry and chunk algebra.
  It does not know anything about disk paths, payloads, or completion metadata."
  ([space]
   (logical-chunk-cache space default-chunk-size))
  ([space chunk-size]
   (->LogicalChunkCache space
                        chunk-size
                        (chunks/chunk-spec chunk-size
                                           (get-in space [:layout :size])))))

(defn identity-cache-map
  "Create an identity cache map. Logical and mapped addresses are identical."
  []
  (->IdentityCacheMap))

(defn logical-cache-universe
  "Create a universe of named logical chunk caches."
  [caches]
  (when-not (map? caches)
    (throw (ex-info "logical cache universe requires a map"
                    {:caches caches})))
  (doseq [[universe-id cache] caches]
    (when-not (instance? LogicalChunkCache cache)
      (throw (ex-info "universe values must be LogicalChunkCache instances"
                      {:universe universe-id
                       :cache cache}))))
  (->LogicalCacheUniverse caches))

(defn mapped-chunk-cache
  "Create a mapped chunk cache from a logical cache universe and cache map."
  ([cache-universe]
   (mapped-chunk-cache cache-universe (identity-cache-map)))
  ([cache-universe cache-map]
   (when-not (instance? LogicalCacheUniverse cache-universe)
     (throw (ex-info "mapped chunk cache requires a LogicalCacheUniverse"
                     {:universe cache-universe})))
   (when-not (satisfies? CacheMap cache-map)
     (throw (ex-info "mapped chunk cache requires a CacheMap"
                     {:cache-map cache-map})))
   (->MappedChunkCache cache-universe cache-map)))

(defn universe-cache
  "Return the LogicalChunkCache named by `universe-id`."
  [{:keys [caches]} universe-id]
  (or (get caches universe-id)
      (throw (ex-info "unknown cache universe"
                      {:universe universe-id
                       :known-universes (set (keys caches))}))))

(defn cache-for-address
  "Return the LogicalChunkCache for `address`."
  [cache-universe {universe-id :universe :as address}]
  (when-not (contains? address :universe)
    (throw (ex-info "address requires :universe" {:address address})))
  (universe-cache cache-universe universe-id))

;; ---------------------------------------------------------------------------
;; Logical coordinate translation
;; ---------------------------------------------------------------------------

(defn params->pos
  "Translate semantic parameter values into zero-based coordinate positions."
  [{:keys [space]} param-values]
  (let [param-values (vec param-values)
        axes         (:axes space)]
    (when-not (= (count param-values) (count axes))
      (throw (ex-info "wrong parameter rank"
                      {:expected (count axes)
                       :actual   (count param-values)})))
    (mapv params/value->pos axes param-values)))

(defn pos->params
  "Translate zero-based coordinate positions into semantic parameter values."
  [{:keys [space]} positions]
  (let [positions (vec positions)
        axes      (:axes space)]
    (when-not (= (count positions) (count axes))
      (throw (ex-info "wrong position rank"
                      {:expected (count axes)
                       :actual   (count positions)})))
    (mapv params/pos->value axes positions)))

(defn params->index
  "Translate semantic parameter values into a flat row-major index."
  [{:keys [space] :as cache} param-values]
  (layout/coord->index (:layout space) (params->pos cache param-values)))

(defn index->pos
  "Translate a flat row-major index into zero-based coordinate positions."
  [{:keys [space]} index]
  (layout/index->coord (:layout space) index))

(defn index->params
  "Translate a flat row-major index into semantic parameter values."
  [cache index]
  (pos->params cache (index->pos cache index)))

;; ---------------------------------------------------------------------------
;; Logical chunk algebra
;; ---------------------------------------------------------------------------

(defn chunk-ids
  "Return every valid chunk id in `cache`."
  [{:keys [chunk-spec]}]
  (range (chunks/num-chunks chunk-spec)))

(defn chunk-offsets
  "Return every valid chunk-local offset in `chunk-id`."
  [{:keys [chunk-spec]} chunk-id]
  (range (chunks/chunk-capacity chunk-spec chunk-id)))

(defn chunk-offset->index
  "Translate a chunk-local address into a flat index."
  [{:keys [chunk-spec]} chunk-id offset]
  (let [{:keys [chunk-size total-size]} chunk-spec
        index (+ (* chunk-id chunk-size) offset)]
    (when-not (<= 0 index (dec total-size))
      (throw (ex-info "chunk offset out of bounds"
                      {:chunk-id chunk-id
                       :offset offset
                       :total-size total-size})))
    (let [[expected-chunk-id _] (chunks/index->chunk-offset chunk-spec index)]
      (when-not (= chunk-id expected-chunk-id)
        (throw (ex-info "offset outside chunk capacity"
                        {:chunk-id chunk-id
                         :offset offset}))))
    index))

(defn index->chunk-offset
  "Translate a flat index into `[chunk-id offset]`."
  [{:keys [chunk-spec]} index]
  (chunks/index->chunk-offset chunk-spec index))

(defn address->index
  "Translate an address map into a flat index within its universe."
  [cache-universe {:keys [chunk-id offset] :as address}]
  (let [cache (cache-for-address cache-universe address)]
    (chunk-offset->index cache chunk-id offset)))

(defn index->address
  "Translate a universe-local flat index into an address map."
  [cache-universe universe-id index]
  (let [cache             (universe-cache cache-universe universe-id)
        [chunk-id offset] (index->chunk-offset cache index)]
    {:universe universe-id
     :chunk-id chunk-id
     :offset offset}))

(defn address->params
  "Translate an address map into semantic parameter values."
  [cache-universe address]
  (let [cache (cache-for-address cache-universe address)]
    (index->params cache (address->index cache-universe address))))

(defn chunk-items
  "Return logical item maps for every offset in `chunk-id`."
  [cache universe-id chunk-id]
  (mapv (fn [offset]
          (let [index (chunk-offset->index cache chunk-id offset)]
            {:address {:universe universe-id
                       :chunk-id chunk-id
                       :offset offset}
             :index index
             :params (index->params cache index)}))
        (chunk-offsets cache chunk-id)))

(defn cache-items
  "Return logical item maps for every item in `cache`."
  [cache universe-id]
  (mapcat #(chunk-items cache universe-id %) (chunk-ids cache)))

(defn universe-items
  "Return logical item maps for every item in every cache universe."
  [{:keys [caches]}]
  (mapcat (fn [[universe-id cache]]
            (cache-items cache universe-id))
          caches))

(defn mapped-item
  "Attach the mapped address for one logical item."
  [{cache-universe :universe cache-map :cache-map} item]
  (assoc item :mapped-address (logical->mapped cache-map cache-universe (:address item))))

(defn mapped-items
  "Return item maps with both logical and mapped addresses."
  [{cache-universe :universe :as mapped-cache}]
  (mapv #(mapped-item mapped-cache %) (universe-items cache-universe)))
