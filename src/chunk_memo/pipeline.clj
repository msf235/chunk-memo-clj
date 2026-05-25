(ns chunk-memo.pipeline
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.orchestrator :as orchestrator]
            [chunk-memo.params :as params]
            [chunk-memo.store.filesystem :as fs]))

(defn- contiguous-integers?
  [values]
  (and (seq values)
       (every? integer? values)
       (every? (fn [[left right]]
                 (= (inc left) right))
               (partition 2 1 values))))

(defn- axis-from-spec
  [[axis-name values]]
  (let [values (vec values)]
    (when-not (contiguous-integers? values)
      (throw (ex-info "axis values must be a non-empty contiguous integer sequence"
                      {:axis axis-name
                       :values values})))
    (params/param-axis axis-name (first values) (inc (last values)))))

(defn parameter-space
  "Create a RunParameterSpace from an ordered map or return one unchanged.

  Map values must be non-empty contiguous integer sequences. Semantic values are
  normalized by `chunk-memo.params` and `chunk-memo.cache` when cache addresses
  are derived."
  [axis-spec]
  (if (instance? chunk_memo.params.RunParameterSpace axis-spec)
    axis-spec
    (do
      (when-not (map? axis-spec)
        (throw (ex-info "axis spec must be an ordered map or RunParameterSpace"
                        {:axis-spec axis-spec})))
      (params/run-parameter-space (mapv axis-from-spec axis-spec)))))

(defn mapped-cache-from-params
  "Build a mapped cache from semantic parameter specs."
  ([axis-spec]
   (mapped-cache-from-params axis-spec {}))
  ([axis-spec {:keys [universe chunk-size cache-map]
               :or   {universe :base
                      chunk-size cache/default-chunk-size}}]
   (let [space          (parameter-space axis-spec)
         logical-cache  (cache/logical-chunk-cache space chunk-size)
         cache-universe (cache/logical-cache-universe {universe logical-cache})]
     (if cache-map
       (cache/mapped-chunk-cache cache-universe cache-map)
       (cache/mapped-chunk-cache cache-universe)))))

(defn param-map
  "Return a semantic parameter map for a mapped-cache item."
  [space item]
  (zipmap (map :name (:axes space)) (:params item)))

(defn realize-from-params!
  "Realize a filesystem-backed cache from semantic parameter specs.

  `f` receives a map from axis names to semantic parameter values. The returned
  payloads are stored by the filesystem store and loaded on later calls."
  ([root axis-spec f]
   (realize-from-params! root axis-spec f {}))
  ([root axis-spec f opts]
   (let [space        (parameter-space axis-spec)
         store        (fs/filesystem-store root)
         mapped-cache (mapped-cache-from-params space opts)]
     (orchestrator/realize!
      store
      mapped-cache
      (fn [item]
        (f (param-map space item)))
      opts))))

(def realize_from_params!
  "Alias for `realize-from-params!`."
  realize-from-params!)
