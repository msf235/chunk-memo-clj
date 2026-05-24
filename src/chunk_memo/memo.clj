(ns chunk-memo.memo
  (:refer-clojure :exclude [memoize])
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.params :as params]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.math BigInteger]
           [java.security MessageDigest]
           [java.nio.file Files StandardCopyOption]))

(def default-chunk-size 200)

;; ---------------------------------------------------------------------------
;; Parameter-space coercion
;; ---------------------------------------------------------------------------

(defn- contiguous-values->axis
  [name values]
  (let [values      (vec values)
        sorted-vals (vec (sort (distinct values)))]
    (when (empty? sorted-vals)
      (throw (ex-info "axis cannot be empty" {:axis name})))
    (let [start    (first sorted-vals)
          stop     (inc (peek sorted-vals))
          expected (vec (range start stop))]
      (when-not (= sorted-vals expected)
        (throw (ex-info "axis must be contiguous for now"
                        {:axis name
                         :values sorted-vals})))
      (params/param-axis name start stop))))

(defn space-from-mapping
  "Create a RunParameterSpace from an ordered mapping of axis name to values.

  Values may be any finite seq of integers, including `(range start stop)`. The
  values for each axis must currently be contiguous. Axis order follows the
  mapping's iteration order, so callers that care about rank should use an
  ordered map, array-map, or an explicit sequence of ParamAxis values."
  [spec]
  (params/run-parameter-space
   (mapv (fn [[name values]]
           (contiguous-values->axis name values))
         spec)))

(defn coerce-axis-spec
  "Coerce `axis-spec` into a RunParameterSpace.

  Accepted forms:

  * an existing RunParameterSpace
  * a map of axis name -> contiguous axis values
  * a sequence of ParamAxis records"
  [axis-spec]
  (cond
    (instance? chunk_memo.params.RunParameterSpace axis-spec)
    axis-spec

    (map? axis-spec)
    (space-from-mapping axis-spec)

    :else
    (let [axes (vec axis-spec)]
      (when-not (every? #(instance? chunk_memo.params.ParamAxis %) axes)
        (throw (ex-info "axis-spec must be a RunParameterSpace, a mapping of axis values, or a sequence of ParamAxis records"
                        {:axis-spec axis-spec})))
      (params/run-parameter-space axes))))

;; ---------------------------------------------------------------------------
;; Stable cache identity
;; ---------------------------------------------------------------------------

(defn- canonicalize
  [x]
  (cond
    (map? x)
    (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
          (map (fn [[k v]] [(canonicalize k) (canonicalize v)]) x))

    (set? x)
    (vec (sort-by pr-str (map canonicalize x)))

    (sequential? x)
    (mapv canonicalize x)

    :else
    x))

(defn stable-cache-id
  "Return a deterministic short SHA-256 cache id for a parameter map."
  [params]
  (let [payload (.getBytes (pr-str (canonicalize params)) "UTF-8")
        digest  (.digest (MessageDigest/getInstance "SHA-256") payload)
        hex     (format "%064x" (BigInteger. 1 digest))]
    (subs hex 0 16)))

;; ---------------------------------------------------------------------------
;; Public memo object
;; ---------------------------------------------------------------------------

(defrecord ChunkMemo [root space chunk-size params caches])

(defn chunk-memo
  "Create a public-facing memoization helper.

  `axis-spec` is coerced with `coerce-axis-spec`. `params` is an optional map of
  run-level parameters that should contribute to the cache identity but are not
  coordinate axes."
  ([root axis-spec]
   (chunk-memo root axis-spec {}))
  ([root axis-spec {:keys [chunk-size params]
                    :or   {chunk-size default-chunk-size
                           params {}}}]
   (->ChunkMemo (io/file root)
                (coerce-axis-spec axis-spec)
                chunk-size
                (or params {})
                (atom {}))))

(defn cache-for-params
  "Return the ChunkCache for `extra-params`, creating it if needed.

  The cache id is derived from the ChunkMemo's base params merged with
  `extra-params`. This mirrors the Python layer's separation between coordinate
  axes and non-axis run parameters."
  ([memo]
   (cache-for-params memo {}))
  ([{:keys [root space chunk-size params caches]} extra-params]
   (let [merged   (merge (or params {}) (or extra-params {}))
         cache-id (stable-cache-id merged)]
     (or (get @caches cache-id)
         (let [created (cache/chunk-cache (io/file root cache-id)
                                          space
                                          chunk-size)]
           (get (swap! caches #(if (contains? % cache-id)
                                 %
                                 (assoc % cache-id created)))
                cache-id))))))

(defn extract-axis-values
  "Extract semantic axis values from an argument map, in space rank order."
  [{:keys [space]} arguments]
  (mapv (fn [{:keys [name]}]
          (if (contains? arguments name)
            (get arguments name)
            (throw (ex-info "missing axis argument"
                            {:axis name
                             :arguments arguments}))))
        (:axes space)))

;; ---------------------------------------------------------------------------
;; Serialization defaults
;; ---------------------------------------------------------------------------

(defn default-serializer!
  "Write a result as EDN."
  [value file]
  (spit file (pr-str value)))

(defn default-deserializer
  "Read a result written by `default-serializer!`."
  [file]
  (edn/read-string (slurp file)))

(defn- move-replacing!
  [source target]
  (Files/move source target
              (into-array StandardCopyOption
                          [StandardCopyOption/REPLACE_EXISTING]))
  nil)

;; ---------------------------------------------------------------------------
;; Memoization wrapper
;; ---------------------------------------------------------------------------

(defn memoize
  "Return a memoizing wrapper around a single-argument function.

  The wrapped function should take one map of named arguments. Axis names in the
  ChunkMemo's space are read from that map to identify the coordinate point.
  Non-axis cache parameters are read from `params-key` and folded into the cache
  id.

  Options:

  * `:params-key`   key containing non-axis cache params, default `:params`
  * `:suffix`       payload suffix, default `.edn`
  * `:serializer!`  `(fn [value file])`, default writes EDN
  * `:deserializer` `(fn [file])`, default reads EDN

  Example:

  ```clojure
  (def memo
    (chunk-memo \"cache\"
                (array-map :x (range 0 10)
                           :y (range 0 20))))

  (def cached-work
    (memoize memo
             (fn [{:keys [x y]}]
               {:value (+ x y)})))

  (cached-work {:x 1 :y 2 :params {:model \"a\"}})
  ```"
  ([memo f]
   (memoize memo f {}))
  ([memo f {:keys [params-key suffix serializer! deserializer]
            :or   {params-key :params
                   suffix ".edn"
                   serializer! default-serializer!
                   deserializer default-deserializer}}]
   (fn [arguments]
     (when-not (map? arguments)
       (throw (ex-info "memoized functions expect a single argument map"
                       {:arguments arguments})))
     (let [params-payload (or (get arguments params-key) {})]
       (when-not (map? params-payload)
         (throw (ex-info "cache params must be a map"
                         {:params-key params-key
                          :value params-payload})))
       (let [chunk-cache (cache-for-params memo params-payload)
             axis-values (extract-axis-values memo arguments)
             path        (cache/result-path chunk-cache axis-values suffix)]
         (if (and (cache/contains? chunk-cache axis-values)
                  (.exists path))
           (deserializer path)
           (let [result (f arguments)
                 parent (.getParentFile path)
                 tmp    (io/file (str (.getPath path) ".tmp"))]
             (when parent
               (.mkdirs parent))
             (serializer! result tmp)
             (move-replacing! (.toPath tmp) (.toPath path))
             (cache/mark-complete! chunk-cache axis-values)
             result)))))))

(defn missing
  "Return parameter maps whose axis point is missing from the relevant cache."
  ([memo argument-maps]
   (missing memo argument-maps {}))
  ([memo argument-maps {:keys [params-key]
                        :or   {params-key :params}}]
   (filterv (fn [arguments]
              (let [params-payload (or (get arguments params-key) {})
                    chunk-cache    (cache-for-params memo params-payload)
                    axis-values    (extract-axis-values memo arguments)]
                (not (cache/contains? chunk-cache axis-values))))
            argument-maps)))
