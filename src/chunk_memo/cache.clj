(ns chunk-memo.cache
  (:refer-clojure :exclude [contains?])
  (:require [chunk-memo.bitmap :as bitmap]
            [chunk-memo.chunks :as chunks]
            [chunk-memo.index.selection :as selection]
            [chunk-memo.layout :as layout]
            [chunk-memo.params :as params]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files StandardCopyOption]))

(def default-chunk-size 200)

(defrecord LogicalChunkCache [root index-root payload-root space chunk-size chunk-spec])

(defn- mkdirs!
  [file]
  (.mkdirs (io/file file))
  file)

(defn- axis-name->str
  [axis-name]
  (cond
    (keyword? axis-name) (name axis-name)
    (symbol? axis-name)  (name axis-name)
    :else                (str axis-name)))

(defn- json-string
  [x]
  (str "\""
       (-> (str x)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\"")
           (str/replace "\b" "\\b")
           (str/replace "\f" "\\f")
           (str/replace "\n" "\\n")
           (str/replace "\r" "\\r")
           (str/replace "\t" "\\t"))
       "\""))

(defn- meta-json
  [{:keys [space chunk-size]}]
  (let [layout (:layout space)
        axes   (:axes space)]
    (str "{\n"
         "  \"chunk_size\": " chunk-size ",\n"
         "  \"shape\": [" (str/join ", " (:shape layout)) "],\n"
         "  \"axes\": [\n"
         (->> axes
              (map (fn [{:keys [name start stop]}]
                     (str "    {\"name\": " (json-string (axis-name->str name))
                          ", \"start\": " start
                          ", \"stop\": " stop "}")))
              (str/join ",\n"))
         "\n  ]\n"
         "}\n")))

(defn- write-meta-once!
  [{:keys [root] :as cache}]
  (let [path (io/file root "meta.json")]
    (when-not (.exists path)
      (spit path (meta-json cache)))
    cache))

(defn chunk-cache
  "Create a filesystem-backed chunk-completion cache.

  The cache stores chunk-local completion bitmaps below `root/index` and exposes
  stable payload paths below `root/payloads`. Payload contents are owned by user
  code; this namespace only plans those paths and tracks completion metadata."
  ([root space]
   (chunk-cache root space default-chunk-size))
  ([root space chunk-size]
   (let [root         (io/file root)
         index-root   (io/file root "index")
         payload-root (io/file root "payloads")
         chunk-spec   (chunks/chunk-spec chunk-size (get-in space [:layout :size]))
         cache        (->LogicalChunkCache root index-root payload-root
                                           space chunk-size chunk-spec)]
     (mkdirs! index-root)
     (mkdirs! payload-root)
     (write-meta-once! cache))))

;; ---------------------------------------------------------------------------
;; Paths
;; ---------------------------------------------------------------------------

(defn chunk-path
  "Return the index bitmap path for `chunk-id`."
  [{:keys [index-root]} chunk-id]
  (io/file index-root (format "chunk_%020d.bin" chunk-id)))

(defn result-path
  "Return the stable payload path for a semantic parameter point.

  The cache does not read or write this file. It only provides a deterministic
  location for user code to store the corresponding result payload."
  ([cache params]
   (result-path cache params ".bin"))
  ([{:keys [payload-root space]} params suffix]
   (let [parts    (map (fn [{:keys [name]} value]
                         (str (axis-name->str name) "=" value))
                       (:axes space)
                       params)
         filename (str (str/join "__" parts) suffix)]
     (io/file payload-root filename))))

;; ---------------------------------------------------------------------------
;; Chunk bitmap IO
;; ---------------------------------------------------------------------------

(defn load-chunk
  "Read a chunk bitmap, returning an empty bitmap for missing chunks."
  [cache chunk-id]
  (let [path (chunk-path cache chunk-id)]
    (if (.exists path)
      (bitmap/deserialize (Files/readAllBytes (.toPath path)))
      bitmap/empty-bitmap)))

(defn- move-replacing!
  [source target]
  (Files/move source target
              (into-array StandardCopyOption
                          [StandardCopyOption/REPLACE_EXISTING]))
  nil)

(defn store-chunk!
  "Persist `bitmap` for `chunk-id` via a temporary file and replace."
  [cache chunk-id bm]
  (let [path (chunk-path cache chunk-id)
        tmp  (io/file (str (.getPath path) ".tmp"))]
    (Files/write (.toPath tmp) (bitmap/serialize bm)
                 (make-array java.nio.file.OpenOption 0))
    (move-replacing! (.toPath tmp) (.toPath path))))

;; ---------------------------------------------------------------------------
;; Coordinate translation
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

(defn params->index
  "Translate semantic parameter values into a flat row-major sweep index."
  [{:keys [space] :as cache} param-values]
  (layout/coord->index (:layout space) (params->pos cache param-values)))

;; ---------------------------------------------------------------------------
;; Query API
;; ---------------------------------------------------------------------------

(defn contains?
  "Return true when the semantic parameter point has been marked complete."
  [{:keys [chunk-spec] :as cache} param-values]
  (let [sweep-id          (params->index cache param-values)
        [chunk-id offset] (chunks/index->chunk-offset chunk-spec sweep-id)]
    (bitmap/contains-value? (load-chunk cache chunk-id) offset)))

(defn- bitmap-covers-offset-range?
  [bm start end]
  (or (>= start end)
      (let [needed (bitmap/add-range bitmap/empty-bitmap start end)]
        (= (bitmap/cardinality needed)
           (bitmap/cardinality (bitmap/intersection bm needed))))))

(defn- chunk-segments
  "Return `[chunk-id offset-start offset-end]` segments for flat interval `[start end)`."
  [chunk-size start end]
  (loop [i start
         out []]
    (if (>= i end)
      out
      (let [chunk-id     (quot i chunk-size)
            chunk-start  (* chunk-id chunk-size)
            chunk-end    (min end (+ chunk-start chunk-size))
            offset-start (- i chunk-start)
            offset-end   (- chunk-end chunk-start)]
        (recur chunk-end
               (conj out [chunk-id offset-start offset-end]))))))

(defn contains-selection?
  "Return true only if every point in `selection` is marked complete."
  [{:keys [space chunk-size] :as cache} coord-selection]
  (let [index-selection (layout/coord-selection->index (:layout space)
                                                       coord-selection)]
    (every?
     (fn [[start end]]
       (every? (fn [[chunk-id offset-start offset-end]]
                 (bitmap-covers-offset-range?
                  (load-chunk cache chunk-id)
                  offset-start
                  offset-end))
               (chunk-segments chunk-size start end)))
     (selection/iter-intervals index-selection))))

(defn status-for-chunk
  "Return `:empty`, `:partial`, or `:complete` for `chunk-id`."
  [{:keys [chunk-spec] :as cache} chunk-id]
  (chunks/chunk-status (load-chunk cache chunk-id) chunk-spec chunk-id))

;; ---------------------------------------------------------------------------
;; Marking API
;; ---------------------------------------------------------------------------

(defn mark-complete!
  "Mark a single semantic parameter point complete."
  [{:keys [chunk-spec] :as cache} param-values]
  (let [sweep-id          (params->index cache param-values)
        [chunk-id offset] (chunks/index->chunk-offset chunk-spec sweep-id)
        bm                (-> (load-chunk cache chunk-id)
                              (bitmap/add offset))]
    (store-chunk! cache chunk-id bm)))

(defn mark-selection-complete!
  "Mark every point in `coord-selection` complete."
  [{:keys [space chunk-spec] :as cache} coord-selection]
  (let [index-selection (layout/coord-selection->index (:layout space)
                                                       coord-selection)
        updates         (reduce (fn [acc [start end]]
                                  (chunks/add-flat-range acc start end chunk-spec))
                                {}
                                (selection/iter-intervals index-selection))]
    (doseq [[chunk-id update] updates]
      (store-chunk! cache
                    chunk-id
                    (bitmap/union (load-chunk cache chunk-id) update))))
  nil)

;; ---------------------------------------------------------------------------
;; Convenience
;; ---------------------------------------------------------------------------

(defn missing
  "Return parameter points from `params-list` that are not yet complete."
  [cache params-list]
  (filterv #(not (contains? cache %)) params-list))
