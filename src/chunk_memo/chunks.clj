(ns chunk-memo.chunks
  (:require [chunk-memo.bitmap :as bitmap]
            [chunk-memo.index.selection :as selection]))

(def chunk-statuses #{:empty :partial :complete})

(defrecord ChunkSpec [chunk-size total-size])

(defrecord Chunk [chunk-id offsets])

(defn chunk-ref
  "Create a chunk specifier.

  `offsets` is either nil, meaning the whole chunk, or a vector of chunk-local
  offsets. No validation is performed here; this is a lightweight internal
  value used by execution layers."
  ([chunk-id]
   (->Chunk chunk-id nil))
  ([chunk-id offsets]
   (->Chunk chunk-id offsets)))

(defn chunk-spec
  "Create a validated chunking specification.

  `chunk-size` is the maximum number of flat sweep indices in each chunk.
  `total-size` is the total number of flat sweep indices in the full layout."
  [chunk-size total-size]
  (when-not (pos? chunk-size)
    (throw (ex-info "chunk-size must be positive"
                    {:chunk-size chunk-size})))
  (when (neg? total-size)
    (throw (ex-info "total-size must be non-negative"
                    {:total-size total-size})))
  (->ChunkSpec chunk-size total-size))

(defn num-chunks
  "Return the number of chunks needed by `spec`."
  [{:keys [chunk-size total-size]}]
  (if (zero? total-size)
    0
    (quot (+ total-size chunk-size -1) chunk-size)))

(defn- valid-chunk-id!
  [spec chunk-id]
  (when-not (<= 0 chunk-id (dec (num-chunks spec)))
    (throw (ex-info "chunk-id out of bounds"
                    {:chunk-id chunk-id
                     :num-chunks (num-chunks spec)}))))

(defn chunk-bounds
  "Return the flat half-open interval `[start end)` covered by `chunk-id`."
  [{:keys [chunk-size total-size] :as spec} chunk-id]
  (valid-chunk-id! spec chunk-id)
  (let [start (* chunk-id chunk-size)
        end   (min (+ start chunk-size) total-size)]
    [start end]))

(defn chunk-capacity
  "Return the number of possible offsets in `chunk-id`."
  [spec chunk-id]
  (let [[start end] (chunk-bounds spec chunk-id)]
    (- end start)))

(defn index->chunk-offset
  "Convert a flat sweep index into `[chunk-id offset]`."
  [{:keys [chunk-size total-size]} sweep-id]
  (when-not (<= 0 sweep-id (dec total-size))
    (throw (ex-info "sweep-id out of bounds"
                    {:sweep-id sweep-id
                     :total-size total-size})))
  [(quot sweep-id chunk-size) (mod sweep-id chunk-size)])

(defn- valid-flat-range!
  [{:keys [total-size]} start end]
  (when (> start end)
    (throw (ex-info "start must be <= end"
                    {:start start :end end})))
  (when-not (and (<= 0 start) (<= end total-size))
    (throw (ex-info "range outside total size"
                    {:start start
                     :end end
                     :total-size total-size}))))

(defn add-flat-range
  "Return `chunks` with flat interval `[start end)` added to chunk-local bitmaps.

  `chunks` is a map of `chunk-id -> BitMap`, where each bitmap stores offsets
  local to that chunk. The input map and bitmaps are not mutated."
  [chunks start end {:keys [chunk-size total-size] :as spec}]
  (valid-flat-range! spec start end)
  (loop [i start
         chunks chunks]
    (if (>= i end)
      chunks
      (let [chunk-id     (quot i chunk-size)
            chunk-start  (* chunk-id chunk-size)
            chunk-end    (min end (+ chunk-start chunk-size) total-size)
            offset-start (- i chunk-start)
            offset-end   (- chunk-end chunk-start)]
        (recur chunk-end
               (update chunks
                       chunk-id
                       (fnil bitmap/add-range bitmap/empty-bitmap)
                       offset-start
                       offset-end))))))

(defn index-selection->chunk-bitmaps
  "Compile an IndexSelection into chunk-local bitmaps.

  Returns a map of `chunk-id -> BitMap(offsets)`."
  [selection spec]
  (reduce (fn [chunks [start end]]
            (add-flat-range chunks start end spec))
          {}
          (selection/iter-intervals selection)))

(defn chunk-status
  "Return `:empty`, `:partial`, or `:complete` for a chunk bitmap."
  [bitmap spec chunk-id]
  (let [n (if bitmap (bitmap/cardinality bitmap) 0)]
    (cond
      (zero? n) :empty
      (= n (chunk-capacity spec chunk-id)) :complete
      :else :partial)))

(defn merge-chunk-bitmaps
  "Return the union of two chunk bitmap maps."
  [base update]
  (merge-with bitmap/union base update))

(defn add-index-selection
  "Return `chunks` with every index in `selection` added.

  This is the immutable Clojure counterpart to the Python mutating helper."
  [chunks selection spec]
  (reduce (fn [chunks [start end]]
            (add-flat-range chunks start end spec))
          chunks
          (selection/iter-intervals selection)))
