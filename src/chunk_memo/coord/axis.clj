(ns chunk-memo.coord.axis)

;; -----------------------------------------------------------------------------
;; Axis protocol
;; -----------------------------------------------------------------------------

(defprotocol Axis
  (contains-value? [axis v])
  (axis-values [axis])
  (axis-size [axis])
  (min-value [axis])
  (max-value [axis]))

;; -----------------------------------------------------------------------------
;; Axis implementations
;; -----------------------------------------------------------------------------

(defrecord IntSetAxis [vals val-set]
  Axis
  (contains-value? [_ v]
    (contains? val-set v))

  (axis-values [_]
    vals)

  (axis-size [_]
    (count vals))

  (min-value [_]
    (first vals))

  (max-value [_]
    (last vals)))

(defn int-set-axis [xs]
  (let [vals (->> xs seq distinct sort vec)]
    (when (empty? vals)
      (throw (ex-info "axis cannot be empty" {})))
    (->IntSetAxis vals (set vals))))

(defrecord RangeAxis [start stop]
  Axis
  (contains-value? [_ v]
    (<= start v (dec stop)))

  (axis-values [_]
    (range start stop))

  (axis-size [_]
    (- stop start))

  (min-value [_]
    start)

  (max-value [_]
    (dec stop)))

(defn range-axis [start stop]
  (when (<= stop start)
    (throw (ex-info "RangeAxis must be non-empty"
                    {:start start :stop stop})))
  (->RangeAxis start stop))

(defrecord StridedAxis [start stop step]
  Axis
  (contains-value? [_ v]
    (and (<= start v)
         (< v stop)
         (zero? (mod (- v start) step))))

  (axis-values [_]
    (range start stop step))

  (axis-size [_]
    (long (Math/ceil (/ (- stop start)
                        (double step)))))

  (min-value [_]
    start)

  (max-value [this]
    (+ start (* (dec (axis-size this)) step))))

(defn strided-axis [start stop step]
  (when (<= step 0)
    (throw (ex-info "step must be positive"
                    {:step step})))

  (when (<= stop start)
    (throw (ex-info "StridedAxis must be non-empty"
                    {:start start :stop stop})))

  (let [axis (->StridedAxis start stop step)]
    (when (zero? (axis-size axis))
      (throw (ex-info "StridedAxis must contain at least one value"
                      {:start start
                       :stop stop
                       :step step})))
    axis))

;; -----------------------------------------------------------------------------
;; Axis normalization
;; -----------------------------------------------------------------------------

(defn axis? [x]
  (satisfies? Axis x))

(defn normalize-axis [x]
  (cond
    (axis? x)
    x

    (integer? x)
    (int-set-axis [x])

    (and (vector? x)
         (= 4 (count x))
         (= :stride (first x)))
    (let [[_ start stop step] x]
      (strided-axis start stop step))

    (sequential? x)
    (int-set-axis x)

    :else
    (throw (ex-info "Cannot normalize axis"
                    {:value x}))))
