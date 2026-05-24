(ns chunk-memo.params
  (:require [chunk-memo.coord :as coord]
            [chunk-memo.coord.axis :as axis]
            [chunk-memo.layout :as layout]))

(defrecord ParamAxis [name start stop])

(defn param-axis
  "Create a semantic parameter axis.

  Values on this axis live in the half-open interval `[start stop)`. Selection
  specs supplied through this namespace are expressed in those semantic values
  and are translated to zero-based positional coordinates before reaching the
  coord/layout layer."
  [name start stop]
  (when-not (< start stop)
    (throw (ex-info "parameter axis must be non-empty"
                    {:name name :start start :stop stop})))
  (->ParamAxis name start stop))

(defn axis-size
  "Return the number of semantic values in `param-axis`."
  [{:keys [start stop]}]
  (- stop start))

(defn value->pos
  "Translate a semantic parameter value to its zero-based coordinate position."
  [{:keys [name start stop] :as param-axis} value]
  (let [pos (- value start)]
    (when-not (<= 0 pos (dec (axis-size param-axis)))
      (throw (ex-info "parameter value outside axis bounds"
                      {:axis name
                       :value value
                       :bounds [start stop]})))
    pos))

(defn- range->pos-axis [param-axis start stop]
  (when-not (< start stop)
    (throw (ex-info "range selection must be non-empty"
                    {:axis (:name param-axis)
                     :start start
                     :stop stop})))
  (coord/range-axis
   (value->pos param-axis start)
   (inc (value->pos param-axis (dec stop)))))

(defn- strided->pos-axis [param-axis start stop step]
  (when-not (pos? step)
    (throw (ex-info "step must be positive"
                    {:axis (:name param-axis)
                     :step step})))
  (when-not (< start stop)
    (throw (ex-info "strided selection must be non-empty"
                    {:axis (:name param-axis)
                     :start start
                     :stop stop
                     :step step})))
  (coord/strided-axis
   (value->pos param-axis start)
   (inc (value->pos param-axis (dec stop)))
   step))

(defn axis->pos
  "Translate a semantic axis spec into a positional AxisSelection.

  Supported specs:

  * integer value, e.g. `3`
  * positional axis object from `chunk-memo.coord.axis`, passed through unchanged
  * half-open semantic range vector `[start stop]`
  * explicit semantic range vector `[:range start stop]`
  * explicit semantic strided range vector `[:stride start stop step]`
  * map with `:start`, `:stop`, and optional `:step`
  * any other sequential collection of individual semantic values"
  [param-axis spec]
  (cond
    (integer? spec)
    (coord/int-set-axis [(value->pos param-axis spec)])

    (axis/axis? spec)
    spec

    (and (vector? spec)
         (= 2 (count spec))
         (every? integer? spec))
    (let [[start stop] spec]
      (range->pos-axis param-axis start stop))

    (and (vector? spec)
         (= :range (first spec))
         (= 3 (count spec)))
    (let [[_ start stop] spec]
      (range->pos-axis param-axis start stop))

    (and (vector? spec)
         (= :stride (first spec))
         (= 4 (count spec)))
    (let [[_ start stop step] spec]
      (strided->pos-axis param-axis start stop step))

    (map? spec)
    (let [{:keys [start stop step]} spec]
      (when (or (nil? start) (nil? stop))
        (throw (ex-info "map axis spec requires :start and :stop"
                        {:axis (:name param-axis)
                         :spec spec})))
      (if step
        (strided->pos-axis param-axis start stop step)
        (range->pos-axis param-axis start stop)))

    (sequential? spec)
    (coord/int-set-axis
     (map #(value->pos param-axis %) spec))

    :else
    (throw (ex-info "Cannot translate axis spec"
                    {:axis (:name param-axis)
                     :spec spec}))))

(defrecord RunParameterSpace [axes layout])

(defn run-parameter-space
  "Create a parameter space from an ordered collection of ParamAxis values."
  [axes]
  (let [axes (vec axes)]
    (when (empty? axes)
      (throw (ex-info "parameter space requires at least one axis" {})))
    (->RunParameterSpace
     axes
     (layout/row-major-layout (mapv axis-size axes)))))

(defn product
  "Create a CoordProduct from semantic per-axis specs.

  `specs` must have the same rank and order as the parameter-space axes."
  [{:keys [axes]} specs]
  (let [specs (vec specs)]
    (when-not (= (count specs) (count axes))
      (throw (ex-info "wrong rank"
                      {:expected (count axes)
                       :actual (count specs)})))
    (coord/coord-product
     (mapv axis->pos axes specs))))

(defn point
  "Create a single-point CoordProduct from semantic parameter values."
  [space values]
  (product space values))

(defn axis-range
  "Select a semantic half-open range along one axis, with all other axes fixed.

  `fixed` is a map from axis name to semantic value for every non-selected axis."
  [{:keys [axes] :as space} axis-name start stop fixed]
  (product
   space
   (mapv (fn [{:keys [name]}]
           (cond
             (= name axis-name)
             [start stop]

             (contains? fixed name)
             (get fixed name)

             :else
             (throw (ex-info "missing fixed value for axis"
                             {:axis name
                              :fixed fixed}))))
         axes)))

(defn union
  "Return the simplified union of coordinate selections."
  [& parts]
  (coord/simplify
   (apply coord/coord-union parts)))

(defn intersection
  "Return the simplified intersection of coordinate selections."
  [& parts]
  (coord/simplify
   (apply coord/coord-intersection parts)))

(defn difference
  "Return the simplified difference `base - remove`."
  [base remove]
  (coord/simplify
   (coord/coord-difference base remove)))

(defn to-index
  "Compile a coordinate selection into an IndexSelection using this space's layout."
  [{:keys [layout]} selection]
  (layout/coord-selection->index layout selection))
