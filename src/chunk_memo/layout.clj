(ns chunk-memo.layout
  (:require [chunk-memo.coord :as coord]
            [chunk-memo.index.selection :as index]))

(defn- row-major-strides [shape]
  (->> (rest shape)
       reverse
       (reductions * 1)
       rest
       reverse
       vec
       (#(conj % 1))))

(defrecord RowMajorLayout [shape strides size rank])

(defn row-major-layout [shape]
  (let [shape (vec shape)]
    (when (empty? shape)
      (throw (ex-info "shape must have at least one dimension" {:shape shape})))
    (when (some #(<= % 0) shape)
      (throw (ex-info "all dimensions must be positive" {:shape shape})))
    (->RowMajorLayout
     shape
     (row-major-strides shape)
     (reduce * 1 shape)
     (count shape))))

(defn axis-full? [ax dim]
  (and (= 0 (coord/min-value ax))
       (= (dec dim) (coord/max-value ax))
       (= dim (coord/axis-size ax))))

(defn axis-single-value [ax]
  (when (= 1 (coord/axis-size ax))
    (coord/min-value ax)))

(defn validate-product! [layout product]
  (let [{:keys [shape rank]} layout
        axes (coord/axes product)]
    (when-not (= rank (count axes))
      (throw
       (ex-info "product rank does not match layout rank"
                {:product-rank (count axes)
                 :layout-rank rank})))

    (doseq [[i ax dim] (map vector (range) axes shape)]
      (when (or (< (coord/min-value ax) 0)
                (>= (coord/max-value ax) dim))
        (throw
         (ex-info "axis bounds out of shape"
                   {:axis i
                    :bounds [(coord/min-value ax) (coord/max-value ax)]
                    :dimension-size dim}))))))

(declare validate-coord-selection!)

(defn validate-coord-selection! [layout selection]
  (let [selection (coord/simplify selection)]
    (cond
      (coord/coord-empty? selection)
      nil

      (coord/coord-product? selection)
      (validate-product! layout selection)

      (coord/coord-union? selection)
      (doseq [part (coord/parts selection)]
        (validate-coord-selection! layout part))

      (coord/coord-intersection? selection)
      (doseq [part (coord/parts selection)]
        (validate-coord-selection! layout part))

      (coord/coord-difference? selection)
      (do
        (validate-coord-selection! layout (coord/difference-base selection))
        (validate-coord-selection! layout (coord/difference-remove selection)))

      :else
      (throw
       (ex-info "Cannot validate selection symbolically"
                {:selection-type (type selection)})))))

(defn coord->index [layout coord]
  (let [{:keys [shape strides rank]} layout
        coord (vec coord)]
    (when-not (= rank (count coord))
      (throw
       (ex-info "coord rank mismatch"
                {:coord-rank (count coord)
                 :layout-rank rank})))

    (reduce +
            (map
             (fn [v dim stride]
               (when-not (<= 0 v (dec dim))
                 (throw
                  (ex-info "coord out of bounds"
                           {:coord coord
                            :shape shape})))
               (* v stride))
             coord shape strides))))

(defn index->coord [layout sweep-id]
  (let [{:keys [shape strides size]} layout]
    (when-not (<= 0 sweep-id (dec size))
      (throw
       (ex-info "sweep id out of bounds"
                {:sweep-id sweep-id
                 :size size})))

    (loop [dims shape
           strides strides
           rem sweep-id
           out []]
      (if-let [stride (first strides)]
        (let [v (quot rem stride)]
          (recur (rest dims)
                 (rest strides)
                 (mod rem stride)
                 (conj out v)))
        out))))

(defn- whole-layout? [layout product]
  (every? true?
           (map axis-full?
                (coord/axes product)
                (:shape layout))))

(defn- suffix-sizes [layout]
  (->> (:shape layout)
       reverse
       (reductions * 1)
       rest
       reverse
       vec))

(defn- suffix-full-flags [layout product]
  (->> (map axis-full? (coord/axes product) (:shape layout))
       reverse
       (reductions #(and %1 %2) true)
       rest
       reverse
       vec))

(defn- try-fixed-last-axis [layout product]
  (let [{:keys [rank shape size]} layout
        axes (coord/axes product)
        last-axis-index (dec rank)]
    (when (every?
           true?
           (map-indexed
            (fn [i ax]
              (if (= i last-axis-index)
                (some? (axis-single-value ax))
                (axis-full? ax (nth shape i))))
            axes))
      (let [selected-last (axis-single-value (nth axes last-axis-index))
            step (last shape)
            start selected-last
            end (inc (+ (- size step) selected-last))]
        (index/strided-set start end step)))))

(defn- compile-product->intervals [layout product]
  (let [{:keys [rank strides]} layout
        suffix-full (suffix-full-flags layout product)
        suffix-size (suffix-sizes layout)]
    (letfn [(rec [axis-i base-id]
              (cond
                (= axis-i rank)
                [[base-id (inc base-id)]]

                (nth suffix-full axis-i)
                [[base-id (+ base-id (nth suffix-size axis-i))]]

                :else
                (let [ax (nth (coord/axes product) axis-i)
                      stride (nth strides axis-i)]
                  (mapcat
                   (fn [pos]
                     (rec (inc axis-i)
                          (+ base-id (* pos stride))))
                    (coord/axis-values ax)))))]
      (index/interval-set (rec 0 0)))))

(defn- coord-product->index [layout product]
  (cond
    (whole-layout? layout product)
    (index/interval-set [[0 (:size layout)]])

    :else
    (or (try-fixed-last-axis layout product)
        (compile-product->intervals layout product))))

(declare coord-selection->index-unchecked)

(defn- compile-by-enumeration [layout selection max-enumeration]
  (when (> (:size layout) max-enumeration)
    (throw
     (ex-info "Cannot compile selection symbolically; layout exceeds max-enumeration"
              {:selection-type (type selection)
               :layout-size (:size layout)
               :max-enumeration max-enumeration})))

  (index/interval-set
   (for [sweep-id (range (:size layout))
         :let [coord (index->coord layout sweep-id)]
         :when (coord/contains-coord? selection coord)]
     [sweep-id (inc sweep-id)])))

(defn- coord-selection->index-unchecked
  ([layout selection]
   (coord-selection->index-unchecked layout selection 1000000))
  ([layout selection max-enumeration]
   (cond
      (coord/coord-empty? selection)
      index/empty-set

      (coord/coord-product? selection)
      (coord-product->index layout selection)

      (coord/coord-union? selection)
      (index/simplify
       (apply index/union-set
              (map #(coord-selection->index-unchecked layout % max-enumeration)
                   (coord/parts selection))))

      (coord/coord-intersection? selection)
      (index/simplify
       (apply index/intersection-set
              (map #(coord-selection->index-unchecked layout % max-enumeration)
                   (coord/parts selection))))

      (coord/coord-difference? selection)
      (index/simplify
       (index/difference-set
        (coord-selection->index-unchecked layout (coord/difference-base selection) max-enumeration)
        (coord-selection->index-unchecked layout (coord/difference-remove selection) max-enumeration)))

      :else
      (compile-by-enumeration layout selection max-enumeration))))

(defn coord-selection->index
  ([layout selection]
   (coord-selection->index layout selection 1000000))
  ([layout selection max-enumeration]
   (let [selection (coord/simplify selection)]
     (validate-coord-selection! layout selection)
     (coord-selection->index-unchecked layout selection max-enumeration))))
