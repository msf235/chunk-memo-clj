(ns chunk-memo.index.selection)

(defprotocol IndexSelection
  (contains-index? [sel i])
  (intersect-range [sel start end])
  (bounds [sel])
  (iter-intervals [sel])
  (covers-range? [sel start end])
  (count-between [sel start end])
  (rank-between [sel start i])
  (simplify [sel]))

(defrecord EmptySet []
  IndexSelection
  (contains-index? [_ _] false)
  (intersect-range [_ _ _] [])
  (bounds [_] nil)
  (iter-intervals [_] [])
  (covers-range? [_ start end] (>= start end))
  (count-between [_ _ _] 0)
  (rank-between [_ _ _] 0)
  (simplify [this] this))

(def empty-set (->EmptySet))

(defn empty-set? [x]
  (instance? EmptySet x))

(defn- valid-range! [start end]
  (when (> start end)
    (throw (ex-info "start must be <= end"
                    {:start start :end end}))))

(defn- add-range-to-intervals [intervals start end]
  (valid-range! start end)
  (if (= start end)
    intervals
    (let [step
          (fn [{:keys [new-start new-end inserted out]} [a b]]
            (cond
              (< b new-start)
              {:new-start new-start
               :new-end new-end
               :inserted inserted
               :out (conj out [a b])}

              (< new-end a)
              (if inserted
                {:new-start new-start
                 :new-end new-end
                 :inserted inserted
                 :out (conj out [a b])}
                {:new-start new-start
                 :new-end new-end
                 :inserted true
                 :out (conj out [new-start new-end] [a b])})

              :else
              {:new-start (min new-start a)
               :new-end (max new-end b)
               :inserted inserted
               :out out}))]

      (let [{:keys [new-start new-end inserted out]}
            (reduce step
                    {:new-start start
                     :new-end end
                     :inserted false
                     :out []}
                    intervals)]
        (cond-> out
          (not inserted) (conj [new-start new-end]))))))

(defrecord IntervalSet [intervals]
  IndexSelection
  (contains-index? [_ i]
    (boolean
     (some (fn [[a b]]
             (<= a i (dec b)))
           intervals)))

  (intersect-range [_ start end]
    (if (>= start end)
      []
      (->> intervals
           (take-while (fn [[a _]] (< a end)))
           (keep (fn [[a b]]
                   (when (> b start)
                     [(max a start) (min b end)])))
           vec)))

  (bounds [_]
    (when (seq intervals)
      [(ffirst intervals)
       (second (last intervals))]))

  (iter-intervals [_]
    (or intervals []))

  (covers-range? [_ start end]
    (or (>= start end)
        (some?
         (some (fn [[a b]]
                 (and (<= a start)
                      (<= end b)))
               intervals))))

  (count-between [this start end]
    (if (>= start end)
      0
      (reduce +
              (map (fn [[a b]] (- b a))
                   (intersect-range this start end)))))

  (rank-between [this start i]
    (if (<= i start)
      0
      (count-between this start i)))

  (simplify [this]
    (if (seq intervals) this empty-set)))

(defn interval-set
  ([] (->IntervalSet []))
  ([intervals]
   (->IntervalSet
    (reduce (fn [acc [start end]]
              (add-range-to-intervals acc start end))
            []
            intervals))))

(defn with-range [intervals start end]
  (interval-set
   (add-range-to-intervals (:intervals intervals) start end)))

(defn with-point [intervals x]
  (with-range intervals x (inc x)))

(defn interval-set? [x]
  (instance? IntervalSet x))

(defrecord StridedSet [start end step]
  IndexSelection
  (contains-index? [_ i]
    (and (<= start i)
         (< i end)
         (zero? (mod (- i start) step))))

  (intersect-range [_ lo hi]
    (if (>= lo hi)
      []
      (let [lo' (max lo start)
            hi' (min hi end)]
        (if (>= lo' hi')
          []
          (let [k (max 0
                       (quot (+ (- lo' start) step -1)
                             step))
                first (+ start (* k step))]
            (cond
              (>= first hi')
              []

              (= step 1)
              [[first hi']]

              :else
              (mapv (fn [x] [x (inc x)])
                    (range first hi' step))))))))

  (bounds [_]
    [start end])

  (iter-intervals [this]
    (intersect-range this start end))

  (covers-range? [this lo hi]
    (cond
      (>= lo hi) true
      (= 1 (- hi lo)) (contains-index? this lo)
      :else (and (= step 1)
                 (<= start lo)
                 (<= hi end))))

  (count-between [_ lo hi]
    (if (>= lo hi)
      0
      (let [lo' (max lo start)
            hi' (min hi end)]
        (if (>= lo' hi')
          0
          (let [k (max 0
                       (quot (+ (- lo' start) step -1)
                             step))
                first (+ start (* k step))]
            (if (>= first hi')
              0
              (inc (quot (- hi' first 1) step))))))))

  (rank-between [this lo i]
    (if (<= i lo)
      0
      (count-between this lo i)))

  (simplify [this] this))

(defn strided-set [start end step]
  (when (<= step 0)
    (throw (ex-info "step must be positive" {:step step})))
  (when (>= start end)
    (throw (ex-info "start must be < end"
                    {:start start :end end})))
  (->StridedSet start end step))

(defn strided-set? [x]
  (instance? StridedSet x))

(defrecord UnionSet [parts]
  IndexSelection
  (contains-index? [_ i]
    (boolean
     (some #(contains-index? % i) parts)))

  (intersect-range [_ start end]
    (if (>= start end)
      []
      (:intervals
       (reduce
        (fn [acc part]
          (reduce
           (fn [acc' [a b]]
             (with-range acc' a b))
           acc
           (intersect-range part start end)))
        (interval-set)
        parts))))

  (bounds [_]
    (let [bs (keep bounds parts)]
      (when (seq bs)
        [(apply min (map first bs))
         (apply max (map second bs))])))

  (iter-intervals [this]
    (if-let [[a b] (bounds this)]
      (intersect-range this a b)
      []))

  (covers-range? [this start end]
    (let [xs (intersect-range this start end)]
      (and (= 1 (count xs))
           (= [start end] (first xs)))))

  (count-between [this start end]
    (reduce +
            (map (fn [[a b]] (- b a))
                 (intersect-range this start end))))

  (rank-between [this start i]
    (if (<= i start)
      0
      (count-between this start i)))

  (simplify [_]
    (let [flat
          (->> parts
               (map simplify)
               (remove empty-set?)
               (mapcat #(if (instance? UnionSet %)
                          (:parts %)
                          [%]))
               vec)]
      (cond
        (empty? flat)
        empty-set

        (= 1 (count flat))
        (first flat)

        :else
        (let [merged
              (reduce
               (fn [acc part]
                 (reduce
                  (fn [acc' [a b]]
                    (with-range acc' a b))
                  acc
                  (iter-intervals part)))
               (interval-set)
               flat)]
          (if (bounds merged)
            merged
            (->UnionSet flat)))))))

(defn union-set [& parts]
  (->UnionSet (vec parts)))

(defrecord IntersectionSet [parts]
  IndexSelection
  (contains-index? [_ i]
    (every? #(contains-index? % i) parts))

  (intersect-range [_ start end]
    (if (>= start end)
      []
      (:intervals
       (reduce
        (fn [current part]
          (reduce
           (fn [next-current [a b]]
             (reduce
              (fn [acc [x y]]
                (with-range acc x y))
              next-current
              (intersect-range part a b)))
           (interval-set)
           (:intervals current)))
        (interval-set [[start end]])
        parts))))

  (bounds [_]
    (let [bs (map bounds parts)]
      (if (some nil? bs)
        nil
        (let [lo (apply max (map first bs))
              hi (apply min (map second bs))]
          (when (< lo hi)
            [lo hi])))))

  (iter-intervals [this]
    (if-let [[a b] (bounds this)]
      (intersect-range this a b)
      []))

  (covers-range? [_ start end]
    (every? #(covers-range? % start end) parts))

  (count-between [this start end]
    (reduce +
            (map (fn [[a b]] (- b a))
                 (intersect-range this start end))))

  (rank-between [this start i]
    (if (<= i start)
      0
      (count-between this start i)))

  (simplify [this]
    this))

(defn intersection-set [& parts]
  (when (empty? parts)
    (throw (ex-info "IntersectionSet requires at least one part" {})))
  (->IntersectionSet (vec parts)))

(defrecord DifferenceSet [base remove]
  IndexSelection
  (contains-index? [_ i]
    (and (contains-index? base i)
         (not (contains-index? remove i))))

  (intersect-range [_ start end]
    (if (>= start end)
      []
      (:intervals
       (reduce
        (fn [result [a b]]
          (loop [cursor a
                 removals (intersect-range remove a b)
                 result result]
            (if-let [[x y] (first removals)]
              (recur (max cursor y)
                     (rest removals)
                     (if (< cursor x)
                       (with-range result cursor x)
                       result))
              (if (< cursor b)
                (with-range result cursor b)
                result))))
        (interval-set)
        (intersect-range base start end)))))

  (bounds [_]
    (bounds base))

  (iter-intervals [this]
    (if-let [[a b] (bounds this)]
      (intersect-range this a b)
      []))

  (covers-range? [this start end]
    (let [xs (intersect-range this start end)]
      (and (= 1 (count xs))
           (= [start end] (first xs)))))

  (count-between [this start end]
    (reduce +
            (map (fn [[a b]] (- b a))
                 (intersect-range this start end))))

  (rank-between [this start i]
    (if (<= i start)
      0
      (count-between this start i)))

  (simplify [this]
    this))

(defn difference-set [base remove]
  (->DifferenceSet base remove))
