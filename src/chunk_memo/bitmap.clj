(ns chunk-memo.bitmap
  (:import [java.math BigInteger]))

(defrecord BitMap [bits])

(def empty-bitmap
  (->BitMap BigInteger/ZERO))

(declare add)

(defn bitmap
  ([] empty-bitmap)
  ([values]
   (reduce add empty-bitmap values)))

(defn add [bm value]
  (when (neg? value)
    (throw (ex-info "bitmap values must be non-negative"
                    {:value value})))
  (update bm :bits #(.setBit ^BigInteger % value)))

(defn add-range [bm start end]
  (when (> start end)
    (throw (ex-info "start must be <= end"
                    {:start start :end end})))
  (when (neg? start)
    (throw (ex-info "bitmap values must be non-negative"
                    {:start start})))
  (if (= start end)
    bm
    (let [width (- end start)
          mask  (.shiftLeft (.subtract (.shiftLeft BigInteger/ONE width)
                                       BigInteger/ONE)
                            start)]
      (update bm :bits #(.or ^BigInteger % mask)))))

(defn contains-value? [bm value]
  (and (not (neg? value))
       (.testBit ^BigInteger (:bits bm) value)))

(defn cardinality [bm]
  (.bitCount ^BigInteger (:bits bm)))

(defn union [a b]
  (->BitMap (.or ^BigInteger (:bits a) (:bits b))))

(defn intersection [a b]
  (->BitMap (.and ^BigInteger (:bits a) (:bits b))))

(defn difference [a b]
  (->BitMap (.andNot ^BigInteger (:bits a) (:bits b))))

(defn bitmap-values [bm]
  (letfn [(step [bits]
            (lazy-seq
             (when-not (zero? (.signum ^BigInteger bits))
               (let [value (.getLowestSetBit ^BigInteger bits)]
                 (cons value
                       (step (.clearBit ^BigInteger bits value)))))))]
    (step (:bits bm))))

(defn serialize
  ([bm]
   (serialize bm nil))
  ([bm nbytes]
   (let [bits (:bits bm)
         nbytes (or nbytes
                    (max 1
                         (quot (+ (.bitLength ^BigInteger bits) 7) 8)))]
     (byte-array
      (map
       (fn [i]
         (unchecked-byte
          (bit-and 0xff
                   (-> ^BigInteger bits
                       (.shiftRight (* 8 i))
                       (.intValue)))))
       (range nbytes))))))

(defn deserialize [^bytes data]
  (let [bits
        (reduce
         (fn [acc i]
           (.or ^BigInteger acc
                (.shiftLeft (BigInteger/valueOf (bit-and 0xff (aget data i)))
                            (* 8 i))))
         BigInteger/ZERO
         (range (alength data)))]
    (->BitMap bits)))
