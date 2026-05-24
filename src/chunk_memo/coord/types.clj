(ns chunk-memo.coord.types
  (:require [chunk-memo.coord.axis :as axis]))

;; -----------------------------------------------------------------------------
;; Empty
;; -----------------------------------------------------------------------------

(defrecord CoordEmpty [])

(def empty-selection
  (->CoordEmpty))

;; -----------------------------------------------------------------------------
;; Product
;; -----------------------------------------------------------------------------

(defrecord CoordProduct [axes])

(defn coord-product [axes]
  (->CoordProduct
   (mapv axis/normalize-axis axes)))

; (defn product-size [^CoordProduct p]
;   (reduce * 1 (map axis/size (:axes p))))

;; -----------------------------------------------------------------------------
;; Union
;; -----------------------------------------------------------------------------

(defrecord CoordUnion [parts])

(defn coord-union [& parts]
  (->CoordUnion (vec parts)))

;; -----------------------------------------------------------------------------
;; Intersection
;; -----------------------------------------------------------------------------

(defrecord CoordIntersection [parts])

(defn coord-intersection [& parts]
  (when (empty? parts)
    (throw (ex-info "intersection requires at least one part" {})))

  (->CoordIntersection (vec parts)))

;; -----------------------------------------------------------------------------
;; Difference
;; -----------------------------------------------------------------------------

(defrecord CoordDifference [base remove])

(defn coord-difference [base remove]
  (->CoordDifference base remove))

(defn coord-product? [x]
  (instance? CoordProduct x))

(defn coord-union? [x]
  (instance? CoordUnion x))

(defn coord-intersection? [x]
  (instance? CoordIntersection x))

(defn coord-difference? [x]
  (instance? CoordDifference x))

(defn coord-empty? [x]
  (instance? CoordEmpty x))

(defn axes [selection]
  (:axes selection))

(defn parts [selection]
  (:parts selection))

(defn difference-base [selection]
  (:base selection))

(defn difference-remove [selection]
  (:remove selection))
