# chunk-memo-clj

Filesystem-backed memoization for functions evaluated over a finite parameter grid.

`chunk-memo-clj` is useful when you have work keyed by named integer parameters, want cached results on disk, and also want compact metadata for which parameter points have already completed.

The main user-facing namespace is `chunk-memo.memo`.

## Installation

This project currently uses `deps.edn` directly. From this repository, run the tests with:

```sh
clojure -M:test
```

To use it from another local project, add this repository as a local/root dependency in that project's `deps.edn`.

## Quick Start

```clojure
(ns example
  (:refer-clojure :exclude [memoize])
  (:require [chunk-memo.memo :as memo]))

(def cache
  (memo/chunk-memo
   "cache"
   (array-map :x (range 0 10)
              :y (range 20 25))))

(def cached-work
  (memo/memoize
   cache
   (fn [{:keys [x y]}]
     {:sum (+ x y)})))

(cached-work {:x 1 :y 20})
;; => {:sum 21}

(cached-work {:x 1 :y 20})
;; Reads the cached result instead of calling the function again.
```

The wrapped function must accept one argument map. Axis names from the memo's parameter space are read from that map in axis order.

## Cache Parameters

Axis values identify a point in the parameter grid. Non-axis parameters can select independent caches through the `:params` key.

```clojure
(cached-work {:x 1 :y 20 :params {:model "small"}})
(cached-work {:x 1 :y 20 :params {:model "large"}})
```

Those two calls use the same coordinate point, but different cache directories because their cache params differ.

You can set base cache params when creating the memo:

```clojure
(def cache
  (memo/chunk-memo
   "cache"
   (array-map :x (range 0 10)
              :y (range 20 25))
   {:params {:dataset "train"}
    :chunk-size 200}))
```

Per-call params are merged with the base params before deriving the cache id.

## Finding Missing Work

Use `memo/missing` to filter argument maps that are not complete in the relevant cache.

```clojure
(memo/missing
 cache
 [{:x 1 :y 20 :params {:model "small"}}
  {:x 2 :y 20 :params {:model "small"}}])
```

This returns only the argument maps whose axis point has not been marked complete for that params payload.

## Serialization

By default, results are serialized as EDN with suffix `.edn`.

Override serialization with `:serializer!`, `:deserializer`, and `:suffix`:

```clojure
(def cached-work
  (memo/memoize
   cache
   expensive-function
   {:suffix ".edn"
    :serializer! (fn [value file]
                   (spit file (pr-str value)))
    :deserializer (fn [file]
                    (clojure.edn/read-string (slurp file)))}))
```

The serializer receives the computed value and a target file. The deserializer receives the payload file.

## Axis Specs

The simplest axis spec is an ordered map from axis name to contiguous integer values:

```clojure
(array-map :x (range 0 10)
           :y [20 21 22])
```

Axis order matters because it defines the parameter-space rank and row-major layout. Use `array-map` or another ordered map when rank order matters.

Axis values must currently be contiguous. For lower-level construction, you can pass a `RunParameterSpace` or a sequence of `ParamAxis` records from `chunk-memo.params`.

## Lower-Level Selection API

Most users can stay in `chunk-memo.memo`. The lower-level namespaces are available when you need to build or inspect selections directly.

`chunk-memo.params` works in semantic parameter values:

```clojure
(require '[chunk-memo.params :as params])

(def space
  (params/run-parameter-space
   [(params/param-axis :x 10 13)
    (params/param-axis :y 20 22)]))

(def selection
  (params/axis-range space :x 10 13 {:y 21}))
```

`chunk-memo.coord` works in zero-based coordinate positions:

```clojure
(require '[chunk-memo.coord :as coord])

(def selection
  (coord/coord-product [(coord/range-axis 0 3)
                        [1]]))

(coord/contains-coord? selection [2 1])
;; => true
```

## How It Is Organized

The project has a few explicit layers:

`chunk-memo.memo` is the main user API. It creates memo objects, wraps functions, chooses cache directories from params, serializes payloads, and reports missing argument maps.

`chunk-memo.params` represents named semantic parameter spaces and translates semantic values into zero-based coordinate selections.

`chunk-memo.coord` represents symbolic N-dimensional coordinate selections such as products, unions, intersections, and differences.

`chunk-memo.layout` compiles coordinate selections into flat row-major index selections.

`chunk-memo.index.selection` represents flat index sets as intervals, strided sets, unions, intersections, and differences.

`chunk-memo.chunks` maps flat indices into chunk-local offsets.

`chunk-memo.bitmap` stores chunk completion metadata compactly.

`chunk-memo.cache` persists chunk bitmaps and computes deterministic payload paths.

## On-Disk Layout

A cache root contains one directory per stable cache id. Each cache directory contains:

```text
meta.json
index/
payloads/
```

`index/` stores chunk-local completion bitmaps. `payloads/` stores user result files. The cache layer tracks completion metadata; payload contents are controlled by the serializer and deserializer used by `memo/memoize`.

## Development

Run the test suite with:

```sh
clojure -M:test
```
