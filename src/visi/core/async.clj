(ns visi.core.async
  (:require [visi.core.runtime :as vr :refer [FIXME func-for visi-realize]]
            [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)))

(defmethod visi-realize ManyToManyChannel
  [x]
  (visi-realize (deref x)))

(def local-transforms
  {:ti-aggregate
   (fn [this zero-value seq-op comb-op]
     (FIXME))

   :ti-flat-map
   (fn [this func]
     (async/pipe this (async/chan 1 (mapcat (func-for this func)))))

   :ti-flat-map-double
   (fn [this func]
     (async/pipe this (async/chan 1 (mapcat (func-for this func))))
     )

   :ti-flat-map-pair
   (fn [this func]
     (async/pipe this (async/chan 1 (mapcat (func-for this func))))
     )

   :ti-fold
   (fn [this zero func]
     (async/reduce (func-for this func) zero this))

   :ti-foreach
   (fn [this func]
     (let [the-func (func-for this func)]
       (async/go-loop []
         (let [v (async/<! this)]
           (when (not (nil? v))
             (the-func v)
             (recur))))))

   :ti-group-by
   (fn [this func]
     (FIXME))

   :ti-map
   (fn [this func]
     (async/pipe this (async/chan 1 (map (func-for this func)))))

   :ti-map-to-double
   (fn [this func]
     (async/pipe this (async/chan 1 (map (func-for this func)))))

   :ti-map-to-pair
   (fn [this func]
     (async/pipe this (async/chan 1 (map (func-for this func)))))

   :ti-reduce
   (fn [this func]
     (reduce (func-for this func) this))

   :ti-do-sink
   (fn [rdd name]
     rdd)

   :ti-drop
   (fn
     [this num]
     (async/pipe this (async/chan 1 (drop num))))

   :ti-aggregate-by-key
   (fn [this zero-value seq-op comb-op]
     (FIXME))

   :ti-combine-by-key
   (fn [this create-combiner merge-value merge-combiners]
     (FIXME))

   :ti-fold-by-key
   (fn [this zero-value func]
     (FIXME))

   :ti-map-values
   (fn [this func]
     (async/pipe this (async/chan 1 (map (func-for this func)))))


   :ti-reduce-by-key
   (fn [this func]
     (FIXME))

   :ti-filter
   (fn [this func]
     (async/pipe this (async/chan 1 (filter (func-for this func)))))

   :ti-sort-by
   (fn [this func asc-or-desc]
     (FIXME))

   :ti-flat-map-values
   (fn [this func]
     (async/pipe this (async/chan 1 (mapcat (func-for this func)))))

   :ti-key-by
   (fn [this func]
     (FIXME))})

(extend ManyToManyChannel
  vr/FunctionFor
  vr/local-functions

  vr/TransformInfo
  local-transforms)