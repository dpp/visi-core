(ns visi.core.grapher
  (:require [visi.core.runtime :as vr]))

;; Builds the metadata for a DAG based on the input

(defrecord DAGBuilder [parents operation]
  vr/FunctionFor

  (void-func-for [this form] form)
  (double-func-for [this form] form)
  (bool-func-for [this form] form)
  (double-iter-func-for [this form] form)
  (func-for [this form] form)
  (pair-iter-func-for [this form] form)
  (pair-func-for [this form] form)
  (iter-func2-for [this form] form)
  (iter-func-for [this form] form)
  (func3-for [this form] form)
  (func2-for [this form] form)

  vr/TransformInfo
  (ti-do-source [this name] (->DAGBuilder this {:ti-do-source name}))
  (ti-do-sink [this name] (->DAGBuilder this {:ti-do-sink name}))
  (ti-aggregate [this zero-value seq-op comb-op] (->DAGBuilder this {:ti-aggregate [zero-value seq-op comb-op]}))
  (ti-aggregate-by-key [this zero-value seq-op comb-op] (->DAGBuilder this {:ti-aggregate-by-key [zero-value seq-op comb-op]}))
  (ti-combine-by-key [this create-combiner merge-value merge-combiners] (->DAGBuilder this {:ti-combine-by-key [create-combiner merge-value merge-combiners]}))
  (ti-flat-map [this func] (->DAGBuilder this {:ti-flat-map func}))
  (ti-flat-map-double [this func] (->DAGBuilder this {:ti-flat-map-double func}))
  (ti-flat-map-pair [this func] (->DAGBuilder this {:ti-flat-map-pair func}))
  (ti-flat-map-values [this func] (->DAGBuilder this {:ti-flat-map-values func}))
  (ti-fold-by-key [this zero-value func] (->DAGBuilder this {:ti-fold-by-key [zero-value func]}))
  (ti-fold [this zero func] (->DAGBuilder this {:ti-fold [zero func]}))
  (ti-foreach [this func] (->DAGBuilder this {:ti-foreach func}))
  (ti-group-by [this func] (->DAGBuilder this {:ti-group-by func}))
  (ti-key-by [this func] (->DAGBuilder this {:ti-key-by func}))
  (ti-map-values [this func] (->DAGBuilder this {:ti-map-values func}))
  (ti-map [this func] (->DAGBuilder this {:ti-map func}))
  (ti-map-to-double [this func] (->DAGBuilder this {:ti-map-to-double func}))
  (ti-map-to-pair [this func] (->DAGBuilder this {:ti-map-to-pair func}))
  (ti-reduce [this func] (->DAGBuilder this {:ti-reduce func}))
  (ti-reduce-by-key [this func] (->DAGBuilder this {:ti-reduce-by-key func}))
  (ti-filter [this func] (->DAGBuilder this {:ti-filter func}))
  (ti-sort-by [this func ascending] (->DAGBuilder this {:ti-sort-by [func ascending]}))
  (ti-drop [this cnt] (->DAGBuilder this {:ti-drop cnt}))
  )


(defn source-node
  "Build a source node... a named root node for the DAG"
  [name]
  (->DAGBuilder nil {:ti-do-source name}))