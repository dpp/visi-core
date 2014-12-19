(ns visi.core.runtime
  (:require [org.httpkit.client :as httpc]
            [clojure.data.csv :as csv]
            ;; [gorilla-renderable.core :as render]
            [clojure.string :as cljstr]
            [visi.core.util :as vu]
            [clojure.data.json :as json]
            [clojure.tools.analyzer.jvm :as ca]
            [clojure.tools.analyzer.passes.jvm.emit-form :as e])

  (:import ;; FIXME (clojure.tools.nrepl.transport Transport)
           (clojure.lang IDeref)
           (java.util UUID)))


;; # How to convert an simple piece of Clojure into an agnostic Map, Reduce, etc. command

;; The previous version of runtime was a pure hack that worked for the Visi demo and worked in a local JVM, but didn't work distributed.
;; The previous version also only worked with Spark.
;; This version has a couple of major changes.

;; First, all of the operations (e.g., map, flatMap, etc.) are done as macros. This is so that the function parameter can be captured as an S-expression rather than a compiled Clojure function.
;; It turns out that distributing the bytecode for Clojure functions to the Spark cluster was non-trivial.
;; Yes, Spark does it with Scala, but there's a significant amount of ASM magic where-in the bytecode is re-written.
;; Plus, I can't figure out how Clojure turns a function into bytecode, so I can't capture the bytecode before it gets turned into a JVM class.

;; So, instead of sending serialized Clojure functions, sending the S-expression over the wire makes much more sense.
;; The Java file `SerializableFunctions.java` contains the classes that are both a Spark function and something that can be cleanly serialized.

;; So, we serialize the S-expression and when the function gets called, if the S-expression hasn't been compiled, eval is called on the s-expression and boom... a Clojure `IFn`.

;; There's still some work that needs to be done because we don't necesssarily have all the "requires" context set up for the Clojure runtime on each node.
;; Luckily, we can analyze the S-expression and get that information (not done yet, but there's a simple place to do it in the code).

;; So, the `runtime/v-map` macro returns `(ti-map ~this (quote ~(clean-it-up func)))`.

;; The `clean-it-up` function runs the Clojure analyzer on the S-expression and yields an S-expression with fully materialized namespaces, etc. It's in `clean-it-up` that we can also add the requires analysis.

;; `clean-it-up` relies on https://github.com/clojure/jvm.tools.analyzer which gives us a very nice intermediate format for the S-express that we can walk and discover all sorts of nice stuff about the expression (including anything we have to explicitly require).

;; `ti-map` is a function in the `TransformInfo` protocol.
;; Protocols are like Java interfaces... a Protocol can be implemented for each class where the class is the class of the instance the first paramter to the call. It's grouped polymorphic dispatch based on the first parameter.
;; The nice thing is that you can add an existing Java class to a Protocol without adding the interface to the class. It's Clojure's answer to the Expression Problem http://en.wikipedia.org/wiki/Expression_problem .

;; In order to support `ti-map` for *any* compute backend, we just extend the back end's "thing that represents data" (in the case of Spark, it's an RDD) to support the TransformInfo protocol.
;; So, ti-map for a Spark RDD looks like:

;; ```
;;    :ti-map
;;    (fn [^JavaRDDLike this func]
;;      (.map
;;       this
;;       (func-for this func)))
;; ```

;; `func-for` is a function on the `FunctionFor` protocol.
;; This protocol wraps the S-expression representing the function with a wrapper that the target runtime expects (in this case, it's a http://spark.apache.org/docs/latest/api/java/org/apache/spark/api/java/function/Function.html ).
;; So, `func-for` takes the S-expression and turns it into a Spark Function.
;; This is done by:

;; ```
;; :func-for (fn [this form] (SerializableFunctions/mkFunction form))
;; ```

;; This calls:

;; ```
;;     public static <A, B> Function<A, B> mkFunction(Object p) {
;;         return new VFunction<A, B>(p);
;;     }
;; ```

;; And boom, we have a function that can be serialized and sent across the Spark cluster.
;; This also means that we can implement the `FunctionFor` protocol to support whatever arbitrary system we want (Hadoop MapReduce, Storm, etc.)


(defprotocol FunctionFor
  (void-func-for [this form])
  (double-func-for [this form])
  (bool-func-for [this form])
  (double-iter-func-for [this form])
  (func-for [this form])
  (pair-iter-func-for [this form])
  (pair-func-for [this form])
  (iter-func2-for [this form])
  (iter-func-for [this form])
  (func3-for [this form])
  (func2-for [this form])
)


(defprotocol TransformInfo
  (ti-do-sink [this name])
  (ti-aggregate [this zero-value seq-op comb-op])
  (ti-aggregate-by-key [this zero-value seq-op comb-op])
  (ti-combine-by-key [this create-combiner merge-value merge-combiners])
  (ti-flat-map [this func])
  (ti-flat-map-double [this func])
  (ti-flat-map-pair [this func])
  (ti-flat-map-values [this func])
  (ti-fold-by-key [this zero-value func])
  (ti-fold [this zero func])
  (ti-foreach [this func])
  (ti-group-by [this func])
  (ti-key-by [this func])
  (ti-map-values [this func])
  (ti-map [this func])
  (ti-map-to-double [this func])
  (ti-map-to-pair [this func])
  (ti-reduce [this func])
  (ti-reduce-by-key [this func])
  (ti-filter [this func])
  (ti-sort-by [this func ascending])
  (ti-drop [this count])
  )


(defn clean-it-up
  [form]
  (-> form ca/analyze (e/emit-form {:qualified-symbols true :hygienic true} ) doall))

(defmacro clean-it-up-mac
  [form]
  `(quote ~(-> form ca/analyze (e/emit-form {:qualified-symbols true :hygienic false} ))))

(defmacro v-aggregate [this zero-value seq-op comb-op]
  `(ti-aggregate ~this ~zero-value (quote ~(clean-it-up seq-op))
                 (quote ~(clean-it-up comb-op))))

(defmacro v-aggregate-by-key [this zero-value seq-op comb-op]
  `(ti-aggregate-by-key ~this ~zero-value
                        (quote ~(clean-it-up  seq-op))
                        (quote ~(clean-it-up  comb-op))))
(defmacro v-combine-by-key [this create-combiner merge-value merge-combiners]
  `(ti-combine-by-key ~this (quote ~(clean-it-up  create-combiner))
                      (quote ~(clean-it-up  merge-value))
                      (quote ~(clean-it-up  merge-combiners))))
(defmacro v-flat-map [this func]
  `(ti-flat-map ~this (quote ~(clean-it-up  func))))
(defmacro v-flat-map-double [this func]
  `(ti-flat-map-double ~this (quote ~(clean-it-up func))))
(defmacro v-flat-map-pair [this func]
  `(ti-flat-map-pair ~this (quote ~(clean-it-up func))))
(defmacro v-flat-map-values [this func]
  `(ti-flat-map-values ~this (quote ~(clean-it-up func))))
(defmacro v-fold-by-key [this zero-value func]
  `(ti-fold-by-key ~this ~zero-value (quote ~(clean-it-up func))))
(defmacro v-fold [this zero func]
  `(ti-fold ~this ~zero (quote ~(clean-it-up func))))
(defmacro v-foreach [this func]
  `(ti-foreach ~this (quote ~(clean-it-up func))))
(defmacro v-group-by [this func]
  `(ti-group-by ~this (quote ~(clean-it-up func))))
(defmacro v-key-by [this func]
  `(ti-key-by ~this (quote ~(clean-it-up func))))
(defmacro v-map-values [this func]
  `(ti-map-values ~this (quote ~(clean-it-up func))))
(defmacro v-map [this func]
  `(ti-map ~this (quote ~(clean-it-up func))))
(defmacro v-map-to-double [this func]
  `(ti-map-to-double ~this (quote ~(clean-it-up func))))
(defmacro v-map-to-pair [this func]
  `(ti-map-to-pair ~this (quote ~(clean-it-up func))))
(defmacro v-reduce [this func]
  `(ti-reduce ~this (quote ~(clean-it-up func))))
(defmacro v-reduce-by-key [this func]
  `(ti-reduce-by-key ~this (quote ~(clean-it-up func))))
(defmacro v-filter [this func]
  `(ti-filter ~this (quote ~(clean-it-up func))))
(defmacro v-sort-by [this func ascending]
  `(ti-sort-by ~this (quote ~(clean-it-up func)) ascending))



(def ^:private -app-params (atom {}))

(defn app-params [] @-app-params)

(defn set-app-params! [params]
  (println "Setting app params to " params)
  (reset! -app-params params))

(defmulti to-iterable class)

(defmethod to-iterable Iterable [x] x)

(defmethod to-iterable :default
  [x]
  (try
    (seq x)
    (catch Exception e [x])))

#_(defn to-iterable
  "Converts the thing to something that's Iterable"
  [x]
  (cond

   (instance? JavaRDDLike x)
   (.collect x)

   (and
    (vector? x)
    (every? #(instance? JavaRDDLike %) x))
   (mapcat #(.collect %) x)

   ))

(defn- record-access
  [info]
  (if-let [{:keys [segmentID code] :as msg}
           (or
            ;; FIXME v-mid/current-message
            ;; clojure.tools.nrepl.middleware.interruptible-eval/*msg* ;; FIXME
            )]
    (when segmentID
      (swap! info assoc segmentID (dissoc msg :transport)))))

(def ^:private session-info (ref {}))

(defn add-msg-for-session
  [session resp]
  (dosync
   (alter session-info assoc session (conj (get @session-info session []) resp))))

(defn pull-session-info
  [session]
  (dosync
   (let [resp (get @session-info session [])]
     (alter session-info dissoc session)
     resp)))

(defn- recompute
  [info]
  (let [segments (-> info deref vals vec)
        ;; FIXME runner (inte/interruptible-eval nil)
        ]
    (reset! info {})
    (future
      (dorun
       (map
        (fn [msg]
          #_(let [transport
                (reify Transport
                  (recv [this] )
                  (recv [this timeout] )
                  (send [this resp]
                    (when (not (contains? (:status resp) :done))
                      (add-msg-for-session
                       (:session resp)
                       (assoc resp :segmentID (:segmentID msg)
                              :clear "true")))
                    this))]
            ;; FIXME (runner (assoc msg :transport transport))
            ))

        segments)))))

(defprotocol Settable
  (s-source? [this])
  (s-running? [this])
  (s-add-ref [this])
  (s-remove-ref [this])
  (s-set! [this value])
  (s-swap! [this func]))

(defn build-watching-var
  ([initial-value] (build-watching-var initial-value {}))
  ([initial-value opts]
     (let [value (atom initial-value)
           ref-cnt (atom 0)
           running (atom true)
           dependents (atom {})
           source (:source opts)]
       (reify
         ;; render/Renderable
         ;; (render [this] (render/render @value))

         IDeref
         (deref [this]
           (record-access dependents)
           @value)

         Iterable
         (iterator [this]
           (record-access dependents)
           (.iterator (to-iterable @value)))

         Settable
         (s-add-ref [this]
           (swap! ref-cnt inc))
         (s-remove-ref [this]
           (let [cnt (swap! ref-cnt dec)]
             (recompute dependents)
             (when (<= 0 cnt)
               (reset! dependents {})
               (reset! value nil)
               (reset! running false))
             ))
         (s-running? [this] @running)
         (s-source? [this] source)
         (s-set! [this new-value]
           (let [ret (reset! value new-value)]
             (recompute dependents)
             ret))
         (s-swap! [this func]
           (let [ret (swap! value func)]
             (recompute dependents)
             ret
             ))))))


(defmulti spark-context (fn [& x] 42))

(defn get-data-from-url
  [url]

  (let [response (httpc/get url)
        type (-> @response :headers :content-type)]
    (cond
     (.startsWith type "application/json")
     (let [ret (json/read-str (-> @response :body))]
       ret)
     :else
     (-> @response :body))))

(defmulti build-rdd-from-url (fn [x _] :default))

(defmacro source
  [var-name url]
  `(~'def ~(symbol var-name) (build-rdd-from-url (~'visi.runtime/spark-context) ~url)))

(defmulti scala-prod-to-vector class)

(defmethod scala-prod-to-vector :default [x] x)

(defn ensure-iterable
  [x]
  (if (instance? Iterable x) x [x]))


(defn read_url
  [url]
  (get-data-from-url url)
  )

(defn starts-with [^String s ^String s2] (.startsWith s s2))

;; (defn twitter-config-builder
;;   []
;;   (->  (twitter4j.conf.ConfigurationBuilder.)
;;        (.setDebugEnabled true)
;;        (.setOAuthConsumerKey "Ctv9xKyG815ScJGzMVVuQw")
;;        (.setOAuthConsumerSecret "INIulEJfsyWSLzFf9u9p4OxqJos9SdmTcoFIMeqJqaA")
;;        (.setOAuthAccessToken "3930521-s9UEBKa8pzWiptCCyrneraZVAxW6DqYL3ieftVYhMo")
;;        (.setOAuthAccessTokenSecret "iHfLYzG1awJEpoZ9hZJBc4hngTwRPqYtuJDezqMPcr0")))

;; (defn twitter-factory
;;   []
;;   (twitter4j.TwitterFactory. (.build (twitter-config-builder))))

;; (defn twitter-instance [] (.getInstance (twitter-factory)))

;; (defn twitter-authorization [] (.getAuthorization (twitter-instance)))


;; (defn streaming-context
;;   ([] (streaming-context 120000))
;;   ([duration]
;;      (let [duration
;;            (if (number? duration)
;;              (org.apache.spark.streaming.Duration. duration)
;;              duration)]
;;        (org.apache.spark.streaming.api.java.JavaStreamingContext.
;;         (spark-context)
;;         duration))))


(defmulti create-twitter-stream identity)

(defmulti create-mqtt-stream identity)

(defn stream-into-watching
  "Put the Stream into a watching var"
  ([stream] (stream-into-watching stream {:start true}))
  ([stream props]
     (let [watching-var (build-watching-var [] {:source stream})]
       (.foreachRDD stream (func-for
                            stream
                            (quote
                             (fn [rdd]
                               (when (s-running? watching-var)
                                 (s-swap! watching-var
                                          (fn [data]
                                            (into data (.collect rdd)))))))))
       (when (:start props) (-> stream .context .start))
       watching-var
       )))

(defn mark-watching-var
  [var-name definition value]
  (when (satisfies? Settable value)
    (let [guid (str (UUID/randomUUID))
          the-var (intern *ns* var-name)]
      (add-watch the-var guid
                 (fn [key reference old new]
                   (cond
                    ;; no change, do nothing
                    (identical? new old)
                    nil

                    ;; increase the ref count
                    (identical? value new)
                    (s-add-ref value)

                    ;; setting to a different value
                    ;; cause dependent items to recompute
                    (identical? value old)
                    (do
                      (remove-watch the-var guid)
                      (s-remove-ref value))

                    ;; ignore
                    :else nil)
                   ))))
  value)

(defmulti send-text identity)

(defmacro text-when
  "Send a text to the number when the condition is true"
  [condition number message]
  `(when ~condition (send-text number message)))


(defmulti to-pair identity)

(defn to-iterable-pair
  [x]
  (map to-pair (to-iterable x)))

(defn to-iterable-double
  [x]
  (mapcat #(if (instance? Number %)
             [(.doubleValue %)]
             nil) (to-iterable x)))

(defn do-sink
  [name expression]
  (ti-do-sink expression (str name)))

(defn spark-job?
  "Is the current thing running as a Spark job?"
  []
  (-> (System/getProperties) (get "SPARK_SUBMIT") boolean))

(defmethod vu/running-job :default
  []
  (spark-job?))
