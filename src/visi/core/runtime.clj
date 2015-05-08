;; (c) Copyright 2014-2015 David Pollak (@dpp, feeder.of.the.bears at gmail)
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns visi.core.runtime
  (:require [org.httpkit.client :as httpc]
            [clojure.data.csv :as csv]
            [clojure.string :as cljstr]
            [visi.core.util :as vu]
            [clojure.data.json :as json]
            [clojure.tools.analyzer.jvm :as ca]
            [clojure.tools.analyzer.passes.jvm.emit-form :as e])

  (:import
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

(def ascending
  "A constant for ascending sorts"
  ::ascending)

(def descending
  "A constant for descending sorts"
  ::descending)


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
  (func2-for [this form]))



(defprotocol TransformInfo
  (ti-do-source [this name])
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
  (ti-drop [this count]))

(defn FIXME
  "Not implemented -- throw an exception"
  []
  (throw (Exception. "Not Implemented")))

(def local-functions
  {:void-func-for (fn [this form] (eval form))
   :double-func-for (fn [this form] (eval form))
   :double-iter-func-for (fn [this form] (eval form))
   :bool-func-for (fn [this form] (eval form))
   :func-for (fn [this form] (eval form))
   :pair-iter-func-for (fn [this form] (eval form))
   :pair-func-for (fn [this form] (eval form))
   :iter-func2-for (fn [this form] (eval form))
   :iter-func-for (fn [this form] (eval form))
   :func3-for (fn [this form] (eval form))
   :func2-for (fn [this form] (eval form))})

(def local-transforms
{:ti-aggregate
   (fn [this zero-value seq-op comb-op]
     (FIXME))

   :ti-flat-map
   (fn [this func]
     (mapcat (func-for this func) this))

   :ti-flat-map-double
   (fn [this func]
     (mapcat (func-for this func) this))

   :ti-flat-map-pair
   (fn [this func]
     (mapcat (func-for this func) this))

   :ti-fold
   (fn [this zero func]
     (reduce (func-for this func) zero this))

   :ti-foreach
   (fn [this func]
     (dorun (map (func-for this func) this)))

   :ti-group-by
   (fn [this func]
     (FIXME))

   :ti-map
   (fn [this func]
     (map (func-for this func) this))

   :ti-map-to-double
   (fn [this func]
     (map (func-for this func) this))

   :ti-map-to-pair
   (fn [this func]
     (map (func-for this func) this))

   :ti-reduce
   (fn [this func]
     (reduce (func-for this func) this))

   :ti-do-sink
   (fn [rdd name]
     (FIXME))

   :ti-drop
   (fn
     [rdd num]
     (drop num rdd))

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
     (map (func-for this func) this))


   :ti-reduce-by-key
   (fn [this func]
     (FIXME))

   :ti-filter
   (fn [this func]
     (filter (func-for this func) this))

   :ti-sort-by
   (fn [this func asc-or-desc]
     (let [ret
           (sort-by (func-for this func) this)]
       (if (= asc-or-desc descending)
         (reverse ret)
         ret)
       ))

   :ti-flat-map-values
   (fn [this func]
     (FIXME))

   :ti-key-by
   (fn [this func]
     (FIXME))})

(extend java.util.List
  FunctionFor
  local-functions

  TransformInfo
  local-transforms)

(extend java.util.Map
  FunctionFor
  local-functions

  TransformInfo
  local-transforms)

(defn- make-array-thing
  [it]
  (if (map? it) [it] it))

(defn- find-op
  "finds all the var references in the analyzed code"
  [info op]
  (if (= op (:op info))
    [info]
    (mapcat (fn [item] (mapcat #(find-op % op) (make-array-thing (item info)))) (:children info))))

(defn- find-vars
  "finds all the var references in the analyzed code"
  [info]
  (find-op info :local))

(defn analyze-it
  [form the-env]
  (->
   form
   (ca/analyze (assoc
                (ca/empty-env)

                :locals
                (->> the-env keys
                     (map (fn [x] [x {:op :binding
                                      :name x
                                      :form x
                                      :local :let}]))
                     (into {}))))))

(defn- find-bound
  "Returns the vars that are bound outside the function"
  [the-vars]
  (filter (fn [v]
            (let [var-name (:name v)
                  info (-> v :env :locals (get var-name))]
              (= :binding (:op info)))) the-vars))

(defn wrap-o-matic
  "Wraps the form in a let statement. Used by the clean-it-up macro"
  [lets form]
  `(let [~@(mapcat
            (fn [[name value]]
              [(symbol name) value])
            lets)] ~form))

(defn clean-it-up
  [form the-env]
  (let [analyzed
        (ca/analyze
         form
         (assoc
          (ca/empty-env)

          :locals
          (->> the-env keys
               (map (fn [x] [x {:op :binding
                                :name x
                                :form x
                                :local :let}]))
               (into {}))))
        emitted (e/emit-form analyzed {:qualified-symbols true :hygienic true})
        bound (-> analyzed find-vars find-bound)
        base `(quote (~'eval ~emitted))]
    (if (empty? bound)
      base
      `(visi.core.runtime/wrap-o-matic
        [~@(map (fn [x] (let [s (:name x)] [(str s) s])) bound)]
        ~base))))

(defmacro clean-it-up-mac
  [form]
  (clean-it-up form &env))

(defmacro v-aggregate [this zero-value seq-op comb-op]
  `(ti-aggregate ~this ~zero-value ~(clean-it-up seq-op &env)
                 ~(clean-it-up comb-op &env)))

(defmacro v-aggregate-by-key [this zero-value seq-op comb-op]
  `(ti-aggregate-by-key ~this ~zero-value
                        ~(clean-it-up seq-op &env)
                        ~(clean-it-up comb-op &env)))
(defmacro v-combine-by-key [this create-combiner merge-value merge-combiners]
  `(ti-combine-by-key ~this ~(clean-it-up create-combiner &env)
                      ~(clean-it-up merge-value &env)
                      ~(clean-it-up merge-combiners &env)))
(defmacro v-flat-map [this func]
  `(ti-flat-map ~this ~(clean-it-up func &env)))
(defmacro v-flat-map-double [this func]
  `(ti-flat-map-double ~this ~(clean-it-up func &env)))
(defmacro v-flat-map-pair [this func]
  `(ti-flat-map-pair ~this ~(clean-it-up func &env)))
(defmacro v-flat-map-values [this func]
  `(ti-flat-map-values ~this ~(clean-it-up func &env)))
(defmacro v-fold-by-key [this zero-value func]
  `(ti-fold-by-key ~this ~zero-value ~(clean-it-up func &env)))
(defmacro v-fold [this zero func]
  `(ti-fold ~this ~zero ~(clean-it-up func &env)))
(defmacro v-foreach [this func]
  `(ti-foreach ~this ~(clean-it-up func &env)))
(defmacro v-group-by [this func]
  `(ti-group-by ~this ~(clean-it-up func &env)))
(defmacro v-key-by [this func]
  `(ti-key-by ~this ~(clean-it-up func &env)))
(defmacro v-map-values [this func]
  `(ti-map-values ~this ~(clean-it-up func &env)))
(defmacro v-map [this func]
  `(ti-map ~this ~(clean-it-up func &env)))
(defmacro v-map-to-double [this func]
  `(ti-map-to-double ~this ~(clean-it-up func &env)))
(defmacro v-map-to-pair [this func]
  `(ti-map-to-pair ~this ~(clean-it-up func &env)))
(defmacro v-reduce [this func]
  `(ti-reduce ~this ~(clean-it-up func &env)))
(defmacro v-reduce-by-key [this func]
  `(ti-reduce-by-key ~this ~(clean-it-up func &env)))
(defmacro v-filter [this func]
  `(ti-filter ~this ~(clean-it-up func &env)))
(defmacro v-sort-by [this func asc-or-dec]
  `(ti-sort-by ~this ~(clean-it-up func &env) ~asc-or-dec))



(def ^:private -app-params (atom {}))

(defn app-params [] @-app-params)

(defn set-app-params! [params]
  (reset! -app-params params))

(defmulti to-iterable class)

(defmethod to-iterable Iterable [x] x)

(defmethod to-iterable :default
  [x]
  (try
    (seq x)
    (catch Exception e [x])))

(defn- record-access
  [info]
  (if-let [{:keys [segmentID code] :as msg}
           (or;; FIXME v-mid/current-message
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
  (let [segments (-> info deref vals vec);; FIXME runner (inte/interruptible-eval nil)
]
    (reset! info {})
    (future
      (dorun
       (map
        (fn [msg]
          #_(let [transport
                  (reify Transport
                    (recv [this])
                    (recv [this timeout])
                    (send [this resp]
                      (when (not (contains? (:status resp) :done))
                        (add-msg-for-session
                         (:session resp)
                         (assoc resp :segmentID (:segmentID msg)
                                :clear "true")))
                      this))];; FIXME (runner (assoc msg :transport transport))
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
             (reset! running false))))
       (s-running? [this] @running)
       (s-source? [this] source)
       (s-set! [this new-value]
         (let [ret (reset! value new-value)]
           (recompute dependents)
           ret))
       (s-swap! [this func]
         (let [ret (swap! value func)]
           (recompute dependents)
           ret))))))


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

(defmacro visi-source
  [var-name url]
  (if (string? url)
    `(~'def ~(symbol var-name)
       (build-rdd-from-url (~'visi.core.runtime/spark-context) ~url))
    `(~'def ~(symbol var-name)
       ~url)
    ))

(defmulti scala-prod-to-vector class)

(defmethod scala-prod-to-vector :default [x] x)

(defn ensure-iterable
  [x]
  (if (instance? Iterable x) x [x]))


(defn read_url
  [url]
  (get-data-from-url url))

(defn starts-with [^String s ^String s2] (.startsWith s s2))


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
     watching-var)))

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
                     :else nil)))))
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
  ;;(ti-do-sink expression (str name))
  nil ;; FIXME deal with sinks
  )

(defn spark-job?
  "Is the current thing running as a Spark job?"
  []
  (-> (System/getProperties) (get "SPARK_SUBMIT") boolean))

(defmethod vu/running-job :default
  []
  (spark-job?))

(defn incr
  "Increment a number or return 1 if it's not a Number"
  [x]
  (if (instance? Number x) (inc x) 1))

(defn decr
  "Decrement a number or return -1 if it's not a Number"
  [x]
  (if (instance? Number x) (dec x) -1))

(defn re-replace
  "a proxy to clojure.string/replace"
  [string regex replacement]
  (clojure.string/replace string regex replacement))


(defn pre-partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  ([f] f)
  ([f arg1]
   (fn [& args] (apply f (concat args (list arg1)))))
  ([f arg1 arg2]
   (fn [& args] (apply f (concat args (list arg1 arg2)))))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f (concat args (list arg1 arg2 arg3)))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f  (concat args (list arg1 arg2 arg3) more)))))

;; function aliases
(def s_replace clojure.string/replace)
(def s_reverse clojure.string/reverse)
(def blank? clojure.string/blank?)
(def capitalize clojure.string/capitalize)
(def escape clojure.string/escape)
(def join clojure.string/join)
(def lower_case clojure.string/lower-case)
(def re_quote_replacement clojure.string/re-quote-replacement)
(def replace_first clojure.string/replace-first)
(def split clojure.string/split)
(def split_lines clojure.string/split-lines)
(def trim clojure.string/trim)
(def trim_newline clojure.string/trim-newline)
(def triml clojure.string/triml)
(def trimr clojure.string/trimr)
(def upper_case clojure.string/upper-case)
(def xform clojure.core/map)
(def xform_flat clojure.core/mapcat)

(defmulti as-string class)

(defmethod as-string Object
  [x]
  (pr-str x))

(defmethod as-string String
  [x]
  (pr-str x))

(defmethod as-string clojure.lang.Keyword
  [x]
  (str (name x) ":"))

(defmethod as-string java.util.List
  [x]
  (str "[" (join ", " (map as-string x)) "]"))

(defmethod as-string java.util.Map
  [x]
  (str "{" (join ", " (map (fn [[k v]] (str (as-string k) " "
                                            (as-string v))) x)) "}"))

(defmethod as-string java.util.Map
  [x]
  (str "{" (join ", " (map (fn [[k v]] (str (as-string k) " "
                                            (as-string v))) x)) "}"))
(defmethod as-string java.util.Set
  [x]
  (str "#{" (join ", " (map as-string x)) "}"))

(defmethod as-string (Class/forName "[Ljava.lang.Object;")
  [x]
  (as-string (seq x)))

(defmethod as-string nil
  [x]
  "nil")

(defn count_for
  "Create a map with :count 1 and the value of the key"
  ([key] (fn [z] (count_for key z)))
  ([key item] {:count 1 key (get item key)}))

(defn merge_sum
  "Merge-with +"
  [x y]
  (merge-with + x y))

(defmulti visi-realize class)

(defmethod visi-realize java.util.List
  [x]
  (take 10 x))

(defmethod visi-realize java.util.Map
  [x]
  (take 10 (seq  x)))

(defmethod visi-realize clojure.lang.IDeref
  [x]
  (visi-realize (deref x)))

(defmethod visi-realize nil
  [x]
  nil)

(defmethod visi-realize java.lang.Object
  [x]
  x)
