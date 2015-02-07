;; (c) Copyright 2014-2015 David Pollak (@dpp, feeder.of.the.bears at gmail)
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns visi.core.util
  (:require [clojure.data.codec.base64 :as b64]))

(defn has-class
  "Is the class loadable?"
  [clz]
  (try
    (let [tfn (fn [x] x)
          tfn-clz (.getClass tfn)
          found-clz (-> tfn-clz .getClassLoader (.loadClass clz))]
      (boolean found-clz))
    (catch Exception e false)))

(defn has-resource
  "Is the resource available"
  [clz]
  (try
    (let [tfn (fn [x] x)
          tfn-clz (.getClass tfn)
          found-clz (-> tfn-clz .getClassLoader (.getResource clz))]
      (boolean found-clz))
    (catch Exception e false)))


(defmacro when-has-class
  "Only load the functions/compile the code
  if the named class is present"
  [clz & body]
  (if (has-class clz)
    (do
      ;; cause all the import and require blocks to
      ;; be evaled in the context of the namespace
      (dorun
       (map
        (fn [x]
          (when (and
                 (seq? x)
                 (or (= 'import (first x))
                     (= 'require (first x))))
            (eval x)))
        body))
      `(do ~@body))
    nil))

(defmacro when-has-resource
  "Only load the functions/compile the code
  if the resource is present"
  [clz & body]
  (if (has-resource clz)
    (do
      ;; cause all the import and require blocks to
      ;; be evaled in the context of the namespace
      (dorun
       (map
        (fn [x]
          (when (and
                 (seq? x)
                 (or (= 'import (first x))
                     (= 'require (first x))))
            (eval x)))
        body))
      `(do ~@body))
    nil))

(defmulti running-job (fn [& _] :default))

(defmethod running-job :default
  []
  false)

(defn fix-namespace
  "Make the current namespace Visi-friendly. Does 'use runtime, clojure.string, clojure.data, then does aliases for all the functions with - in them"
  []
  (when (not (find-var (symbol (str (-> *ns* .name name) "/$$fixed$$"))))
    (eval '(def $$fixed$$ true))
    (eval '(use 'visi.core.runtime))
    (eval '(use 'clojure.data))


    (doseq [[key value] (.getMappings *ns*)]
      (let [kn (str key)
            repl (.replace kn "-" "_")
            matches (re-matches #"[a-zA-Z_$][a-zA-Z\d_$]+" repl)
            symd (symbol repl)
            found (get (.getMappings *ns*) symd)]
        (when (and
               (not= kn repl)
               matches
               (not found))
          (.refer *ns* symd value))))))

(defn bytes? [x]
  (= (Class/forName "[B")
     (.getClass x)))

(defn decode-base64
  [value]
  (cond
    (string? value) (decode-base64 (.getBytes value "UTF-8"))
    (bytes? value) (String. (b64/decode value) "UTF-8")
    :else (decode-base64 (pr-str value)))
  )
