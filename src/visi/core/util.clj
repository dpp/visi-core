(ns visi.core.util)

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
