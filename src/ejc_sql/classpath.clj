;; Version of `add-classpath` imlementation from Kaocha project.
;; https://github.com/lambdaisland/kaocha/blob/master/src/kaocha/classpath.clj

(ns ejc-sql.classpath
  "This is the add-classpath function from Pomegranate 1.0.0, extracted so we
  don't need to pull in Aether."
  (:refer-clojure :exclude [add-classpath])
  (:require [dynapath.util :as dp]
            [clojure.java.io :as io]))

(def new-jdk
  (>= (BigDecimal. (System/getProperty "java.specification.version")) 1.9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pomegranate

(defn ensure-compiler-loader
  "Ensures the clojure.lang.Compiler/LOADER var is bound to a DynamicClassLoader,
  so that we can add to Clojure's classpath dynamically."
  []
  (when-not (bound? Compiler/LOADER)
    (.bindRoot Compiler/LOADER (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader)))))

(defn- classloader-hierarchy
  "Returns a seq of classloaders, with the tip of the hierarchy first.
   Uses the current thread context ClassLoader as the tip ClassLoader
   if one is not provided."
  ([]
   (if new-jdk
     (ensure-compiler-loader)
     (classloader-hierarchy (.. Thread currentThread getContextClassLoader)))
   (classloader-hierarchy (deref clojure.lang.Compiler/LOADER)))
  ([tip]
   (->> tip
        (iterate #(.getParent %))
        (take-while boolean))))

(defn- modifiable-classloader?
  "Returns true iff the given ClassLoader is of a type that satisfies
   the dynapath.dynamic-classpath/DynamicClasspath protocol, and it can
   be modified."
  [cl]
  (dp/addable-classpath? cl))

(defn add-classpath
  "A corollary to the (deprecated) `add-classpath` in clojure.core. This implementation
   requires a java.io.File or String path to a jar file or directory, and will attempt
   to add that path to the right classloader (with the search rooted at the current
   thread's context classloader)."
  ([jar-or-dir classloader]
   (when-not (dp/add-classpath-url classloader (.toURL (.toURI (io/file jar-or-dir))))
     (throw (IllegalStateException. (str classloader " is not a modifiable classloader")))))
  ([jar-or-dir]
   (let [classloaders (classloader-hierarchy)]
     (if-let [cl (if new-jdk
                   (filter modifiable-classloader? classloaders)
                   (last (filter modifiable-classloader? classloaders)))]
       ;; Add to all classloaders that allow it. Brute force but doesn't hurt.
       (if new-jdk
         (run! #(add-classpath jar-or-dir %) cl)
         (add-classpath jar-or-dir cl))
       (throw (IllegalStateException. (str "Could not find a suitable classloader to modify from "
                                           classloaders)))))))

;; /Pomegranate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
