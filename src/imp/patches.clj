(ns imp.patches)

;;original source by Zachary Tellman
;;https://github.com/ztellman/proteus/blob/master/src/proteus.clj
;;original proteus license:

;;Copyright © 2013 Zachary Tellman
;;Distributed under the MIT License.

;;patched to actually allow nested let-mutables.
;;https://github.com/ztellman/proteus/issues/7
;;existing PR failed for my tests.

(ns proteus
  (:require
    [riddley.walk :refer (walk-exprs)]
    [riddley.compiler :refer (locals)])
  (:import
    [clojure.lang
     Compiler$LocalBinding]))

;;;

(declare transform-let-mutable-form)

(defn- key* [x]
  (when x (key x)))

(defn- mutable-vars []
  (->> (locals) keys (filter (comp ::write-form meta))))

(defn- read-form [x]
 (if (not (:tag (meta x)))
   (-> (locals) (find x) key* meta ::read-form)
   nil))

(defn- write-form [x]
  (-> (locals) (find x) key* meta ::write-form))

(defn- transform-predicate [x]
  (or (and (symbol? x)
           (not (-> x meta :tag)))
      (and (seq? x)
           (or
            (#{'letfn* 'set!} (first x))
            (and (= 'fn* (first x)) (not (:local (meta x))))
            (= (first x) '.)))))

(def proteans  #{"proteus.Containers$B"
                 "proteus.Containers$L"
                 "proteus.Containers$D"
                 "proteus.Containers$O"})

;;custom handler to allow us to bottom out so we don't
;;expand (. protean-value x) into
;;(. protean-value (. x x))  in nested contexts
;;for the read-form calls.
(defn xform-dot [x]
  (if (-> x second meta :tag proteans)
    x
    `(. ~@(transform-let-mutable-form (rest x)))))

(defn- transform-handler [x]
  (if (symbol? x)
    (or (read-form x) x)
    (condp = (first x)
      '. ;;handle dot forms explicitly now to prevent implicit walking.
      (xform-dot x)
      'set!
      (let [[_ k v] x]
        (if-let [f (write-form k)]
          (f (transform-let-mutable-form v))
          x))
      'fn*
      (let [vs (seq (mutable-vars))]
        `(let [~@(interleave vs (map read-form vs))]
           ~x))
      'letfn*
      (let [[_ bindings & body] x
            vs (seq (mutable-vars))
            vs' (map #(gensym (name %)) vs)]
        `(let [~@(interleave vs' vs)
               ~@(interleave vs (map read-form vs))]
           (~'letfn* ~bindings
            (let [~@(interleave vs vs')]
              ~(transform-let-mutable-form `(do ~@body)))))
        x))))

(defn- transform-let-mutable-form [x]
  (walk-exprs transform-predicate transform-handler x))

;;;

(defn- typeof [x env]
  (if-let [^Compiler$LocalBinding binding (get env x)]
    (when (.hasJavaClass binding)
      (.getJavaClass binding))
    (cond
      (instance? Boolean x) Boolean/TYPE
      (instance? Long x) Long/TYPE
      (instance? Double x) Double/TYPE)))

(defmacro let-mutable
  "Acts as a let-binding for variables that can be modified with `set!`.
   (let-mutable [x 0]
     (dotimes [_ 100]
       (set! x (inc x)))
     x)
   The mutable variable cannot escape the scope in which it's defined; if the variable is
   closed over, the current value will be captured.  Wherever possible, unboxed numbers are
   used, giving significantly better performance than `clojure.core/with-local-vars`."
  [bindings & body]
  (let [ks (->> bindings
             (partition 2)
             (map first))
        vs (->> bindings
             (partition 2)
             (map second))
        types (map
                #(condp = (typeof % &env)
                   Boolean/TYPE "proteus.Containers$B"
                   Long/TYPE "proteus.Containers$L"
                   Double/TYPE "proteus.Containers$D"
                   "proteus.Containers$O")
                vs)
        ks (map
             (fn [k type]
               (with-meta k {:tag type}))
             ks
             types)]
    (transform-let-mutable-form
      `(let [~@(interleave
                 (map
                   (fn [k]
                     (vary-meta k merge
                       {::read-form `(.x ~k)
                        ::write-form (fn [x] `(do (.set ~k ~x) nil))}))
                   ks)
                 (map (fn [v type] `(new ~(symbol type) ~v)) vs types))]
         ~@body))))

(in-ns 'imp.patches)

