(ns imp.core
  (:require [proteus :as p]))

(defmacro dofor
  "Mimics the usage of for loops in imperative language, with some minor features:
  Given an arg vec of [idx ini pred [&optional [res rinit]]] where:
  idx -> var to bind for the current numeric index
  init -> initial value of idx
  pred -> predicate to determine termination, typically based on idx
  step -> expression to compute next idx, typically (inc idx) etc...
  res -> optional, lexical var to define a collected result, will be
         the return of the loop if supplied
  rinit -> optional, initial value of res
  Binds an implicit return function inside the loop context, where
  (return) stops looping and yields res, and (return x) stops looping
  and yields x."
  [[idx init
    pred step
    & [res rinit]] & body]
  (let [res   (or res (gensym "res"))
        rinit (or rinit nil)
        returned (gensym "returned")]
    `(proteus/let-mutable
      [~res ~rinit
       ~returned false]
      (let [~'return
            ^:local
            (fn ([]   (set! ~returned true))
              ([x#] (set! ~returned true)
               (set! ~res x#)
               x#))]
        (loop [~idx (long ~init)]
          (when (and ~pred (not ~returned))
            (set! ~res (do ~@body))
            (recur ~step)))
        ~res))))

;;expose for convenience so proteus ns isn't required.
(defmacro let-mutable
  "proteus.core/let-mutable
  The mutable variable cannot escape the scope in which it's defined; if the variable is
  closed over, the current value will be captured.  Wherever possible, unboxed numbers are
  used, giving significantly better performance than `clojure.core/with-local-vars`."
  [bindings & body]
  `(proteus.core/let-mutable ~bindings ~@body))

