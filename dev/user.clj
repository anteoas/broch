(ns user
  (:require [cider.piggieback]
            [cljs.repl.browser :as browser]))

(defmacro capture-env
  "Capture local bindings.

  Example:

  (defn adder [x y]
    (user/capture-env)
    (+ x y))

  expands to:

  (defn adder [x y]
    (def x x)
    (def y y)
    (+ x y))

  Useful for debugging function bodies in the repl."
  []
  (conj (map (fn [local]
               `(def ~local ~local))
             (keys &env))
        'do))

(defn printr [x] (println x) x)

(comment
  ;; Start cljs repl
  (cider.piggieback/cljs-repl (browser/repl-env))

  )
