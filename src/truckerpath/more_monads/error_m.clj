(ns truckerpath.more-monads.error-m
  (:require [clojure.algo.monads :refer [defmonad domonad]]))

(defprotocol ComputationFailed
  "A protocol that determines if a computation has resulted in a failure.
   This allows the definition of what constitutes a failure to be extended
   to new types by the consumer."
  (has-failed? [self]))

(extend-protocol ComputationFailed
  Object
  (has-failed? [self] false)

  nil
  (has-failed? [self] false) ; assume that `nil` result is not a failure

  Exception
  (has-failed? [self] true))

(defmonad error-m
  "Maybe-like monad extended to fail not on `nil` values but on any types
   extending `ComputationFailed` protocol (based on `has-failed?` function).
   Ripped off from https://brehaut.net/blog/2011/error_monads"
  [m-result identity
   m-bind   (fn [m f] (if (has-failed? m)
                        m
                        (f m)))])

(defmacro attempt-all
  "Syntactic sugar for `error-m` monad to make it look like `let` form
   with additional `else` option which may handle failure.

   If no `else` form provided, `attempt-all` will just return failure,
   otherwise `else` form may handle it somehow. The failure result will
   be injected into `else` form scope in the `failure` symbol.

   Examples:
   (attempt-all [a 1
                 b (inc a)]
      (+ a b))              ; return 3

   (attempt-all [a (some-failure 'failure')
                 b (inc a)] ; wont be executed
      (+ a b))              ; wont be executed, return failure

   (attempt-all [a (some-failure 'failure')
                 b (inc a)]
      (+ a b)
      (do
         (log/error 'ERROR!' failure) ;`failure` injected by attempt-all
         failure))"
  ([bindings body] `(domonad error-m ~bindings ~body))
  ([bindings body else]
   `(let [result# (attempt-all ~bindings ~body)]
      (if (has-failed? result#)
        (let [~'failure result#]
          ~else)
        result#))))
