(ns truckerpath.more-monads.error-m-test
  (:require [clojure.test :refer :all]
            [truckerpath.more-monads.error-m :refer [attempt-all ComputationFailed]]))

(defrecord SomeError [error]
  ComputationFailed
  (has-failed? [_] true))

(deftest attempt-all-test
  (testing "no errors"
    (attempt-all [a 1
                  b 2]
      (do
        (is (= a 1))
        (is (= b 2)))
      (throw (Exception. "Should not fall into fail branch"))))

  (testing "nil result"
    (attempt-all [a 1
                  b nil]
      (do
        (is (= a 1))
        (is (= b nil)))
      (throw (Exception. "Should not fall into fail branch"))))

  (testing "exception result"
    (let [exc (Exception. "error")]
      (attempt-all [a 1
                    b exc]
        (throw (Exception. "Should not fall into success branch"))
        (is (= failure exc)))))

  (testing "thrown exception"
    (let [exc (Exception. "error")]
      (try
        (attempt-all [a 1
                      b (throw exc)]
          (throw (Exception. "Should not fall into success branch"))
          (throw (Exception. "Should not fall into fail branch")))

        (catch Exception e
          (is (= e exc))))))

  (testing "computation failure"
    (let [exp-failure (->SomeError "error value")]
      (attempt-all [a 1
                    b exp-failure]
        (throw (Exception. "Should not fall into success branch"))
        (is (= failure exp-failure))))))
