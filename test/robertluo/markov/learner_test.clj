(ns robertluo.markov.learner-test
  (:require [clojure.test :refer [deftest is testing]]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.fade :as fade]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.timed :as timed]))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest every-learner-is-a-learner
  ;; learn checks its learner against Learner; a well-formed call is the check passing
  (doseq [l [estimate/counting (fade/fading 0.9) timed/timed]]
    (is (some? (learner/learn l (:empty l) [])))))

(deftest learn-combine-readout
  (let [l estimate/counting
        a (learner/learn l {} [:a :b])
        b (learner/learn l {} [:b :a])]
    (is (= {:a {:b 1} :b {:a 1}} (learner/combine l a b)))
    (is (= {:a {:b 1.0} :b {:a 1.0}} (learner/readout l (learner/combine l a b))))))

(deftest guarded-by-malli
  (testing "something short of a learner is refused"
    (is (refused? learner/learn {:step conj} {} [])))
  (testing "observations that are not a collection are refused"
    (is (refused? learner/learn estimate/counting {} :a))))
