(ns robertluo.markov.fade-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.fade :as fade]
            [robertluo.markov.gen :as mgen]
            [robertluo.markov.laws :as laws]
            [robertluo.markov.learner :as learner]))

(def ^:private fading (fade/fading 0.9))

(defn- gen-known [learner]
  (gen/let [p mgen/prior
            walks (gen/vector mgen/walk 0 3)]
    (reduce #(learner/learn learner %1 %2) (fade/faded (estimate/prior p)) walks)))

(defspec faded-combine-with-nothing-as-identity 100
  (laws/identity-law fading (gen-known fading)))

(defspec faded-combine-associatively 100
  (laws/associativity-law fading (gen-known fading)))

(defspec fading-in-parts-knows-the-same-as-fading-at-once 200
  (laws/parts-law fading (gen-known fading) mgen/walk-split))

(defspec without-fading-it-is-counting 200
  (prop/for-all [p mgen/prior
                 walk mgen/walk]
                (let [known (estimate/prior p)]
                  (= (learner/learn estimate/counting known walk)
                     (:weights (learner/learn (fade/fading 1) (fade/faded known) walk))))))

(defspec a-step-per-transition 200
  (prop/for-all [walk mgen/walk]
                (= (max 0 (dec (count walk)))
                   (:elapsed (learner/learn fading (fade/faded {}) walk)))))

(defspec memory-is-bounded-by-one-over-one-minus-lambda 200
  ;; n transitions weigh 1 + λ + … + λⁿ⁻¹ in all, below 1 / (1 − λ) however many
  (prop/for-all [lambda (gen/elements [0.5 0.9 0.99])
                 walk mgen/walk]
                (<= (transduce (mapcat vals) + 0
                               (vals (:weights (learner/learn (fade/fading lambda) (fade/faded {}) walk))))
                    (+ (/ 1 (- 1 lambda)) 1e-9))))

(deftest order-matters
  (let [a (learner/learn fading (fade/faded {}) [:a :b])
        b (learner/learn fading (fade/faded {}) [:b :a])]
    (is (not (laws/approx= (learner/combine fading a b) (learner/combine fading b a))))
    (testing "what came later weighs more"
      (let [w (:weights (learner/combine fading a b))]
        (is (< (get-in w [:a :b]) (get-in w [:b :a])))))))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest guarded-by-malli
  (testing "a fading factor outside (0, 1] is refused"
    (is (refused? fade/fading 0))
    (is (refused? fade/fading 1.5)))
  (testing "a walk of something other than states is refused by the step"
    (is (refused? learner/learn fading (fade/faded {}) ["a" "b"]))))
