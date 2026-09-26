(ns robertluo.markov.estimate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.gen :as mgen]
            [robertluo.markov.laws :as laws]
            [robertluo.markov.learner :as learner]))

(defn- learn [known walk]
  (learner/learn estimate/counting known walk))

(def gen-knowledge
  "What may be known: a prior, then some walks learned."
  (gen/let [p mgen/prior
            walks (gen/vector mgen/walk 0 4)]
    (reduce learn (estimate/prior p) walks)))

(def gen-weights
  "Integer weights from each of one to five states to each, with at least one positive
   per row: counts that are exactly proportional to a chain."
  (gen/bind
   (gen/choose 1 5)
   (fn [n]
     (let [states (mapv #(keyword (str "s" %)) (range n))
           gen-row (gen/fmap #(into {} (filter (comp pos? val)) (zipmap states %))
                             (gen/such-that #(some pos? %)
                                            (gen/vector (gen/choose 0 9) n)))]
       (gen/fmap #(zipmap states %) (gen/vector gen-row n))))))

(defn- normalise [weights]
  (update-vals weights (fn [row]
                         (let [total (reduce + (vals row))]
                           (update-vals row #(double (/ % total)))))))

(defn- total [counts]
  (transduce (mapcat vals) + 0 (vals counts)))

(defspec estimate-is-a-chain 200
  (prop/for-all [knowledge (gen/such-that seq gen-knowledge)]
                (m/validate chain/Chain (estimate/estimate knowledge))))

(defspec counts-combine-with-nothing-as-identity 100
  (laws/identity-law estimate/counting gen-knowledge))

(defspec counts-combine-associatively 100
  (laws/associativity-law estimate/counting gen-knowledge))

(defspec counting-in-parts-knows-the-same-as-counting-at-once 200
  (laws/parts-law estimate/counting gen-knowledge mgen/walk-split))

(defspec learning-order-does-not-matter 200
  (prop/for-all [knowledge gen-knowledge
                 walks (gen/vector mgen/walk 0 5)]
                (= (reduce learn knowledge walks)
                   (reduce learn knowledge (reverse walks)))))

(defspec learning-counts-one-transition-per-step 200
  (prop/for-all [knowledge gen-knowledge
                 walk mgen/walk]
                (= (+ (total knowledge) (max 0 (dec (count walk))))
                   (total (learn knowledge walk)))))

(defspec plain-estimate-allows-only-observed-transitions 200
  (prop/for-all [walk (gen/not-empty mgen/walk)]
                (let [counts (learn {} walk)]
                  (every? (fn [[from row]]
                            (let [seen (get counts from)]
                              (if (seq seen)
                                (= (set (keys seen)) (set (keys row)))
                                (= {from 1.0} row))))
                          (estimate/estimate counts)))))

(defspec smoothed-prior-allows-every-transition-between-its-states 200
  (prop/for-all [states (gen/not-empty (gen/set (gen/elements [:a :b :c :d])))
                 walk (gen/vector (gen/elements [:a :b :c :d]))]
                (let [est (estimate/estimate (learn (estimate/prior {:states states :alpha 1})
                                                    walk))]
                  (every? (fn [from] (every? #(pos? (get-in est [from %] 0.0)) states))
                          states))))

(defspec estimate-recovers-the-chain-of-proportional-counts 200
  (prop/for-all [weights gen-weights
                 k (gen/choose 1 5)]
                (< (estimate/distance (normalise weights)
                                      (estimate/estimate (update-vals weights
                                                                      #(update-vals % (partial * k)))))
                   1e-9)))

(defspec distance-is-a-metric 200
  (prop/for-all [[a b c] (gen/bind (gen/choose 1 4)
                                   (fn [n]
                                     (gen/vector (gen/such-that #(= n (count %))
                                                                (gen/fmap normalise gen-weights)
                                                                100)
                                                 3)))]
                (and (zero? (estimate/distance a a))
                     (= (estimate/distance a b) (estimate/distance b a))
                     (<= (estimate/distance a c)
                         (+ (estimate/distance a b) (estimate/distance b c) 1e-9)))))

(deftest learns-from-a-walk-that-is-never-held
  (testing "an eduction caches nothing, so learning from it holds no state, even guarded"
    (let [walk (eduction (map #(if (even? %) :a :b)) (range 1000000))]
      (is (= {:a {:b 500000} :b {:a 499999}} (learn {} walk))))))

(defn- refused? [kind f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= kind (:type (ex-data e))))))

(deftest guarded-by-malli
  (testing "a well-formed call passes"
    (is (= {:a {:b 1} :b {}} (learn {} [:a :b])))
    (is (= {:a {:b 1.0} :b {:b 1.0}} (estimate/estimate {:a {:b 1} :b {}})))
    (is (= {:a {:a 1 :b 1} :b {:a 1 :b 1}} (estimate/prior {:states #{:a :b} :alpha 1})))
    (is (= 1.0 (estimate/distance {:a {:a 1.0}} {:b {:b 1.0}}))))
  (testing "a negative pseudo-count is refused"
    (is (refused? :malli.core/invalid-input estimate/prior {:states #{:a} :alpha -1})))
  (testing "a zero count is refused"
    (is (refused? :malli.core/invalid-input estimate/estimate {:a {:a 0}})))
  (testing "counts naming a next state without a row are refused"
    (is (refused? :malli.core/invalid-input learn {:a {:z 1}} [:a])))
  (testing "a walk that is not a collection is refused"
    (is (refused? :malli.core/invalid-input learn {} :a)))
  (testing "a walk of something other than states is refused by the counting step, which
            the learner calls through its var"
    (is (refused? :malli.core/invalid-input learn {} ["a" "b"])))
  (testing "knowing nothing makes no chain"
    (is (refused? :malli.core/invalid-output estimate/estimate {}))))
