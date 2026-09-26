(ns robertluo.markov.estimate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]))

(def gen-walks
  "Walks over a few states, any of them empty or a single state."
  (gen/vector (gen/vector (gen/elements [:a :b :c :d]))))

(def gen-prior
  (gen/let [states (gen/set (gen/elements [:c :d :e]))
            alpha (gen/elements [0 1/2 1 3])]
    {:states states :alpha alpha}))

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

(defn- merge-counts [a b]
  (merge-with #(merge-with + %1 %2) a b))

(defspec estimate-is-a-chain 200
  (prop/for-all [walks gen-walks
                 prior gen-prior]
                (let [cs (estimate/counts walks)]
                  (or (and (empty? cs) (empty? (:states prior)))    ; nothing to estimate from
                      (m/validate chain/Chain (estimate/estimate prior cs))))))

(defspec counts-add-up-over-walks 200
  (prop/for-all [walks1 gen-walks
                 walks2 gen-walks]
                (= (estimate/counts (concat walks1 walks2))
                   (merge-counts (estimate/counts walks1) (estimate/counts walks2)))))

(defspec counts-one-transition-per-step 200
  (prop/for-all [walks gen-walks]
                (= (reduce + (map #(max 0 (dec (count %))) walks))
                   (reduce + (mapcat vals (vals (estimate/counts walks)))))))

(defspec plain-estimate-allows-only-observed-transitions 200
  (prop/for-all [walks (gen/such-that seq gen-walks)]
                (let [cs (estimate/counts walks)]
                  (or (empty? cs)
                      (every? (fn [[from row]]
                                (let [seen (get cs from)]
                                  (if (seq seen)
                                    (= (set (keys seen)) (set (keys row)))
                                    (= {from 1.0} row))))
                              (estimate/estimate {} cs))))))

(defspec smoothed-estimate-allows-every-transition 200
  (prop/for-all [walks gen-walks
                 prior (gen/fmap #(assoc % :alpha 1) gen-prior)]
                (let [cs (estimate/counts walks)]
                  (or (and (empty? cs) (empty? (:states prior)))
                      (let [est (estimate/estimate prior cs)
                            states (set (keys est))]
                        (every? #(= states (set (keys %))) (vals est)))))))

(defspec estimate-recovers-the-chain-of-proportional-counts 200
  (prop/for-all [weights gen-weights
                 k (gen/choose 1 5)]
                (let [chain (normalise weights)]
                  (< (estimate/distance chain
                                        (estimate/estimate {} (update-vals weights
                                                                           #(update-vals % (partial * k)))))
                     1e-9))))

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

(defn- refused? [kind f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= kind (:type (ex-data e))))))

(deftest guarded-by-malli
  (testing "a well-formed call passes"
    (is (= {:a {:b 1} :b {}} (estimate/counts [[:a :b]])))
    (is (= {:a {:b 1.0} :b {:b 1.0}} (estimate/estimate {} {:a {:b 1} :b {}})))
    (is (= 1.0 (estimate/distance {:a {:a 1.0}} {:b {:b 1.0}}))))
  (testing "a negative pseudo-count is refused"
    (is (refused? :malli.core/invalid-input estimate/estimate {:alpha -1} {:a {:a 1}})))
  (testing "a zero count is refused"
    (is (refused? :malli.core/invalid-input estimate/estimate {} {:a {:a 0}})))
  (testing "no observations and no known states make no chain"
    (is (refused? :malli.core/invalid-output estimate/estimate {} {}))))
