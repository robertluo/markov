(ns robertluo.markov.timed-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [robertluo.markov.laws :as laws]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.timed :as timed]))

(def ^:private machine
  {:up       {:degraded 0.2 :down 0.02}
   :degraded {:up 0.5 :down 0.3}
   :down     {:up 1.0}})

(def ^:private gen-draw
  (gen/such-that #(< % 1.0) (gen/double* {:min 0.0 :max 1.0 :NaN? false})))

(def ^:private gen-sojourns
  "A trajectory of the machine, from pairs of generated draws."
  (gen/fmap #(into [] (timed/sojourns machine :up) %)
            (gen/vector (gen/tuple gen-draw gen-draw) 0 20)))

(def ^:private gen-known
  (gen/fmap #(reduce (fn [k s] (learner/learn timed/timed k s)) (:empty timed/timed) %)
            (gen/vector gen-sojourns 0 3)))

(def ^:private gen-split
  "[whole first second]: a trajectory cut inside one of its sojourns, which each part
   keeps a share of."
  (gen/let [whole (gen/not-empty gen-sojourns)
            i (gen/choose 0 (dec (count whole)))
            f (gen/double* {:min 0.0 :max 1.0 :NaN? false})]
    (let [[s held] (nth whole i)
          before (* f held)]
      [whole
       (conj (subvec whole 0 i) [s before])
       (into [[s (- held before)]] (subvec whole (inc i)))])))

(defn- draws
  "Unbounded draws for `seed`, caching nothing."
  [seed]
  (let [base (.nextLong (java.util.SplittableRandom. seed))]
    (eduction (map #(.nextDouble (java.util.SplittableRandom. (+ base %)))) (range))))

(defspec timed-combine-with-nothing-as-identity 100
  (laws/identity-law timed/timed gen-known))

(defspec timed-combine-associatively 100
  (laws/associativity-law timed/timed gen-known))

(defspec timed-in-parts-knows-the-same-as-timed-at-once 200
  (laws/parts-law timed/timed gen-known gen-split))

(defspec sojourns-jump-only-along-positive-rates 200
  (prop/for-all [sojourns gen-sojourns]
                (every? (fn [[[a _] [b _]]] (pos? (get-in machine [a b] 0)))
                        (partition 2 1 sojourns))))

(defspec for-hours-watches-exactly-that-long 100
  ;; the draws are unbounded; only the clock ends the watch
  (prop/for-all [seed gen/large-integer
                 hours (gen/choose 1 500)]
                (laws/approx= (double hours)
                              (transduce (comp (partition-all 2) (timed/sojourns machine :up)
                                               (timed/for-hours hours) (map second))
                                         + 0.0 (draws seed)))))

(defspec every-hour-held-and-every-jump-is-known 200
  (prop/for-all [sojourns gen-sojourns]
                (let [{:keys [held jumps]} (learner/learn timed/timed (:empty timed/timed) sojourns)]
                  (and (laws/approx= (transduce (map second) + 0.0 sojourns)
                                     (reduce + 0.0 (vals held)))
                       (= (max 0 (dec (count sojourns)))
                          (transduce (mapcat vals) + 0 (vals jumps)))))))

(defspec rates-recover-the-rates-of-proportional-evidence 200
  ;; hold each state h hours and jump q·h times along a rate q: the rates are q
  (prop/for-all [rates (gen/fmap #(into {} (map (fn [[s row]] [s (dissoc row s)])) %)
                                 (gen/map (gen/elements [:a :b :c])
                                          (gen/map (gen/elements [:a :b :c])
                                                   (gen/choose 1 5))))
                 hours (gen/choose 1 10)]
                (let [states (into (set (keys rates)) (mapcat keys (vals rates)))
                      known {:held (zipmap states (repeat (double hours)))
                             :jumps (into {} (comp (map (fn [[s row]] [s (update-vals row #(* % hours))]))
                                                   (filter (comp seq second)))
                                          rates)}]
                  (laws/approx= (merge (zipmap states (repeat {})) rates)
                                (learner/readout timed/timed known)))))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest the-last-sojourn-counts-its-time-but-no-jump
  (is (= {:held {:up 2.0 :down 1.5} :jumps {:up {:down 1}}}
         (learner/learn timed/timed (:empty timed/timed) [[:up 2.0] [:down 1.5]]))))

(deftest guarded-by-malli
  (testing "a watch of no time is refused"
    (is (refused? timed/for-hours 0)))
  (testing "a sojourn of negative time is refused by the step"
    (is (refused? learner/learn timed/timed (:empty timed/timed) [[:up -1.0] [:down 1.0]])))
  (testing "a rate that is not positive is refused"
    (is (refused? timed/sojourns {:up {:down 0}} :up))))
