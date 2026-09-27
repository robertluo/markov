(ns robertluo.markov.cssr-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [robertluo.markov.cssr :as cssr]
            [robertluo.markov.hidden :as hidden]
            [robertluo.markov.laws :as laws]
            [robertluo.markov.learner :as learner]))

(def ^:private params {:l-max 3 :alpha 0.001})
(def ^:private learner (cssr/cssr params))

(def ^:private gen-symbols
  (gen/vector (gen/elements [:0 :1]) 0 60))

(def ^:private gen-known
  (gen/fmap #(learner/learn learner {} %) gen-symbols))

(def ^:private gen-split
  "[whole first second]: symbols cut at a position after the first l-max, the second part
   starting with the last l-max symbols of the first, as context."
  (gen/let [whole (gen/vector (gen/elements [:0 :1]) 3 60)
            i (gen/choose 3 (count whole))]
    [whole (subvec whole 0 i) (subvec whole (- i 3))]))

(defspec suffix-counts-combine-with-nothing-as-identity 100
  (laws/identity-law learner gen-known))

(defspec suffix-counts-combine-associatively 100
  (laws/associativity-law learner gen-known))

(defspec counting-suffixes-in-parts-knows-the-same-as-at-once 200
  (laws/parts-law learner gen-known gen-split))

(defspec every-suffix-length-counts-the-same-positions 200
  ;; one position per symbol after the first l-max, counted once at each suffix length
  (prop/for-all [syms gen-symbols
                 l (gen/choose 1 4)]
                (let [counts (learner/learn (cssr/cssr {:l-max l :alpha 0.01}) {} syms)
                      totals (update-vals (group-by (comp count key) counts)
                                          #(transduce (mapcat (comp vals val)) + 0 %))]
                  (every? #(= (max 0 (- (count syms) l)) (get totals % 0)) (range (inc l))))))

(defspec a-suffix-counts-what-its-extensions-count 200
  ;; what followed a suffix is what followed its one-symbol-longer extensions, together
  (prop/for-all [syms gen-symbols]
                (let [counts (learner/learn learner {} syms)]
                  (every? (fn [[suffix row]]
                            (or (= 3 (count suffix))
                                (= row (apply merge-with + {} (keep #(counts (into [%] suffix)) [:0 :1])))))
                          counts))))

(defspec a-reconstruction-is-a-machine-or-nothing 100
  ;; whatever the symbols, the readout is a unifilar machine or nil; its guard checks it
  (prop/for-all [syms (gen/vector (gen/elements [:0 :1]) 0 300)]
                (let [machine (learner/readout learner (learner/learn learner {} syms))]
                  (or (nil? machine) (m/validate hidden/Unifilar machine)))))

(defn- emitted [machine start seed n]
  (let [r (java.util.Random. seed)]
    (into [] (comp (partition-all 2) (hidden/emit machine start) (take n))
          (repeatedly #(.nextDouble r)))))

(defn- learned [machine seed n]
  (learner/readout learner (learner/learn learner {} (emitted machine (key (first machine)) seed n))))

(defn- close? [a b] (< (abs (- a b)) 0.05))

(deftest recovers-the-even-process
  ;; A: 0 back to A or 1 on to B, half each; B: always 1, back to A
  (let [machine (learned {:A {:0 {:A 1/2} :1 {:B 1/2}} :B {:1 {:A 1}}} 11 20000)
        [b a]   (map key (sort-by (comp count val) machine))]
    (is (= 2 (count machine)))
    (is (= {:1 {a 1}} (machine b)))
    (is (= #{:0 :1} (set (keys (machine a)))))
    (is (close? 0.5 (get-in machine [a :0 a])))
    (is (close? 0.5 (get-in machine [a :1 b])))))

(deftest a-coin-has-one-state
  (let [machine (learned {:s {:0 {:s 1/2} :1 {:s 1/2}}} 12 20000)]
    (is (= 1 (count machine)))
    (is (close? 0.5 (get-in machine [(key (first machine)) :0 (key (first machine))])))))

(deftest recovers-the-golden-mean-process
  ;; no two 1s in a row: after a 1 comes a 0
  (let [machine (learned {:A {:0 {:A 1/2} :1 {:B 1/2}} :B {:0 {:A 1}}} 13 20000)]
    (is (= 2 (count machine)))
    (is (m/validate hidden/Unifilar machine))
    (is (some #(= [:0] (keys (val %))) machine))))

(deftest splits-states-that-predict-alike-but-lead-apart
  ;; A and B both toss a coin, so they predict alike, but A leads to B and B to C, which
  ;; emits 2 and returns to A: only splitting by where states go tells A from B
  (let [machine (learned {:A {:0 {:B 1/2} :1 {:B 1/2}}
                          :B {:0 {:C 1/2} :1 {:C 1/2}}
                          :C {:2 {:A 1}}}
                         5 20000)
        next-of (fn [s] (key (first (val (first (machine s))))))
        c       (key (first (filter #(= [:2] (keys (val %))) machine)))]
    (is (= 3 (count machine)))
    (testing "the states form the cycle C → A → B → C"
      (is (= c (-> c next-of next-of next-of)))
      (is (= 3 (count (set (take 3 (iterate next-of c)))))))))

(deftest too-little-data-leaves-no-machine
  (is (nil? (learner/readout learner (learner/learn learner {} [:0 :1 :0])))))

(deftest the-even-process-has-transient-states
  (let [counts (learner/learn learner {} (emitted {:A {:0 {:A 1/2} :1 {:B 1/2}} :B {:1 {:A 1}}}
                                                  :A 11 20000))
        {:keys [grown recurrent]} (cssr/reconstruct params counts)]
    (testing "the all-1s suffixes are grown into states, and dropped"
      (is (some #(contains? % [:1 :1]) (keep-indexed (fn [i s] (when-not (recurrent i) s)) grown))))))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest guarded-by-malli
  (testing "a significance outside [0, 1] is refused"
    (is (refused? cssr/cssr {:l-max 3 :alpha 1.5})))
  (testing "a longest suffix of 0 is refused"
    (is (refused? cssr/cssr {:l-max 0 :alpha 0.01})))
  (testing "a count that is not positive is refused"
    (is (refused? cssr/reconstruct params {[] {:0 0}}))))
