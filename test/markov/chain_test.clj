(ns markov.chain-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [markov.chain :as chain]))

(def gen-chain
  "A chain of one to six states, each row normalised from non-negative integer weights
   with at least one positive, so rows carry zero probabilities as well."
  (gen/bind
   (gen/choose 1 6)
   (fn [n]
     (let [states (mapv #(keyword (str "s" %)) (range n))
           gen-row (gen/fmap
                    (fn [weights]
                      (let [total (reduce + weights)]
                        (zipmap states (map #(double (/ % total)) weights))))
                    (gen/such-that #(some pos? %)
                                   (gen/vector (gen/choose 0 9) n)))]
       (gen/fmap #(zipmap states %) (gen/vector gen-row n))))))

(def gen-row
  (gen/bind gen-chain #(gen/elements (vals %))))

(def gen-draw
  (gen/such-that #(< % 1.0) (gen/double* {:min 0.0 :max 1.0 :NaN? false})))

(defspec next-state-has-positive-probability 200
  (prop/for-all [row gen-row
                 u gen-draw]
    (pos? (get row (chain/next-state row u)))))

(defspec next-state-picks-each-state-in-proportion-to-its-probability 200
  ;; Draws spread evenly over [0, 1) land in each interval as often as its width allows,
  ;; to within one draw at either end.
  (prop/for-all [row gen-row
                 n (gen/choose 10 500)]
    (let [picked (frequencies (for [i (range n)]
                                (chain/next-state row (/ (+ i 0.5) n))))]
      (every? (fn [[s p]] (<= (abs (- (get picked s 0) (* p n))) 1.0))
              row))))

(defspec walk-follows-only-positive-transitions 200
  (prop/for-all [[chain start us] (gen/bind gen-chain
                                            #(gen/tuple (gen/return %)
                                                        (gen/elements (keys %))
                                                        (gen/vector gen-draw)))]
    (let [path (chain/walk chain start us)]
      (and (= (inc (count us)) (count path))
           (= start (first path))
           (every? (fn [[a b]] (pos? (get-in chain [a b])))
                   (partition 2 1 path))))))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest guarded-by-malli
  (let [chain {:a {:a 0.5 :b 0.5} :b {:a 1.0}}]
    (testing "a well-formed call passes"
      (is (= :b (chain/next-state (:a chain) 0.7)))
      (is (= [:a :b :a] (chain/walk chain :a [0.7 0.3]))))
    (testing "a row that does not sum to one is refused"
      (is (refused? chain/next-state {:a 0.5} 0.3)))
    (testing "a draw outside [0, 1) is refused"
      (is (refused? chain/next-state (:a chain) 1.0)))
    (testing "a chain whose row names a state outside it is refused"
      (is (refused? chain/walk {:a {:z 1.0}} :a [0.3])))
    (testing "a start outside the chain is refused"
      (is (refused? chain/walk chain :z [0.3])))))
