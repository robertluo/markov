(ns robertluo.markov.hidden-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [robertluo.markov.hidden :as hidden]))

(def ^:private even
  {:A {:0 {:A 1/2} :1 {:B 1/2}}
   :B {:1 {:A 1}}})

(def ^:private gen-draw
  (gen/double* {:min 0.0 :max (Math/nextDown 1.0) :NaN? false :infinite? false}))

(def gen-machine
  "A machine over two or three hidden states and two symbols, from integer weights with
   at least one positive per row, as exact fractions."
  (gen/let [n (gen/choose 2 3)
            weights (gen/vector (gen/such-that #(some pos? %)
                                               (gen/vector (gen/choose 0 3) (* 2 n)))
                                n)]
    (let [states (mapv #(keyword (str "s" %)) (range n))
          edges (for [sym [:0 :1] to states] [sym to])]
      (zipmap states
              (for [ws weights
                    :let [total (reduce + ws)]]
                (reduce (fn [row [[sym to] w]]
                          (if (pos? w) (assoc-in row [sym to] (/ w total)) row))
                        {} (map vector edges ws)))))))

(def ^:private gen-draw-pairs
  (gen/vector (gen/tuple gen-draw gen-draw) 0 40))

(defn- emitted [machine start pairs]
  (into [] (hidden/emit machine start) pairs))

(defspec a-machine-is-a-machine 100
  (prop/for-all [machine gen-machine]
                (m/validate hidden/Machine machine)))

(defspec tracking-the-emitting-machine-finds-every-symbol-possible 200
  ;; from its true start, the machine gives every symbol it emits a positive probability,
  ;; and every belief sums to exactly 1
  (prop/for-all [machine gen-machine
                 draws gen-draw-pairs]
                (let [start (first (sort (keys machine)))]
                  (every? (fn [{:keys [p belief]}]
                            (and (pos? p) (== 1 (reduce + (vals belief)))))
                          (sequence (hidden/track machine {start 1})
                                    (emitted machine start draws))))))

(defspec a-unifilar-belief-stays-certain-once-the-state-is-known 200
  ;; in the even process a 0 is only emitted from A, back to A
  (prop/for-all [draws gen-draw-pairs]
                (let [steps (sequence (hidden/track even {:A 2/3 :B 1/3}) (emitted even :A draws))]
                  (every? #(= 1 (count (:belief %)))
                          (drop-while #(not= :0 (:symbol %)) steps)))))

(deftest tracking-costs-the-entropy-rate
  ;; the even process's entropy rate is 2/3 bit per symbol
  (let [draws (let [r (java.util.Random. 7)] (vec (repeatedly 20000 #(vector (.nextDouble r) (.nextDouble r)))))
        steps (sequence (hidden/track even {:A 2/3 :B 1/3}) (emitted even :A draws))
        bits  (transduce (map #(- (/ (Math/log (:p %)) (Math/log 2)))) + 0.0 steps)]
    (is (< (abs (- (/ bits (count steps)) 2/3)) 0.02))))

(deftest unifilar
  (is (m/validate hidden/Unifilar even))
  (is (not (m/validate hidden/Unifilar {:A {:0 {:A 1/2 :B 1/2}} :B {:0 {:A 1}}}))))

(deftest a-symbol-ruled-out-leaves-the-belief-as-it-was
  (is (= {:A 1} (hidden/observe even {:A 1} :2))))

(defn- refused? [f & args]
  (try (apply f args)
       false
       (catch clojure.lang.ExceptionInfo e
         (= :malli.core/invalid-input (:type (ex-data e))))))

(deftest guarded-by-malli
  (testing "a row that does not sum to one is refused"
    (is (refused? hidden/emit {:A {:0 {:A 1/2}}} :A)))
  (testing "a next state outside the machine is refused"
    (is (refused? hidden/emit {:A {:0 {:Z 1}}} :A)))
  (testing "a belief with a zero in it is refused"
    (is (refused? hidden/predict even {:A 1 :B 0}))))
