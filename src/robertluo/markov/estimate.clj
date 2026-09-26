(ns robertluo.markov.estimate
  "Learning a chain from observations: count the transitions seen in some walks, and
   estimate the chain most likely to have made them.

   Everything a Markov chain can learn from a walk is in its steps, how often each state
   was followed by each other state: the next state depends on today only, so the order
   of the steps adds nothing once they are counted."
  (:require [robertluo.markov.chain :as chain]))

(def Counts
  "How many times each observed state was followed by each next state. A state observed
   but never followed, as the last of a walk, has an empty row."
  [:map-of chain/State [:map-of chain/State pos-int?]])

(def Prior
  "What is known before the observations: `:states` known to exist, observed or not, and
   a pseudo-count `:alpha` added to every transition. Both default to nothing."
  [:map
   [:states {:optional true} [:set chain/State]]
   [:alpha {:optional true} [:and number? [:>= 0]]]])

(defn counts
  "The transitions counted over every walk of `walks`."
  {:malli/schema [:=> [:cat [:sequential [:sequential chain/State]]] Counts]}
  [walks]
  (reduce (fn [acc walk]
            (reduce (fn [acc [from to]] (update-in acc [from to] (fnil inc 0)))
                    (reduce #(update %1 %2 (fnil identity {})) acc walk)
                    (partition 2 1 walk)))
          {}
          walks))

(defn estimate
  "The chain `counts` suggest under `prior`. Its states are those observed and those the
   prior names. Each row is the counts of that state plus α for every state, over their
   total: with α zero, the maximum-likelihood estimate. A state with nothing to go on (no
   transitions observed from it, α zero) stays put, so the estimate is always a chain.

   The state space is what was observed plus what the prior adds, rather than the
   prior's alone, so that no constraint spans both arguments."
  {:malli/schema [:=> [:cat Prior Counts] chain/Chain]}
  [{:keys [states alpha] :or {states #{} alpha 0}} counts]
  (let [space (into states (concat (keys counts) (mapcat keys (vals counts))))]
    (into {}
          (for [from space
                :let [row   (get counts from {})
                      total (+ (reduce + (vals row)) (* alpha (count space)))]]
            [from (if (zero? total)
                    {from 1.0}
                    (into {}
                          (for [to space
                                :let [p (/ (+ (get row to 0) alpha) total)]
                                :when (pos? p)]
                            [to (double p)])))]))))

(defn distance
  "How far apart two chains are: the largest total-variation distance between their rows
   for the same state, that is the most probability two rows give differently to any set
   of next states. 0 when the chains agree; a state only one of them has counts as the
   maximum, 1."
  {:malli/schema [:=> [:cat chain/Chain chain/Chain] [:double {:min 0.0 :max 1.0}]]}
  [a b]
  (reduce max 0.0
          (for [s (into (set (keys a)) (keys b))
                :let [ra (get a s) rb (get b s)]]
            (if (and ra rb)
              (min 1.0 (* 0.5 (reduce + (for [t (into (set (keys ra)) (keys rb))]
                                          (abs (- (get ra t 0.0) (get rb t 0.0)))))))
              1.0))))
