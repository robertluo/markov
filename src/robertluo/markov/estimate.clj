(ns robertluo.markov.estimate
  "Learning a chain from observations, progressively. What is known is kept as counts:
   how many times each state was seen followed by each next state. A prior is counts
   imagined before any observation (pseudo-counts); learning a walk adds its counts; and
   what has been learned is the prior for whatever comes next, so

     (-> (prior {:states #{:a :b} :alpha 1}) (learn walk-1) (learn walk-2))

   knows the same as learning both walks at once, in either order. estimate turns what is
   known, at any point, into the chain most likely to have made it.

   Counts are all a Markov chain can learn from a walk: the next state depends on today
   only, so the order of the steps adds nothing once they are counted."
  (:require [robertluo.markov.chain :as chain]))

(defn- closed? [counts]
  (every? #(contains? counts %) (mapcat keys (vals counts))))

(def Counts
  "How many times each known state was followed by each next state, observed or imagined
   (a pseudo-count need not be whole). Every next state is a known state; a state known
   but never followed, as the last of a walk, has an empty row."
  [:and
   [:map-of chain/State [:map-of chain/State [:and number? pos?]]]
   [:fn {:error/message "every next state must have a row"} closed?]])

(def Prior
  "What is known before any observation: the `:states` known to exist, and a pseudo-count
   `:alpha` (default 0) for every transition between them."
  [:map
   [:states [:set chain/State]]
   [:alpha {:optional true} [:and number? [:>= 0]]]])

(defn prior
  "The counts `prior` stands for: α for every transition between its states."
  {:malli/schema [:=> [:cat Prior] Counts]}
  [{:keys [states alpha] :or {alpha 0}}]
  (let [row (if (pos? alpha) (zipmap states (repeat alpha)) {})]
    (zipmap states (repeat row))))

(defn- transitions
  "A transducer from states to [previous state] pairs, previous nil for the first."
  [rf]
  (let [prev (volatile! nil)]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc s] (let [p @prev]
                 (vreset! prev s)
                 (rf acc [p s]))))))

(defn- tally [counts [from to]]
  (cond-> (update counts to #(or % {}))
    from (update-in [from to] (fnil inc 0))))

(def ^:private Walk
  "A walk of any length, unbounded included: checked only for being seqable, since
   checking its states would realise it. The guard on what learn answers checks them."
  [:fn {:error/message "should be a seqable collection of states"} seqable?])

(defn learn
  "What `knowledge` becomes on observing `walk`: each of its states known, each of its
   transitions counted once more. The walk is consumed as it is read, so a lazy one of any
   length is never held whole. A walk continuing an earlier one should start with that
   one's last state, or the transition between them is lost."
  {:malli/schema [:=> [:cat Counts Walk] Counts]}
  [knowledge walk]
  (transduce transitions (completing tally) knowledge walk))

(defn- normalise [from row]
  (let [total (reduce + 0 (vals row))]
    (if (zero? total)
      {from 1.0}
      (update-vals row #(double (/ % total))))))

(defn estimate
  "The chain most likely to have made the counts `knowledge` holds: each row over its
   total. A state with nothing to go on (nothing seen or imagined to follow it) stays put,
   so the estimate is always a chain."
  {:malli/schema [:=> [:cat Counts] chain/Chain]}
  [knowledge]
  (into {} (map (fn [[from row]] [from (normalise from row)])) knowledge))

(defn- row-distance [ra rb]
  (if (and ra rb)
    (min 1.0 (* 0.5 (transduce (map #(abs (- (get ra % 0.0) (get rb % 0.0))))
                               + 0.0
                               (into (set (keys ra)) (keys rb)))))
    1.0))

(defn distance
  "How far apart two chains are: the largest total-variation distance between their rows
   for the same state, that is the most probability two rows give differently to any set
   of next states. 0 when the chains agree; a state only one of them has counts as the
   maximum, 1."
  {:malli/schema [:=> [:cat chain/Chain chain/Chain] [:double {:min 0.0 :max 1.0}]]}
  [a b]
  (transduce (map #(row-distance (get a %) (get b %)))
             max 0.0
             (into (set (keys a)) (keys b))))
