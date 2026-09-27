(ns robertluo.markov.cssr
  "Learning the hidden states of a process from its symbols, by CSSR, Causal-State
   Splitting Reconstruction (Shalizi & Klinkner, 2004), in a simplified form. Two pasts
   are in the same state when they predict the same next symbol; the states are built
   from that definition.

   `(cssr params)` is a robertluo.markov.learner. The evidence is suffix counts, what
   followed each recent past: plain counts, gathered in one fold, combined by adding. The
   states are found by the readout, `reconstruct`:

   1. grow: extend suffixes into the past one symbol at a time, and put each in the state
      of its parent if a chi-square test at significance α finds them predicting alike,
      else in the best-matching other state, else in a new one;
   2. keep the recurrent states: those where a suffix, extended further into the past,
      stays. A state of pasts too short to reveal where the process is (such as the all-1s
      suffixes of the even process) is transient. This criterion stands in for CSSR's own
      removal of transient states from the transition graph;
   3. determinize: split states until each sends all its longest suffixes to one state per
      symbol, leaving out next suffixes that land in no recurrent state (unresolved);
   4. prune states left with nowhere to go.

   The result is a unifilar robertluo.markov.hidden machine, or nil when no state
   survives, as when there is too little data."
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.hidden :as hidden]
            [robertluo.markov.learner :as learner])
  (:import [org.apache.commons.math3.stat.inference ChiSquareTest]))

(def Suffix
  "The last few symbols of the past, oldest first."
  [:vector chain/State])

(def SuffixCounts
  "For each suffix of the past, how many times each symbol came next."
  [:map-of Suffix [:map-of chain/State pos-int?]])

(def Params
  "The longest suffix looked at, and the significance level of the test that tells two
   predictions apart: a smaller α splits states less readily."
  [:map
   [:l-max pos-int?]
   [:alpha [:double {:min 0.0 :max 1.0}]]])

(defn suffix-events
  "A transducer from symbols to [suffix next] events: at each position with at least
   `l-max` symbols before it, one per suffix length from 0 to `l-max`. Counting only
   positions with full context makes every suffix length count the same positions, and
   makes learning in parts exact when each part after the first starts with the last
   `l-max` symbols of the one before, as context."
  {:malli/schema [:=> [:cat nat-int?] ifn?]}
  [l-max]
  (fn [rf]
    (let [past (volatile! [])]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc sym]
         (let [p   @past
               acc (if (= l-max (count p))
                     (reduce (fn [acc l]
                               (let [r (rf acc [(subvec p (- l-max l)) sym])]
                                 (if (reduced? r) (reduced r) r)))
                             acc (range (inc l-max)))
                     acc)]
           (vswap! past #(let [w (conj % sym)] (if (> (count w) l-max) (subvec w 1) w)))
           acc))))))

(defn step
  "What `counts` becomes on one more [suffix next] event."
  {:malli/schema [:=> [:cat SuffixCounts [:tuple Suffix chain/State]] SuffixCounts]}
  [counts [suffix sym]]
  (update-in counts [suffix sym] (fnil inc 0)))

(defn combine
  "What was counted apart, as one: the counts added."
  {:malli/schema [:=> [:cat SuffixCounts SuffixCounts] SuffixCounts]}
  [a b]
  (merge-with #(merge-with + %1 %2) a b))

;; The state search below works on a handful of states and on suffixes of bounded length,
;; small by definition.

(defn- p-value
  "How likely counts this different are, were both drawn from one distribution."
  [a b]
  (let [syms (vec (into (set (keys a)) (keys b)))]
    (if (< (count syms) 2)
      1.0
      (.chiSquareTestDataSetsComparison (ChiSquareTest.)
                                        (long-array (map #(get a % 0) syms))
                                        (long-array (map #(get b % 0) syms))))))

(defn- pooled [counts suffixes]
  (transduce (map counts) (completing #(merge-with + %1 %2)) {} suffixes))

(defn- place
  "The states once `child` is placed: in `parent`'s state if it predicts alike, else in
   the other state it matches best, else in a new state."
  [counts alpha states parent child]
  (let [c (counts child)
        p #(p-value c (pooled counts (nth states %)))]
    (if (> (p parent) alpha)
      (update states parent conj child)
      (let [[best pv] (apply max-key second [nil -1.0]
                             (for [i (range (count states)) :when (not= i parent)]
                               [i (p i)]))]
        (if (and best (> pv alpha))
          (update states best conj child)
          (conj states #{child}))))))

(defn- grow [counts {:keys [l-max alpha]}]
  (let [alphabet (keys (counts []))]
    (reduce (fn [states l]
              ;; states only grow at the end, so an index taken here stays valid
              (reduce (fn [states [parent x]]
                        (reduce (fn [states a]
                                  (let [child (into [a] x)]
                                    (if (counts child)
                                      (place counts alpha states parent child)
                                      states)))
                                states alphabet))
                      states
                      (for [[i s] (map-indexed vector states) x s :when (= l (count x))] [i x])))
            [#{[]}]
            (range l-max))))

(defn- index-of [states]
  (into {} (for [[i s] (map-indexed vector states) x s] [x i])))

(defn- recurrent-in
  "A predicate on `states`: whether some suffix in a state, extended one symbol further
   into the past, stays in it."
  [states alphabet]
  (let [of (index-of states)]
    (fn [s]
      (let [i (of (first s))]
        (boolean (some (fn [x] (some #(= i (of (into [%] x))) alphabet)) s))))))

(defn- successors
  "Where suffix `x` goes on each symbol seen after it: the index of the state holding x
   with the symbol added and its oldest symbol dropped, when some state holds it."
  [counts of x]
  (into {}
        (keep (fn [[b _]] (when-let [j (of (subvec (conj x b) 1))] [b j])))
        (counts x)))

(defn- compatible? [a b]
  (every? (fn [[k v]] (or (not (contains? b k)) (= v (b k)))) a))

(defn- split-by-successors [counts states]
  (let [of (index-of states)]
    (into []
          (mapcat (fn [s]
                    (let [keyed (sort-by (comp - count second)
                                         (map (juxt identity #(successors counts of %)) s))]
                      (vals (reduce (fn [groups [x k]]
                                      (if-let [g (first (filter #(compatible? k %) (keys groups)))]
                                        (-> groups
                                            (dissoc g)
                                            (assoc (merge g k) (conj (groups g) x)))
                                        (assoc groups k #{x})))
                                    {} keyed)))))
          states)))

(defn- determinize [counts states]
  (->> (iterate #(split-by-successors counts %) states)
       (partition 2 1)
       (drop-while (fn [[a b]] (not= (count a) (count b))))
       ffirst))

(defn- moves
  "Where each of `states` goes on each symbol it was seen to emit, by index; a symbol
   whose next suffix is unresolved is left out."
  [counts states]
  (let [of (index-of states)]
    (mapv (fn [s]
            (into {} (keep (fn [[b _]] (when-let [j (some #(get (successors counts of %) b) s)]
                                         [b j])))
                  (pooled counts s)))
          states)))

(defn- prune
  "Drop states with nowhere to go, until every state left has somewhere. Dropping one can
   leave another with nowhere, so this repeats."
  [counts states]
  (let [ms   (moves counts states)
        kept (into [] (keep-indexed (fn [i s] (when (seq (ms i)) s))) states)]
    (if (= (count kept) (count states)) states (recur counts kept))))

(def Reconstruction
  [:map
   [:grown {:doc "the states as grown, before any is dropped"} [:vector [:set Suffix]]]
   [:recurrent {:doc "for each grown state, whether it is kept"} [:vector :boolean]]
   [:states {:doc "the final states, by their longest suffixes"} [:vector [:set Suffix]]]
   [:machine {:doc "nil when no state survives"} [:maybe hidden/Unifilar]]])

(defn reconstruct
  "The states `counts` reveal under `params`, and the machine they make. Its states are
   named :s0, :s1, … in the order they were found."
  {:malli/schema [:=> [:cat Params SuffixCounts] Reconstruction]}
  [{:keys [l-max] :as params} counts]
  (let [grown      (grow counts params)
        recurrent? (recurrent-in grown (keys (counts [])))
        flags      (mapv recurrent? grown)
        full       (into []
                         (comp (keep-indexed (fn [i s] (when (flags i) s)))
                               (map (fn [s] (into #{} (filter #(= l-max (count %))) s)))
                               (filter seq))
                         grown)
        states     (prune counts (determinize counts full))
        id         #(keyword (str "s" %))]
    {:grown     grown
     :recurrent flags
     :states    states
     :machine   (when (seq states)
                  (into {}
                        (map-indexed
                         (fn [i [s m]]
                           (let [c     (pooled counts s)
                                 total (reduce + 0 (map c (keys m)))]
                             [(id i) (into {} (map (fn [[b j]] [b {(id j) (/ (c b) total)}])) m)])))
                        (map vector states (moves counts states))))}))

(defn readout
  "The machine `counts` reveal under `params`, or nil."
  {:malli/schema [:=> [:cat Params SuffixCounts] [:maybe hidden/Unifilar]]}
  [params counts]
  (:machine (reconstruct params counts)))

(defn cssr
  "The learner of states: symbols in, suffix counts known, a machine (or nil) out."
  {:malli/schema [:=> [:cat Params] learner/Learner]}
  [{:keys [l-max] :as params}]
  {:events    (suffix-events l-max)
   :statistic SuffixCounts
   :empty     {}
   :step      #'step
   :combine   #'combine
   :readout   (fn [counts] (readout params counts))})
