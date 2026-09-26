;; # Learning the states: CSSR
;;
;; A lab. The even notebook tracked the even process with the true machine: it was handed
;; the states. Here they are learned from symbols alone, by building them from their
;; definition: two pasts are in the same state when they predict the same future. This is
;; CSSR, Causal-State Splitting Reconstruction (Shalizi & Klinkner, 2004), in a simplified
;; form, as a learner: the evidence is gathered in one fold, and the states are found by
;; the readout.

(ns robertluo.markov.cssr-notebook
  (:require [clojure.string :as str]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.hidden :as hidden]
            [robertluo.markov.instrument :as instrument]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind])
  (:import [org.apache.commons.math3.stat.inference ChiSquareTest]))

;; ## The data
;;
;; Symbols from the even process: runs of 1s between 0s are always even. The learner sees
;; only these.

(def ^:private even
  {:A {:0 {:A 1/2} :1 {:B 1/2}}
   :B {:1 {:A 1}}})

(defn- symbols [seed n]
  (into [] (comp (partition-all 2) (hidden/emit even :A) (take n)) (view/draw-stream seed)))

(def train (symbols 2 10000))
(def test-symbols (symbols 3 10000))

(apply str (map name (take 80 train)))

;; ## The evidence: what follows each suffix
;;
;; A suffix is the last few symbols of the past, oldest first. For every position, and
;; every suffix length from 0 up to `l-max`, count which symbol came next. Only positions
;; with a full `l-max` symbols before them are counted, so that every suffix length counts
;; the same positions.
;;
;; These counts are the **statistic** of a learner: the **events** are [suffix next]
;; pairs, the **step** counts one, and they **combine** by adding.

(def Suffix [:vector chain/State])

(def SuffixCounts
  "For each suffix of the past, how many times each symbol came next."
  [:map-of Suffix [:map-of chain/State pos-int?]])

(defn suffix-events
  "A transducer from symbols to [suffix next] events: at each position with at least
   `l-max` symbols before it, one per suffix length from 0 to `l-max`."
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

(defn suffix-step
  {:malli/schema [:=> [:cat SuffixCounts [:tuple Suffix chain/State]] SuffixCounts]}
  [counts [suffix sym]]
  (update-in counts [suffix sym] (fnil inc 0)))

(defn suffix-combine
  {:malli/schema [:=> [:cat SuffixCounts SuffixCounts] SuffixCounts]}
  [a b]
  (merge-with #(merge-with + %1 %2) a b))

^:kindly/hide-code (kind/hidden (instrument/instrument!))

(def ^:private l-max 3)

(def ^:private evidence
  (learner/learn {:events    (suffix-events l-max)
                  :statistic SuffixCounts
                  :empty     {}
                  :step      #'suffix-step
                  :combine   #'suffix-combine
                  :readout   identity}
                 {} train))

(defn- p-one [counts]
  (let [n (reduce + 0 (vals counts))]
    (double (/ (get counts :1 0) n))))

(kind/table
 {:column-names ["suffix" "then 0" "then 1" "P(1 next)"]
  :row-vectors (for [[suffix counts] (sort-by (comp (juxt count identity) key) evidence)
                     :when (<= (count suffix) 2)]
                 [(apply str (map name suffix)) (get counts :0 0) (get counts :1 0)
                  (format "%.3f" (p-one counts))])})

;; Already the states show: after a 0 the next symbol is a coin (A); after 01 it is
;; surely 1 (B); after 1 or 11 it is a mixture, since those suffixes do not say where the
;; pairs of 1s begin.

;; ## Growing the states
;;
;; Start with one state holding the empty suffix. Then, length by length, extend each
;; suffix one symbol further into the past, and ask whether the longer suffix predicts
;; like the state its shorter parent is in: a chi-square test on the counts, at
;; significance α. If it does, it joins that state. If not, it joins whichever other state
;; it matches best, or, matching none, starts a new state. States split only when the
;; evidence demands it.

(def Params
  [:map
   [:l-max pos-int?]
   [:alpha [:double {:min 0.0 :max 1.0}]]])

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
                                    (if (counts child) (place counts alpha states parent child) states)))
                                states alphabet))
                      states
                      (for [[i s] (map-indexed vector states) x s :when (= l (count x))] [i x])))
            [#{[]}]
            (range l-max))))

(def ^:private grown (grow evidence {:l-max l-max :alpha 0.001}))

(defn- state-table [counts states recurrent?]
  (kind/table
   {:column-names ["state" "suffixes" "P(1 next)" "recurrent?"]
    :row-vectors (for [[i s] (map-indexed vector states)]
                   [i (str/join " " (sort (map #(if (seq %) (apply str (map name %)) "λ") s)))
                    (format "%.3f" (p-one (pooled counts s)))
                    (recurrent? s)])}))

;; ## Transient states
;;
;; Some states hold only suffixes that have not seen enough: the all-1s suffixes, whose
;; prediction is a mixture, because they do not reach back to the last 0. Such a state is
;; where the machine is before the symbols have revealed its state, never where it
;; returns to. So a state is kept only if some suffix in it, extended further into the
;; past, stays in it: once a past has revealed its state, more past does not change it.
;; (CSSR removes transient states from the transition graph; this criterion is a
;; simpler stand-in, enough for this page.)

(defn- recurrent-in [states]
  (let [of (into {} (for [[i s] (map-indexed vector states) x s] [x i]))]
    (fn [s]
      (let [i (of (first s))]
        (boolean (some (fn [x] (some #(= i (of (into [%] x))) [:0 :1])) s))))))

(state-table evidence grown (recurrent-in grown))

;; ## Making it a machine
;;
;; A machine needs to know where each state goes on each symbol. For a suffix x of the
;; longest length and a symbol b, the next suffix is x with b added and its oldest symbol
;; dropped. A state must send all its suffixes to the same state on b; where they
;; disagree, the state is split until they agree. A next suffix that lands in a transient
;; state is unresolved (the window lost the 0 that would settle it), and is left out.

(defn- successors [counts of x]
  (into {}
        (keep (fn [[b _]]
                (when-let [j (of (subvec (conj x b) 1))] [b j])))
        (counts x)))

(defn- compatible? [a b]
  (every? (fn [[k v]] (or (not (contains? b k)) (= v (b k)))) a))

(defn- split-by-successors [counts states]
  (let [of (into {} (for [[i s] (map-indexed vector states) x s] [x i]))]
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

(defn- determinize
  "Split states until each sends all its suffixes to one state per symbol. The number of
   states is small by definition, so iterating to a fixed point is cheap."
  [counts states]
  (->> (iterate #(split-by-successors counts %) states)
       (partition 2 1)
       (drop-while (fn [[a b]] (not= (count a) (count b))))
       ffirst))

(defn- moves
  "Where each of `states` goes on each symbol it was seen to emit, by index; a symbol
   whose next suffix is unresolved is left out."
  [counts states]
  (let [of (into {} (for [[i s] (map-indexed vector states) x s] [x i]))]
    (mapv (fn [s]
            (into {} (keep (fn [[b _]] (when-let [j (some #(get (successors counts of %) b) s)]
                                         [b j])))
                  (pooled counts s)))
          states)))

(defn- prune
  "Drop states with nowhere to go, until every state left has somewhere. Dropping one can
   leave another with nowhere, so this repeats; the states are few by definition."
  [counts states]
  (let [ms (moves counts states)
        kept (into [] (keep-indexed (fn [i s] (when (seq (ms i)) s))) states)]
    (if (= (count kept) (count states)) states (recur counts kept))))

(def Reconstruction
  [:map
   [:grown [:vector [:set Suffix]]]
   [:states [:vector [:set Suffix]]]
   [:machine {:doc "nil when no state survives"} [:maybe hidden/Machine]]])

(defn reconstruct
  "The states `counts` reveal under `params`, and the machine they make."
  {:malli/schema [:=> [:cat Params SuffixCounts] Reconstruction]}
  [{:keys [l-max] :as params} counts]
  (let [grown  (grow counts params)
        kept   (filterv (recurrent-in grown) grown)
        full   (into [] (comp (map (fn [s] (into #{} (filter #(= l-max (count %))) s)))
                              (filter seq))
                     kept)
        states (prune counts (determinize counts full))
        id     #(keyword (str "s" %))]
    {:grown   grown
     :states  states
     :machine (when (seq states)
                (into {}
                      (map-indexed
                       (fn [i [s m]]
                         (let [c     (pooled counts s)
                               total (reduce + 0 (map c (keys m)))]
                           [(id i) (into {} (map (fn [[b j]] [b {(id j) (/ (c b) total)}])) m)])))
                      (map vector states (moves counts states))))}))

(defn cssr
  "The learner of states: symbols in, suffix counts known, a machine out."
  {:malli/schema [:=> [:cat Params] learner/Learner]}
  [{:keys [l-max] :as params}]
  {:events    (suffix-events l-max)
   :statistic SuffixCounts
   :empty     {}
   :step      #'suffix-step
   :combine   #'suffix-combine
   :readout   (fn [counts] (:machine (reconstruct params counts)))})

^:kindly/hide-code (kind/hidden (instrument/instrument!))

(def ^:private learned (reconstruct {:l-max l-max :alpha 0.001} evidence))

(defn- machine-diagram [machine]
  (kind/mermaid
   (apply str "stateDiagram-v2\n"
          (for [[from row] machine
                [sym nexts] row
                [to p] nexts]
            (str "  " (name from) " --> " (name to) ": " (name sym) " | "
                 (format "%.2f" (double p)) "\n")))))

(machine-diagram (:machine learned))

;; Two states, as in the even process, up to their names: one emits 0 or 1 about half
;; the time each, going to the other on 1; the other always emits 1 and goes back.

;; ## How well it predicts
;;
;; Tracked from a uniform belief, on symbols it has not seen, against the ⅔-bit floor:

(defn- bits [p] (- (/ (Math/log p) (Math/log 2))))

(defn- test-loss [machine]
  (let [start (zipmap (keys machine) (repeat (/ 1 (count machine))))
        n     (- (count test-symbols) 10)]
    (/ (transduce (comp (hidden/track machine start) (drop 10) (map (comp bits :p)))
                  + 0.0 test-symbols)
       n)))

(test-loss (:machine learned))

;; ## Learning in parts
;;
;; Suffix counts are counts, so learning in parts knows the same as learning at once. The
;; second part starts with the last `l-max` symbols of the first, as context only: they
;; are not counted again.

(let [l     (cssr {:l-max l-max :alpha 0.001})
      [a b] [(subvec train 0 5000) (subvec train (- 5000 l-max))]]
  (= (learner/learn l {} train)
     (learner/combine l (learner/learn l {} a) (learner/learn l {} b))))

;; ## How much data, how long a suffix
;;
;; The page's machine came from 10,000 symbols with suffixes up to 3 long. How does it
;; fare with less, or with other settings?

(defn- trial [n params]
  (let [known   (learner/learn (cssr params) {} (subvec train 0 n))
        machine (learner/readout (cssr params) known)]
    [n (:l-max params) (:alpha params) (count machine)
     (if-not machine
       "no state survives"
       (let [loss (test-loss machine)]
         (if (Double/isInfinite loss) "∞ (rules out a symbol that comes)" (format "%.4f" loss))))]))

(kind/table
 {:column-names ["symbols" "l-max" "α" "states" "bits per symbol"]
  :row-vectors (concat
                (for [n [100 1000 10000] alpha [0.05 0.001]]
                  (trial n {:l-max 3 :alpha alpha}))
                (for [l [2 4 5]]
                  (trial 10000 {:l-max l :alpha 0.001})))})

;; - With 100 symbols the tests have too little to go on either way, and no setting finds
;;   the process.
;; - At 1,000, α = 0.05 already finds both states. The stricter α = 0.001 cannot tell the
;;   two transient mixtures (P(1) of 0.67 and 0.75) apart, merges them into one state, and
;;   that state then passes for recurrent, since extending 1 back to 11 stays in it: two
;;   extra states, and worse predictions. A loose α errs towards splitting, a strict one
;;   towards merging; here merging was the costly mistake.
;; - With suffixes of at most 2, the machine cannot be seen returning to B: the one state
;;   left rules out what comes. From 3 on, longer suffixes change nothing.

;; ## What it took
;;
;; - The evidence is suffix counts: plain counts, learned in one fold, combined by adding,
;;   so they can be gathered progressively from unbounded data.
;; - The states come out of the readout, by testing which suffixes predict alike. Nothing
;;   said there were two.
;; - What it needs instead: suffixes long enough to see where a state is revealed
;;   (`l-max`), enough data for the test, and a significance α that trades false splits
;;   against false merges.
;;
;; The next notebook assumes the number of states instead, and fits them by Baum–Welch.
