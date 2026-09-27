;; # Learning the states: CSSR
;;
;; *Part 5 of 6 · previous: [hidden states](robertluo.markov.even_notebook.html)
;; · next: [learning hidden states by Baum–Welch](robertluo.markov.hmm_notebook.html)*
;;
;; A show room for `robertluo.markov.cssr`. The even notebook tracked the even process
;; with the true machine: it was handed the states. Here they are learned from symbols
;; alone, by building them from their definition: two pasts are in the same state when
;; they predict the same future. This is CSSR, Causal-State Splitting Reconstruction
;; (Shalizi & Klinkner, 2004), in a simplified form, as a learner: the evidence is
;; gathered in one fold, and the states are found by the readout.
;;
;; ## Background
;;
;; **What a state is, by definition.** The previous page argued that the right state is
;; not a window of recent symbols but whatever in the past matters for the future. Take
;; that literally: group together all pasts that give the same prediction of what comes
;; next. Each group is a **causal state**, and the causal states with their transitions
;; form the process's **ε-machine**. It is the smallest model that predicts as well as
;; possible, and it is unifilar: from a state, the next symbol settles the next state. The
;; even process's ε-machine is exactly its two states, A and B.
;;
;; **From definition to algorithm.** We cannot look at infinitely long pasts, so CSSR
;; looks at **suffixes**, the last few symbols, up to a length `l-max`. It counts what
;; followed each suffix, then groups suffixes whose counts look like the same prediction.
;; A longer suffix is a more detailed past; if it predicts like its shorter parent, it adds
;; nothing and joins the parent's state. If it predicts differently, the state must
;; **split**. States appear only when the evidence demands them, so the number of states
;; is found, not assumed.
;;
;; **Telling predictions apart: a statistical test.** Counts are noisy. After a 0, the
;; even process emits 1 half the time, but 1,000 observations will not show exactly 500.
;; Are 48% and 53% "the same prediction"? A **chi-square test** answers: if both sets of
;; counts came from one and the same distribution, how likely is a difference this large?
;; That probability is the **p-value**. When it falls below a chosen **significance level
;; α**, we call the predictions different. The choice of α is a trade-off between two
;; mistakes: a large α splits states that are really one (a false split); a small α
;; merges states that are really two (a false merge).
;;
;; **Transient states.** Some pasts have not yet revealed where the process is: in the
;; even process, a run of 1s longer than the suffix cannot tell where the pairs began.
;; Their predictions are mixtures. They describe where a process might be *before* its
;; state is known, not a state it returns to, and CSSR drops them.

(ns robertluo.markov.cssr-notebook
  (:require [clojure.string :as str]
            [robertluo.markov.cssr :as cssr]
            [robertluo.markov.hidden :as hidden]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]))

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
;; These counts are the **statistic** of the learner `(cssr/cssr params)`, in the shape
;; of [part 3](robertluo.markov.learner_notebook.html): the **events** are [suffix next]
;; pairs (`cssr/suffix-events`), the **step** counts one, and they **combine** by adding.
;; So the evidence can be gathered progressively, and all the hard work happens in the
;; **readout**, whenever a model is wanted.

(def ^:private params {:l-max 3 :alpha 0.001})

(def ^:private evidence (learner/learn (cssr/cssr params) {} train))

(defn- p-one [counts]
  (let [n (reduce + 0 (vals counts))]
    (double (/ (get counts :1 0) n))))

(defn- suffix-name [suffix]
  (if (seq suffix) (apply str (map name suffix)) "λ"))

(kind/table
 {:column-names ["suffix" "then 0" "then 1" "P(1 next)"]
  :row-vectors (for [[suffix counts] (sort-by (comp (juxt count identity) key) evidence)
                     :when (<= (count suffix) 2)]
                 [(suffix-name suffix) (get counts :0 0) (get counts :1 0)
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
;;
;; ## Transient states
;;
;; Some states hold only suffixes that have not seen enough: the all-1s suffixes, whose
;; prediction is a mixture, because they do not reach back to the last 0. Such a state is
;; where the machine is before the symbols have revealed its state, never where it
;; returns to. So a state is kept only if some suffix in it, extended further into the
;; past, stays in it: once a past has revealed its state, more past does not change it.
;; (CSSR removes transient states from the transition graph; this criterion is a
;; simpler stand-in.)

(def ^:private learned (cssr/reconstruct params evidence))

(defn- pooled [counts suffixes]
  (transduce (map counts) (completing #(merge-with + %1 %2)) {} suffixes))

(kind/table
 {:column-names ["state" "suffixes" "P(1 next)" "kept?"]
  :row-vectors (for [[i s] (map-indexed vector (:grown learned))]
                 [i (str/join " " (sort (map suffix-name s)))
                  (format "%.3f" (p-one (pooled evidence s)))
                  (get (:recurrent learned) i)])})

;; ## Making it a machine
;;
;; A machine needs to know where each state goes on each symbol. For a suffix x of the
;; longest length and a symbol b, the next suffix is x with b added and its oldest symbol
;; dropped. A state must send all its suffixes to the same state on b; where they
;; disagree, the state is split until they agree. A next suffix that lands in a transient
;; state is unresolved (the window lost the 0 that would settle it), and is left out.

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

(let [l     (cssr/cssr params)
      l-max (:l-max params)
      [a b] [(subvec train 0 5000) (subvec train (- 5000 l-max))]]
  (= (learner/learn l {} train)
     (learner/combine l (learner/learn l {} a) (learner/learn l {} b))))

;; ## How much data, how long a suffix
;;
;; The page's machine came from 10,000 symbols with suffixes up to 3 long. How does it
;; fare with less, or with other settings?

(defn- trial [n params]
  (let [l       (cssr/cssr params)
        machine (learner/readout l (learner/learn l {} (subvec train 0 n)))]
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
;; - Its limits show on harder processes. On a 3-phase cycle of 0s and 1s, whose phase
;;   takes long to reveal, this simplified version builds too many states; where a symbol
;;   marks the phase, it recovers the cycle exactly (see the tests).
;;
;; The next notebook assumes the number of states instead, and fits them by Baum–Welch.
