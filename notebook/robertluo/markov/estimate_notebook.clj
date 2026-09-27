;; # Learning a chain from observations
;;
;; *Part 2 of 6 · previous: [the simplest Markov chain](robertluo.markov.chain_notebook.html)
;; · next: [learning over time](robertluo.markov.learner_notebook.html)*
;;
;; A show room for `robertluo.markov.estimate`. The chain notebook went from a chain to
;; its walks; this goes back, from walks alone to a chain that explains them: a model of
;; the process that made them, which grows better as observations keep coming.
;;
;; ## Background
;;
;; In practice nobody hands us the chain. We see what happened (a year of weather, a log
;; of clicks) and want the probabilities that best explain it. That is **estimation**:
;; guessing a model's numbers from data. Two ideas carry this page.
;;
;; **Maximum likelihood.** Any candidate chain gives the observed walk some probability:
;; multiply the probability of each step taken. That number is the candidate's
;; **likelihood**. The maximum-likelihood estimate is the candidate that makes the data
;; most probable. For a Markov chain it comes out as common sense: if a sunny day was
;; followed by a cloudy one 10 times out of 41, estimate P(cloudy | sunny) = 10/41. Count,
;; then divide.
;;
;; **Prior knowledge.** Counting alone is overconfident with little data. Never having
;; seen snow follow rain does not make it impossible. A **prior** encodes what we believe
;; before looking, here as **pseudo-counts**: pretend each transition was already seen α
;; times. With α = 1 this is Laplace's old "add one" rule. As real counts pile up, the
;; pretend ones matter less and less. (In Bayesian terms, the pseudo-counts are a
;; Dirichlet prior, and the result is the mean of the posterior.)
;;
;; A consequence that shapes the code: the counts are all the data can tell a Markov chain
;; (they are a **sufficient statistic**). Once counted, the walk can be thrown away, and
;; new observations just add to the counts. Learning is progressive.

(ns robertluo.markov.estimate-notebook
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]))

;; ## A hidden chain
;;
;; The page knows the process; the model will not. Tomorrow's weather, as in the chain
;; notebook:

(def weather
  {:sunny  {:sunny 0.7 :cloudy 0.2 :rainy 0.1}
   :cloudy {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :rainy  {:sunny 0.2 :cloudy 0.4 :rainy 0.4}})

(def ^:private palette
  [[:sunny "#f2b705"] [:cloudy "#9aa5b1"] [:rainy "#2f6db5"]])

;; ## Observations
;;
;; What the model gets: a few walks, say three months of weather, each observed from
;; its own first day.

(def months
  (for [[start seed] [[:sunny 1] [:rainy 2] [:cloudy 3]]]
    (chain/walk weather start (view/draws seed 29))))

(kind/fragment (map #(view/timeline palette %) months))

;; ## Counting
;;
;; What the model knows is counts: how often each state was followed by each other
;; state. That is everything a Markov chain can learn from a walk, since the next state
;; depends on today only. Knowing nothing is no counts; learning a walk adds its counts.
;;
;; (In the code, `learner/learn` folds a walk into what is known: it goes through the walk
;; once, updating the counts at each step. The next page explains the shape behind it.)

(def observed (reduce #(learner/learn estimate/counting %1 %2) {} months))

(defn- count-table [counts order]
  (kind/table
   {:column-names (cons "today \\ tomorrow" (map name order))
    :row-vectors (for [from order]
                   (cons (name from) (map #(get-in counts [from %] 0) order)))}))

(count-table observed (map first palette))

;; ## Estimating
;;
;; The chain most likely to have made these counts divides each row by its total: the
;; maximum-likelihood estimate.

(def learned (estimate/estimate observed))

(view/heatmaps [["hidden" weather] ["learned from 3 months" learned]])

(view/diagram learned)

;; ## How good is it?
;;
;; To score an estimate we need a distance between two chains. Compare one row at a time:
;; the **total variation distance** between two rows is the most probability they give
;; differently to any set of next states (half the sum of the differences). It is 0 when
;; the rows agree and 1 when they share nothing. Two chains are as far apart as their
;; furthest pair of rows.

(estimate/distance weather learned)

;; ## Prior knowledge
;;
;; Before any observation, the model may already know something: which states exist,
;; and a pseudo-count α for each transition between them, as if each had been seen α
;; times. A prior is counts too, so learning starts from it in the same way.

(def ^:private states (set (keys weather)))

(def smoothed (estimate/prior {:states states :alpha 1}))

(count-table smoothed (map first palette))

;; ## Learning as it goes
;;
;; What has been learned is the prior for whatever comes next, so the model can take a
;; year of weather a month at a time, and be as good at each month's end as it can be on
;; what it has seen. Each month starts on the last day of the one before, so that the
;; transition between them is counted.

(def year (chain/walk weather :sunny (view/draws 11 360)))

(def by-month (partition 31 30 year))

(def ^:private progress
  (for [[label knowledge] [["no prior" {}] ["α = 1" smoothed]]]
    [label (map-indexed (fn [i known] [(* 30 (inc i))
                                       (estimate/distance weather (estimate/estimate known))])
                        (rest (reductions #(learner/learn estimate/counting %1 %2) knowledge by-month)))]))

(view/error-curve progress)

;; A month at a time or all at once, the model ends knowing the same:

(= (reduce #(learner/learn estimate/counting %1 %2) smoothed by-month)
   (learner/learn estimate/counting smoothed year))

;; Observations need not fit in memory either. `learn` reads a walk as it goes, so an
;; unbounded stream of draws, walked lazily by `chain/steps` and bounded only where it
;; is consumed, is never held whole. (*Lazily* means each step is produced only when
;; something asks for it, so a stream can be endless without filling memory.)

(def ^:private long-run
  (eduction (chain/steps weather :sunny) (take 100000) (view/draw-stream 9)))

(estimate/distance weather (estimate/estimate (learner/learn estimate/counting smoothed long-run)))

;; ## More observations, better model
;;
;; The error of an estimate from one walk of n steps, averaged over 20 walks for each n,
;; shrinks roughly like 1/√n: a hundred times the data for a tenth of the error. That rate
;; is the usual one for estimating a proportion from samples (a poll's margin of error
;; shrinks the same way), since each row of the chain is a set of proportions.

(def ^:private sizes [10 30 100 300 1000 3000 10000])

(defn- mean-error [knowledge n]
  (let [errors (for [seed (range 20)
                     :let [walk (chain/walk weather :sunny (view/draws (+ (* 1000 n) seed) n))]]
                 (estimate/distance weather (estimate/estimate (learner/learn estimate/counting knowledge walk))))]
    (/ (reduce + errors) (count errors))))

(view/error-curve
 (for [[label knowledge] [["no prior" {}] ["α = 1" smoothed]]]
   [label (for [n sizes] [n (mean-error knowledge n)])]))

;; ## Too few observations
;;
;; Ten days are not enough to see every transition, and plain counting calls what it
;; has not seen impossible: a zero in the matrix, a missing arrow in the diagram. The
;; prior keeps every transition possible, at the price of pulling each row towards
;; uniform. In the charts above, it helps while data is scarce, and its pull fades as the
;; counts outgrow α.
;;
;; These ten days end on the only rainy day, so nothing was seen to follow rain: with no
;; prior the model has nothing to go on and rain stays put; with α = 1 its row is
;; uniform.

(def ten-days (chain/walk weather :sunny (view/draws 5 10)))

(view/heatmaps [["hidden" weather]
                ["no prior" (estimate/estimate (learner/learn estimate/counting {} ten-days))]
                ["α = 1" (estimate/estimate (learner/learn estimate/counting smoothed ten-days))]])

(view/diagram (estimate/estimate (learner/learn estimate/counting {} ten-days)))

;; A prior can also name a state never observed. Its row is the prior alone, uniform:

(estimate/estimate
 (learner/learn estimate/counting (estimate/prior {:states (conj states :snowy) :alpha 1}) ten-days))

;; ## Guarded
;;
;; Knowing nothing makes no chain: the estimate has no state, and the guard on its
;; return refuses it:

(view/refusal #(estimate/estimate {}))
