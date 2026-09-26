;; # Learning a chain from observations
;;
;; A show room for `robertluo.markov.estimate`. The chain notebook went from a chain to
;; its walks; this goes back, from walks alone to a chain that explains them: a model of
;; the process that made them.

(ns robertluo.markov.estimate-notebook
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.instrument :as instrument]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]))

;; Everything below runs against guarded functions, as the tests do.

(instrument/instrument!)

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
;; Everything a Markov chain can learn from a walk is in its steps: how often each state
;; was followed by each other state. The order of the steps, once counted, adds nothing,
;; since the next state depends on today only.

(def observed (estimate/counts months))

(defn- count-table [counts order]
  (kind/table
   {:column-names (cons "today \\ tomorrow" (map name order))
    :row-vectors (for [from order]
                   (cons (name from) (map #(get-in counts [from %] 0) order)))}))

(count-table observed (map first palette))

;; ## Estimating
;;
;; The chain most likely to have made these counts divides each row by its total: the
;; maximum-likelihood estimate. A prior may add a pseudo-count α to every transition,
;; and may name states known to exist but not observed; the empty prior adds nothing.

(def learned (estimate/estimate {} observed))

(view/heatmaps [["hidden" weather] ["learned from 3 months" learned]])

(view/diagram learned)

;; ## How good is it?
;;
;; Two chains are as far apart as their furthest pair of rows, and two rows as far as
;; the most probability they give differently to any set of next states (total
;; variation): 0 when they agree, 1 when they share nothing.

(estimate/distance weather learned)

;; More observations make a better model. The error of an estimate from one walk of n
;; steps, averaged over 20 walks for each n, shrinks roughly like 1/√n: a hundred times
;; the data for a tenth of the error.

(def ^:private sizes [10 30 100 300 1000 3000 10000])

(defn- mean-error [prior n]
  (let [errors (for [seed (range 20)
                     :let [walk (chain/walk weather :sunny (view/draws (+ (* 1000 n) seed) n))]]
                 (estimate/distance weather
                                    (estimate/estimate prior (estimate/counts [walk]))))]
    (/ (reduce + errors) (count errors))))

(view/error-curve
 (for [[label prior] [["α = 0" {}] ["α = 1" {:alpha 1}]]]
   [label (for [n sizes] [n (mean-error prior n)])]))

;; ## Too few observations
;;
;; Ten days are not enough to see every transition, and plain counting calls what it
;; has not seen impossible: a zero in the matrix, a missing arrow in the diagram. A
;; pseudo-count keeps every transition possible, at the price of pulling each row
;; towards uniform. In the chart above, smoothing helps while data is scarce, and its
;; pull fades as the counts outgrow α.
;;
;; These ten days end on the only rainy day, so nothing was seen to follow rain: with
;; α = 0 the model has nothing to go on and rain stays put; with α = 1 its row is
;; uniform.

(def ten-days (estimate/counts [(chain/walk weather :sunny (view/draws 5 10))]))

(view/heatmaps [["hidden" weather]
                ["α = 0" (estimate/estimate {} ten-days)]
                ["α = 1" (estimate/estimate {:alpha 1} ten-days)]])

(view/diagram (estimate/estimate {} ten-days))

;; A prior can also name a state never observed. With α it gets a uniform row, since
;; there is nothing else to go on; without it, it stays put:

(estimate/estimate {:states #{:snowy} :alpha 1} ten-days)

;; ## Guarded
;;
;; No observations make no chain: the estimate has no state, and the guard on its return
;; refuses it:

(view/refusal #(estimate/estimate {} (estimate/counts [])))
