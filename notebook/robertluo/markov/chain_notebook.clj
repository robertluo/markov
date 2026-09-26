;; # The simplest Markov chain
;;
;; A show room for `robertluo.markov.chain`: finitely many states, discrete time, and
;; transition probabilities that do not change over time.

(ns robertluo.markov.chain-notebook
  (:require [malli.core :as m]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.view :as view]))

;; ## A chain
;;
;; A chain maps each state to its row, and a row maps the next states to their
;; probabilities. Tomorrow's weather, knowing only today's:

(def weather
  {:sunny  {:sunny 0.7 :cloudy 0.2 :rainy 0.1}
   :cloudy {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :rainy  {:sunny 0.2 :cloudy 0.4 :rainy 0.4}})

(m/validate chain/Chain weather)

(view/diagram weather)

;; Each state keeps one colour in every chart below.

(def ^:private palette
  [[:sunny "#f2b705"] [:cloudy "#9aa5b1"] [:rainy "#2f6db5"]])

;; The same chain as a transition matrix, one row per state today:

(view/matrix weather)

;; ## One step
;;
;; `next-state` lays a row out as consecutive intervals of [0, 1), one per next state,
;; and a uniform draw picks the interval it falls in. The ticks are the draws below.

(def some-draws [0.05 0.5 0.75 0.95])

(view/row-intervals palette (:sunny weather) some-draws)

(for [u some-draws]
  [u '-> (chain/next-state (:sunny weather) u)])

;; ## A walk
;;
;; Randomness is an argument: a walk is told its draws. A seeded generator makes the
;; draws, so this page renders the same every time.

(def month (chain/walk weather :sunny (view/draws 42 30)))

(view/timeline palette month)

;; ## The long run
;;
;; Over many steps, the share of days spent in each state settles, and it settles to
;; the same shares whichever state the walk starts from. Each walk gets draws of its
;; own: two walks on the same draws move together once they meet, and would agree for
;; that reason alone.

(view/share-bars palette
                 (for [[start seed] [[:sunny 7] [:rainy 8]]]
                   [(str "from " (name start))
                    (chain/walk weather start (view/draws seed 10000))]))

;; Those shares are the chain's stationary distribution. `robertluo.markov.chain` does not
;; compute it yet; it is a candidate for the next lab notebook.

;; ## Guarded
;;
;; A call outside the schemas is refused, for instance a row that does not sum to one:

(view/refusal #(chain/next-state {:sunny 0.5} 0.3))
