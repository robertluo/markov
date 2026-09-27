;; # The simplest Markov chain
;;
;; *Part 1 of 6 · next: [learning a chain from observations](robertluo.markov.estimate_notebook.html)*
;;
;; A show room for `robertluo.markov.chain`: finitely many states, discrete time, and
;; transition probabilities that do not change over time.
;;
;; ## Background
;;
;; Many things move from one situation to another in steps: the weather from day to day,
;; a board-game piece from square to square, a customer from one page of a website to the
;; next. A **Markov chain** is the simplest model of such movement. It has a set of
;; **states** (sunny, cloudy, rainy), and for each state, the probabilities of each state
;; that can come next. Those probabilities are all it knows.
;;
;; What makes it *Markov* is one assumption, the **Markov property**: what happens next
;; depends only on where you are now, not on how you got there. If today is rainy, the
;; chance of rain tomorrow is the same whether the last week was all sun or all rain. The
;; chain has no memory beyond the present state. That sounds restrictive, and it is: later
;; pages ask what to do when a process seems to remember more. But it is also what makes a
;; chain simple enough to reason about, simulate, and learn.
;;
;; The pages of this tutorial build on one another:
;;
;; 1. **The simplest Markov chain** (this page): what a chain is, and how it runs.
;; 2. [Learning a chain](robertluo.markov.estimate_notebook.html) from what it did.
;; 3. [Learning over time](robertluo.markov.learner_notebook.html): chains that drift, and
;;    chains in continuous time.
;; 4. [Hidden states](robertluo.markov.even_notebook.html): when what you see is not the
;;    state.
;; 5. [Learning hidden states by CSSR](robertluo.markov.cssr_notebook.html).
;; 6. [Learning hidden states by Baum–Welch](robertluo.markov.hmm_notebook.html).
;;
;; Each page shows its code. You can read the prose alone and treat the code and its
;; results as the evidence.

(ns robertluo.markov.chain-notebook
  (:require [malli.core :as m]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.view :as view]))

;; ## A chain
;;
;; A chain maps each state to its **row**, and a row maps the next states to their
;; probabilities. A row's probabilities add up to 1: something always happens next, even
;; if it is staying put. Tomorrow's weather, knowing only today's:

(def weather
  {:sunny  {:sunny 0.7 :cloudy 0.2 :rainy 0.1}
   :cloudy {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :rainy  {:sunny 0.2 :cloudy 0.4 :rainy 0.4}})

;; Read the first row as: after a sunny day, the next is sunny 70% of the time, cloudy
;; 20%, rainy 10%. The code checks that this is a well-formed chain (every row sums to 1,
;; and every state a row mentions has a row of its own):

(m/validate chain/Chain weather)

;; The same chain as a diagram, one arrow per possible move, labelled with its
;; probability:

(view/diagram weather)

;; Each state keeps one colour in every chart below.

(def ^:private palette
  [[:sunny "#f2b705"] [:cloudy "#9aa5b1"] [:rainy "#2f6db5"]])

;; And as a **transition matrix**, the usual way to write a chain down: one row per state
;; today, one column per state tomorrow.

(view/matrix weather)

;; ## One step
;;
;; To run a chain, we need a way to pick the next state with the right probabilities.
;; Take a random number u, evenly spread between 0 and 1 (a **uniform draw**). Lay the
;; row's probabilities end to end along [0, 1): sunny takes the first 0.7, cloudy the next
;; 0.2, rainy the last 0.1. Wherever u falls picks the next state; since each state's
;; stretch is as long as its probability, each is picked exactly that often.
;; `next-state` does this. The ticks are the draws below.

(def some-draws [0.05 0.5 0.75 0.95])

(view/row-intervals palette (:sunny weather) some-draws)

(for [u some-draws]
  [u '-> (chain/next-state (:sunny weather) u)])

;; ## A walk
;;
;; A **walk** is a run of the chain: a start, then one step per draw. The code never makes
;; random numbers itself; it is handed its draws. That keeps every function *pure* (same
;; input, same output), which makes it easy to test. The draws here come from a random
;; generator started at a fixed **seed**, so this page shows the same month every time.

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

;; Those shares are the chain's **stationary distribution**: the long-run fraction of time
;; spent in each state. For a chain like this one, where every state can reach every other
;; and the chain does not cycle on a fixed schedule, the stationary distribution exists,
;; is unique, and the walk forgets its start. That is why a chain can describe a climate
;; and not just a day. `robertluo.markov.chain` does not compute it yet; it is a candidate
;; for a later page.

;; ## Guarded
;;
;; Every function in this project declares the shape of what it takes and gives back (a
;; **schema**), and checks it when run. A call outside the schemas is refused, with a
;; message saying why, for instance a row that does not sum to one:

(view/refusal #(chain/next-state {:sunny 0.5} 0.3))
