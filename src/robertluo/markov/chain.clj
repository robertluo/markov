(ns robertluo.markov.chain
  "The simplest Markov chain: finitely many states, discrete time, and transition
   probabilities that do not change over time.

   A chain is a map from each state to its row, and a row maps the next states to their
   probabilities. Every state a row names is itself a state of the chain, and each row
   sums to one.

   Randomness is an argument: a step is told a uniform draw u in [0, 1), so every
   function here is pure and a walk is a reduction over its draws.")

(def tolerance
  "How far a row's sum may drift from 1.0 through floating-point rounding."
  1e-9)

(def State :keyword)

(def Probability [:double {:min 0.0 :max 1.0}])

(def Draw
  "A uniform draw from [0, 1)."
  [:and :double [:>= 0.0] [:< 1.0]])

(defn- stochastic? [row]
  (< (abs (- 1.0 (reduce + (vals row)))) tolerance))

(def Row
  [:and
   [:map-of State Probability]
   [:fn {:error/message "row must sum to 1"} stochastic?]])

(defn- closed? [chain]
  (every? #(contains? chain %) (mapcat keys (vals chain))))

(def Chain
  [:and
   [:map-of {:min 1} State Row]
   [:fn {:error/message "every next state must be a state of the chain"} closed?]])

(defn next-state
  "The next state `row` picks for the draw `u`: the row is laid out as consecutive
   intervals of [0, 1), one per next state with positive probability, and u picks the
   interval it falls in. Rounding that leaves u past the last interval picks the last
   state.

   Takes the row and not the chain and a state, so that every constraint on the
   arguments is a schema on one of them: malli checks a function's guard only after the
   body has run."
  {:malli/schema [:=> [:cat Row Draw] State]}
  [row u]
  ;; A row has one entry per state, small by definition, so a loop over it is fine.
  (loop [[[s p] & more] (filter (comp pos? val) row)
         upper 0.0]
    (let [upper (+ upper p)]
      (if (or (< u upper) (empty? more))
        s
        (recur more upper)))))

(defn steps
  "A transducer from draws to the states they lead to, one per draw, walking from `start`
   (which it does not emit). Each use keeps its own current state. Over an unbounded
   stream of draws, `(sequence (steps chain start) draws)` walks lazily, and
   `(transduce (steps chain start) rf init draws)` consumes the draws as they come.

   The draws are not a schema argument here, as checking them would realise the stream;
   next-state guards each one. A `start` outside the chain has no row, which next-state
   refuses."
  {:malli/schema [:=> [:cat Chain State] fn?]}
  [chain start]
  (fn [rf]
    (let [state (volatile! start)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc u] (rf acc (vswap! state #(next-state (get chain %) u))))))))

(def Transition
  "One step of a walk as an event: the state before and the state after. The first state
   of a walk has no state before it, nil."
  [:tuple [:maybe State] State])

(defn transitions
  "A transducer from the states of a walk to its transitions, one per state: the first
   with nil before it, so that a walk's every state is seen, even a walk of one."
  {:malli/schema [:=> [:cat ifn?] ifn?]}
  [rf]
  (let [prev (volatile! nil)]
    (fn
      ([] (rf))
      ([acc] (rf acc))
      ([acc s] (let [p @prev]
                 (vreset! prev s)
                 (rf acc [p s]))))))

(defn walk
  "Every state visited from `start`, one step per draw: `start` first, then one state per
   element of `us`, a finite collection. For an unbounded stream of draws, use steps."
  {:malli/schema [:=> [:cat Chain State [:sequential Draw]] [:vector State]]}
  [chain start us]
  (into [start] (steps chain start) us))
