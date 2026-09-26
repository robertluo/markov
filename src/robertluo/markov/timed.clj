(ns robertluo.markov.timed
  "Chains in continuous time. Such a chain is its rates: from each state, how often per
   hour it moves to each other state. The time spent in a state is exponential, with the
   total rate out of it; where it goes then is picked in proportion to the rates.

   A trajectory is sojourns, each a state and how long it lasted, built lazily from
   pairs of draws by `sojourns` and bounded by a clock, `for-hours`.

   `timed` is the robertluo.markov.learner: for a rate, the evidence is how many times a
   state was left for another, and how long was spent in it in all, so what is known is
   time held per state and jumps, and rate = jumps / time held. Time adds and jumps add,
   so what was learned in windows of time combines by adding, in either order, even where
   a window's edge falls inside a sojourn."
  (:require [robertluo.markov.chain :as chain]))

(def Rates
  "Transitions per hour from each state to each other state. A state with no rates out
   of it is never left."
  [:map-of chain/State [:map-of chain/State [:and number? pos?]]])

(def Sojourn
  "A state, and how many hours it lasted."
  [:tuple chain/State [:double {:min 0.0}]])

(defn sojourns
  "A transducer from pairs of draws to the sojourns they make from `start` under `rates`:
   one draw for how long, one for where next. Each use keeps its own current state. A
   state never left has no sojourn to end, which next-state refuses."
  {:malli/schema [:=> [:cat Rates chain/State] ifn?]}
  [rates start]
  (fn [rf]
    (let [state (volatile! start)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc [u1 u2]]
         (let [s     @state
               row   (get rates s)
               total (reduce + 0 (vals row))]
           (vreset! state (chain/next-state (update-vals row #(double (/ % total))) u2))
           (rf acc [s (/ (- (Math/log (- 1.0 u1))) total)])))))))

(defn for-hours
  "A transducer passing sojourns until `hours` have passed, cutting the last one short
   and stopping there."
  {:malli/schema [:=> [:cat [:and number? pos?]] ifn?]}
  [hours]
  (fn [rf]
    (let [clock (volatile! 0.0)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc [s held]]
         (let [left (- hours @clock)]
           (if (< held left)
             (do (vswap! clock + held)
                 (rf acc [s held]))
             (ensure-reduced (rf acc [s (double left)])))))))))

(def Event
  "A sojourn, and the state it jumped to; nil when watching ended first."
  [:tuple chain/State [:double {:min 0.0}] [:maybe chain/State]])

(defn sojourn-events
  "A transducer from sojourns to events: each sojourn with the next one's state, and the
   last, on completion, with nil, since watching ended before it did. Its time still
   counts: the state lasted at least that long."
  {:malli/schema [:=> [:cat ifn?] ifn?]}
  [rf]
  (let [prev (volatile! nil)]
    (fn
      ([] (rf))
      ([acc] (rf (if-let [[s held] @prev]
                   (unreduced (rf acc [s held nil]))
                   acc)))
      ([acc [s :as sojourn]]
       (let [p @prev]
         (vreset! prev sojourn)
         (if p (rf acc (conj p s)) acc))))))

(def Timed
  "Hours held per state, and jumps from each state to each other."
  [:map
   [:held [:map-of chain/State [:double {:min 0.0}]]]
   [:jumps [:map-of chain/State [:map-of chain/State pos-int?]]]])

(defn step
  "What `known` becomes on one more event: its time added, its jump counted."
  {:malli/schema [:=> [:cat Timed Event] Timed]}
  [known [s held next]]
  (cond-> (update-in known [:held s] (fnil + 0.0) held)
    next (-> (update-in [:held next] (fnil + 0.0) 0.0)
             (update-in [:jumps s next] (fnil inc 0)))))

(defn combine
  "What was learned in two windows, as one: times added, jumps added."
  {:malli/schema [:=> [:cat Timed Timed] Timed]}
  [a b]
  {:held  (merge-with + (:held a) (:held b))
   :jumps (merge-with #(merge-with + %1 %2) (:jumps a) (:jumps b))})

(defn rates
  "The rates most likely to have made what `known` holds: jumps over time held. A state
   never seen left has no rates out of it."
  {:malli/schema [:=> [:cat Timed] Rates]}
  [{:keys [held jumps]}]
  (into {}
        (map (fn [[s h]]
               [s (if (pos? h)
                    (update-vals (get jumps s {}) #(double (/ % h)))
                    {})]))
        held))

(def timed
  "The learner of rates: sojourns in, rates out."
  {:events    #'sojourn-events
   :statistic Timed
   :empty     {:held {} :jumps {}}
   :step      #'step
   :combine   #'combine
   :readout   #'rates})
