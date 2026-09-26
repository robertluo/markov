(ns robertluo.markov.learner
  "Learning, whatever is learned. A learner turns observations into events, and folds
   each event into a statistic, what is known; two statistics learned apart combine into
   one; and a readout turns what is known into a model.

   What is known is plain data, kept apart from the learner: it can be stored, and it is
   the prior for whatever is learned next. A learner meets three laws, which the tests
   check for every one:

   - `(:empty L)` is combine's identity: combining it with k, on either side, gives k.
   - combine is associative. It need not be commutative: order may matter.
   - learning in parts knows the same as learning at once: learning `a` then `b` equals
     combining what was learned from `a` with what was learned from `b` alone.

   A learner's functions are vars (`#'step`), or calls to them, never the functions
   themselves: a function captured before `instrument!` runs is never guarded.")

(def Learner
  [:map
   [:events    {:doc "a transducer from observations to events"} ifn?]
   [:statistic {:doc "the malli schema of what is known"} some?]
   [:empty     {:doc "knowing nothing: combine's identity"} any?]
   [:step      {:doc "(step known event), what known becomes on one more event"} ifn?]
   [:combine   {:doc "(combine earlier later), what was learned apart, as one"} ifn?]
   [:readout   {:doc "(readout known), the model what is known suggests"} ifn?]])

(defn learn
  "What `known` becomes on observing `observations` under `learner`. The observations are
   consumed as they are read, so they may be of any length, even unbounded if the
   learner's events bound them."
  {:malli/schema [:=> [:cat Learner any? [:fn seqable?]] any?]}
  [{:keys [events step]} known observations]
  (transduce events (completing step) known observations))

(defn combine
  "What was learned in `earlier` and then in `later`, as one statistic."
  {:malli/schema [:=> [:cat Learner any? any?] any?]}
  [learner earlier later]
  ((:combine learner) earlier later))

(defn readout
  "The model `known` suggests under `learner`."
  {:malli/schema [:=> [:cat Learner any?] any?]}
  [learner known]
  ((:readout learner) known))
