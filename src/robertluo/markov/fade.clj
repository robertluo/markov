(ns robertluo.markov.fade
  "Learning a chain that drifts, by counts that fade: every weight fades by a factor λ per
   step before the step's transition is counted, so a transition seen n steps ago weighs
   λⁿ and the model remembers roughly the last 1/(1 − λ) steps. With λ = 1 nothing fades,
   and this is counting again.

   `(fading λ)` is the robertluo.markov.learner. What was learned first has faded further
   by the time what was learned later ends, so combine needs to know how many steps the
   later part spans, and order matters."
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.learner :as learner]))

(def Lambda
  "The factor a weight fades by per step, in (0, 1]."
  [:and number? [:> 0] [:<= 1]])

(def Faded
  "Faded counts, and how many steps they span."
  [:map
   [:weights estimate/Counts]
   [:elapsed nat-int?]])

(defn faded
  "What is known before any observation, from `counts`: a prior, or {} for nothing."
  {:malli/schema [:=> [:cat estimate/Counts] Faded]}
  [counts]
  {:weights counts :elapsed 0})

(defn- scale [weights f]
  (update-vals weights (fn [row] (update-vals row #(* f %)))))

(defn step
  "What `known` becomes on one more transition: every weight faded by `lambda`, then the
   transition counted. The first state of a walk is no step, so fades nothing."
  {:malli/schema [:=> [:cat Lambda Faded chain/Transition] Faded]}
  [lambda {:keys [weights elapsed] :as known} [from to]]
  (if from
    {:weights (-> (scale weights lambda)
                  (update to #(or % {}))
                  (update-in [from to] (fnil + 0) 1))
     :elapsed (inc elapsed)}
    (update known :weights update to #(or % {}))))

(defn combine
  "What was learned in `earlier` and then in `later`, as one: the earlier faded for each
   of the later's steps, then added."
  {:malli/schema [:=> [:cat Lambda Faded Faded] Faded]}
  [lambda earlier later]
  {:weights (estimate/combine (scale (:weights earlier) (Math/pow lambda (:elapsed later)))
                              (:weights later))
   :elapsed (+ (:elapsed earlier) (:elapsed later))})

(defn estimate
  "The chain the faded counts `known` suggest."
  {:malli/schema [:=> [:cat Faded] chain/Chain]}
  [known]
  (estimate/estimate (:weights known)))

(defn fading
  "The learner whose counts fade by `lambda` per step: walks in, a chain out."
  {:malli/schema [:=> [:cat Lambda] learner/Learner]}
  [lambda]
  {:events    #'chain/transitions
   :statistic Faded
   :empty     (faded {})
   :step      (fn [known event] (step lambda known event))
   :combine   (fn [earlier later] (combine lambda earlier later))
   :readout   #'estimate})
