(ns robertluo.markov.laws
  "The laws every robertluo.markov.learner meets, as test.check properties, for each
   learner's test to state with its own generators. Numbers compare to within rounding:
   fading and time are floating point."
  (:require [clojure.test.check.properties :as prop]
            [robertluo.markov.learner :as learner]))

(defn approx=
  "Whether `a` and `b` agree, numbers to within rounding, maps key by key."
  [a b]
  (cond
    (and (map? a) (map? b)) (and (= (set (keys a)) (set (keys b)))
                                 (every? #(approx= (get a %) (get b %)) (keys a)))
    (and (number? a) (number? b)) (< (abs (- a b)) 1e-9)
    :else (= a b)))

(defn identity-law
  "Combining with the learner's empty, on either side, changes nothing."
  [learner gen-known]
  (prop/for-all [known gen-known]
                (and (approx= known (learner/combine learner (:empty learner) known))
                     (approx= known (learner/combine learner known (:empty learner))))))

(defn associativity-law
  [learner gen-known]
  (prop/for-all [a gen-known
                 b gen-known
                 c gen-known]
                (approx= (learner/combine learner (learner/combine learner a b) c)
                         (learner/combine learner a (learner/combine learner b c)))))

(defn parts-law
  "Learning the whole of some observations knows the same as learning its first part, and
   combining that with what the second part teaches alone. `gen-split` makes
   [whole first second], split however the learner's observations split."
  [learner gen-known gen-split]
  (prop/for-all [known gen-known
                 [whole a b] gen-split]
                (approx= (learner/learn learner known whole)
                         (learner/combine learner
                                          (learner/learn learner known a)
                                          (learner/learn learner (:empty learner) b)))))
