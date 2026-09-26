(ns robertluo.markov.gen
  "Generators shared by the tests of learners that learn from walks."
  (:require [clojure.test.check.generators :as gen]))

(def walk
  "A walk over a few states, possibly empty or a single state."
  (gen/vector (gen/elements [:a :b :c :d])))

(def prior
  (gen/let [states (gen/set (gen/elements [:a :b :c :d :e]))
            alpha (gen/elements [0 1/2 1 3])]
    {:states states :alpha alpha}))

(def walk-split
  "[whole first second]: a walk cut at one of its states, which both parts keep, so that
   no transition is lost between them."
  (gen/let [whole (gen/not-empty walk)
            i (gen/choose 0 (dec (count whole)))]
    [whole (subvec whole 0 (inc i)) (subvec whole i)]))
