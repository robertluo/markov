(ns robertluo.markov.instrument
  "Turns :malli/schema metadata into checks. A schema that is only written down is a
   comment; this makes every guarded function in robertluo.markov.* throw on a bad argument
   or a bad return, in the REPL and in the test run."
  (:require [clojure.string :as str]
            [malli.dev.pretty :as pretty]
            [malli.instrument :as mi]))

(defn- markov-namespaces []
  (->> (all-ns)
       (map ns-name)
       (filter #(str/starts-with? (name %) "robertluo.markov."))
       (remove #{'robertluo.markov.instrument 'robertluo.markov.notebooks})))

(defn instrument!
  "Collects every loaded robertluo.markov.* namespace and instruments it. Answers the
   instrumented function names, so a count can be compared against what you expect."
  []
  (mi/clj-collect! {:ns (markov-namespaces)})
  (mi/instrument! {:report (pretty/thrower)}))

(defn hook
  "kaocha :post-load hook, see tests.edn."
  [test-plan]
  (instrument!)
  test-plan)
