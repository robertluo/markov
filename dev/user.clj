(ns user
  (:require [markov.instrument :as instrument]))

(comment
  ;; after (re)loading a markov.* namespace
  (instrument/instrument!))
