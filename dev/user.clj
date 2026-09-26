(ns user
  (:require [robertluo.markov.instrument :as instrument]))

(comment
  ;; after (re)loading a robertluo.markov.* namespace
  (instrument/instrument!)

  ;; with the :notebook alias: render a notebook and open it in the browser
  ((requiring-resolve 'scicloj.clay.v2.api/make!)
   {:source-path "notebook/robertluo/markov/chain_notebook.clj"}))
