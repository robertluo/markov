# Markov Chain

This project is an implementation of various Markov Chains. Starting from the simplest form.

We use the following libraries:

 - Malli. All functions should be guarded with its checking.
 - test.check. All pure functions except trivial ones should use property based test.
 - github:robertluo/state-graph. If we find a graph is needed for the problem domain.
 - Clay. Notebooks under `notebook/` are a show room for each feature, and a lab where a new
   feature is trialled before its settled shape moves into `src`. Render one with
   `clojure -M:dev:notebook -m scicloj.clay.v2.main -r notebook/markov/chain_notebook.clj`
   and open `target/notebook/markov.chain_notebook.html`.
