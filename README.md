# Markov Chain

This project is an implementation of various Markov Chains. Starting from the simplest form.

We use the following libraries:

 - Malli. All functions should be guarded with its checking.
 - test.check. All pure functions except trivial ones should use property based test.
 - github:robertluo/state-graph. If we find a graph is needed for the problem domain.
 - Clay. Notebooks under `notebook/` are a show room for each feature, and a lab where a new
   feature is trialled before its settled shape moves into `src`. `devenv up` keeps every
   notebook rendered: it renders them all on start and again whenever a file under `src/` or
   `notebook/` changes. Open `target/notebook/robertluo.markov.chain_notebook.html`. Without
   devenv, `clojure -M:dev:notebook -m robertluo.markov.notebooks` does the same, and
   `--once` renders once and exits.
