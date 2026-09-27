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

## The notebooks as a tutorial

The notebooks double as a tutorial for readers new to Markov chains. Each page opens with
the background it needs, and links to the previous and next. Read them in order:

1. `chain_notebook`: what a Markov chain is, and how it runs.
2. `estimate_notebook`: learning a chain from what it did (maximum likelihood, priors).
3. `learner_notebook`: learning over time (chains that drift, chains in continuous time).
4. `even_notebook`: hidden states, and why a longer window of the past is no substitute.
5. `cssr_notebook`: learning hidden states by building them from the data (CSSR).
6. `hmm_notebook`: learning hidden states by fitting an assumed number (Baum–Welch).
