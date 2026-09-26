# markov

Implementations of Markov chains, starting from the simplest form. See README.md.

## Commands

| | |
|---|---|
| test | `clojure -M:dev:test` (kaocha lives in `:dev`; `-M:test` alone has no runner) — also `devenv test` |
| lint | `clojure -M:lint --lint src test dev notebook` |
| REPL | `clojure -M:dev:nrepl`, then `(robertluo.markov.instrument/instrument!)` after loading a namespace |
| notebook REPL | `clojure -M:dev:notebook:nrepl`, then `(scicloj.clay.v2.api/make! {:source-path "notebook/robertluo/markov/chain_notebook.clj"})` |
| render notebooks | automatic: `devenv up` runs `robertluo.markov.notebooks`, which renders every notebook to `target/notebook/` and again on each change under `src/` or `notebook/` (reloading changed source namespaces first). Once: `clojure -M:dev:notebook -m robertluo.markov.notebooks --once` (exits 1 on failure). From a notebook REPL: `(robertluo.markov.notebooks/watch!)`. Config in `clay.edn` |

## Conventions

- **Every public function carries `:malli/schema`** (`[:=> [:cat ...] ret]`).
  `dev/robertluo/markov/instrument.clj` collects every `robertluo.markov.*` namespace and
  instruments it; kaocha runs that as a `post-load` hook (tests.edn), so tests always run
  against guarded functions.
  - malli checks a `:=>` guard only *after* the body has run, so a constraint across arguments
    cannot stop a bad call. Shape the arguments so each constraint is a schema on one of them.
  - A `def` alias of a function bypasses instrumentation (it captures the raw fn). Delegate with `defn`.
  - Private helpers (`defn-`) are not collected and need no schema.
- **Pure, non-trivial functions get property-based tests** with test.check (`defspec`). Prefer an
  invariant independent of the implementation over restating it. Example-based `deftest` is for
  the schema refusals and for concrete scenarios.
- Randomness is an argument (a uniform draw in [0, 1)), never called inside a function, so
  everything stays pure and testable.
- `io.github.robertluo/state-graph` (git dep) is on the classpath; use it only when a problem
  needs a graph.
- Tests: `test/robertluo/markov/<ns>_test.clj`, one per source namespace.

## Notebooks

[Clay](https://scicloj.github.io/clay/) notebooks live in
`notebook/robertluo/markov/<feature>_notebook.clj` (ns `robertluo.markov.<feature>-notebook`).
A notebook is both:

- **a show room** for a feature already in `src`: it demonstrates the feature on a concrete
  example, with kindly visualisations (`scicloj.kindly.v4.kind`: vega-lite, mermaid, tables).
- **a lab** for a new feature: trial it first in a new notebook. Once its shape is settled,
  migrate the general part into `src/robertluo/markov/<feature>.clj` with tests, and leave the
  notebook requiring it, as the feature's example and application.

In a notebook:

- Call `(instrument/instrument!)` near the top, so the page runs against guarded functions.
  The notebook's own ns is `robertluo.markov.*`, so it is collected too: a candidate function
  for `src` is a public `defn` with `:malli/schema` from its first trial (call `instrument!`
  again after defining it); presentation helpers are `defn-`.
- Draws come from a seeded `java.util.Random`, so a page renders the same every time.
- Rendered pages are build output (`target/notebook/`); check a rendered page, not just that it
  renders — Clay does not run the charts' JavaScript.
