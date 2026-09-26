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
- **Data of any size goes through transducers.** Where data may be large or unbounded (walks,
  draws, observations), process it with `transduce`/`into`/`sequence`/`eduction` over a
  transducer, and build it from its source lazily (e.g. `chain/steps`). A caller bounds it
  where it is consumed (`(take n)`). Keep `loop`/`recur` and plain lazy-seq chains (`for`,
  `partition`, `reductions`) to data that is small by definition, such as a row or a chain's
  states, and say so in a comment where it is not obvious.
  - Don't give such data a `[:sequential ...]` schema: checking it realises it, and never
    returns on an unbounded one. Check that it is `seqable?` and let the return schema catch
    bad elements.
  - The instrumentation wrapper keeps its arguments until the return is checked, so a guarded
    call keeps a lazy seq's head alive. Pass unbounded data as an eduction over a source that
    caches nothing (`(range)`, `view/draw-stream`); holding one keeps no elements alive.
- **Learning goes through `robertluo.markov.learner`.** A learner is a map (schema `Learner`):
  `:events` (a transducer), `:statistic` (its schema), `:empty`, `:step`, `:combine`,
  `:readout`. What is known is plain data, kept apart from the learner. There is one way to
  learn (`learner/learn`), with no convenience wrappers for now. A new learner:
  - puts its step, combine and readout in public `defn`s with `:malli/schema`, and refers to
    them from the map as vars (`#'step`) or calls to them (`(fn [k e] (step lambda k e))`),
    never as function values: a function captured before `instrument!` is never guarded.
  - states the three laws in its test through `test/robertluo/markov/laws.clj`
    (`identity-law`, `associativity-law`, `parts-law` with its own way to split
    observations); generators for walks are in `test/robertluo/markov/gen.clj`.
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
- Show things through `robertluo.markov.view` (`notebook/robertluo/markov/view.clj`): state
  diagram, transition matrix, side-by-side heatmaps, row intervals, walk timeline, share bars,
  error curve, a humanized schema refusal, and seeded `draws`. Charts take a palette (`[[state colour] ...]`) so a state keeps
  its colour across a page. A way of showing that a second notebook would want goes there, with
  `:malli/schema` like any public fn; one-off presentation stays a `defn-` in the notebook.
- Draws come from a seeded `java.util.Random` (`view/draws`), so a page renders the same every time.
- Rendered pages are build output (`target/notebook/`); check a rendered page, not just that it
  renders — Clay does not run the charts' JavaScript.
