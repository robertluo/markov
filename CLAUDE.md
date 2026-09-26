# markov

Implementations of Markov chains, starting from the simplest form. See README.md.

## Commands

| | |
|---|---|
| test | `clojure -M:dev:test` (kaocha lives in `:dev`; `-M:test` alone has no runner) — also `devenv test` |
| lint | `clojure -M:lint --lint src test dev` |
| REPL | `clojure -M:dev:nrepl`, then `(markov.instrument/instrument!)` after loading a namespace |

## Conventions

- **Every public function carries `:malli/schema`** (`[:=> [:cat ...] ret]`). `dev/markov/instrument.clj`
  collects every `markov.*` namespace and instruments it; kaocha runs that as a `post-load` hook
  (tests.edn), so tests always run against guarded functions.
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
- Tests: `test/markov/<ns>_test.clj`, one per source namespace.
