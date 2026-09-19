# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`ejc-sql` is an Emacs SQL client. It is **two programs in one repository**: an
Emacs Lisp frontend (`*.el` in the root) and a Clojure/JDBC backend
(`src/ejc_sql/*.clj`), wired together by [clomacs](https://github.com/clojure-emacs/clomacs)
over a CIDER nREPL. Almost every non-trivial change touches both sides, and the
seam between them is where the bugs live.

## Commands

```bash
make elpa                          # cask install - REQUIRED before the first `make test`
make test                          # the whole suite (see below)
lein test                          # Clojure side only, no Emacs needed
lein test ejc-sql.deps-resolver-test   # a single Clojure namespace
lein check                         # reflection warnings - not a clean baseline,
                                   # classpath.clj, lib.clj and output.clj have
                                   # their own, clj-http's are third-party
```

`make test` runs `cask exec emacs --script test/ejc-tests.el`, which starts a
real nREPL via `clomacs-with-nrepl` and runs every ERT test. It needs `lein`,
and `mvn` too - the script calls `ejc-test:run-maven-dependency-plugin` to fetch
the H2 driver first. Unlike `clomacs`, the `test` target here does **not**
depend on `elpa`, so a fresh clone fails with `Cannot open load file: sesman`
until `make elpa` is run (CI runs them in that order).

ERT tests are tagged: `el` (pure Emacs Lisp), `cl` (shells out to `lein test`),
`el+cl` (needs the live nREPL bridge). There is no command line selector - the
bottom of `test/ejc-tests.el` runs them all whenever the file is loaded in batch
mode, with `'(tag el)` and `'(tag el+cl)` sitting there commented out. To narrow
the run, uncomment one of those; to check one Clojure namespace, use `lein test`
with its name.

Byte compilation is warning-clean and should stay that way:

```bash
cask exec emacs -Q -batch -L . -f batch-byte-compile ejc-sql.el
```

When a change starts using something new from `clomacs`, the copy of it that
`cask` pulled from MELPA into `.cask/` can lag behind, and anything that loads
`ejc-interaction.el` - the test suite, the byte compilation above - dies with
`Eager macro-expansion failure: (error "Keyword argument :timeout not one of
(...)")` or its equivalent. `cask update` refreshes it once MELPA has rebuilt;
until then, put the `clomacs` checkout on the load path ahead of `.cask`.

## Architecture

**The bridge.** `ejc-interaction.el` is the single place where the Emacs side
reaches the Clojure side: every `clomacs-defun` there wraps one Clojure function
as an Emacs command. The reverse direction (Clojure calling Emacs) goes over an
HTTP server started by `ejc-httpd-start`, declared in the same file. A Clojure
function that is only ever called from Emacs looks dead to the linter - such
names are listed in `.clj-kondo/config.edn` under `:clojure-lsp/unused-public-var`,
and new ones have to be added there.

**Query results are asynchronous and file-based.** `ejc-eval-user-sql-at-point`
returns immediately; on the Clojure side `eval-sql-and-log-print` runs the query
in a `future`, writes the formatted table to a *result file*, then calls
`ejc-complete-query` back in Emacs over the HTTP bridge, which loads that file
into the results buffer. There is a ring of result files behind
`ejc-show-prev-result` / `ejc-show-next-result`. The result files and the query
log land in an `ejc-sql/` directory relative to the JVM working directory
(gitignored).

**Connections.** `ejc-create-connection` normalizes its arguments into an alist
in `ejc-connections`; `:classpath` is always stored as a vector of
`file-truename`d paths. `ejc-connect-to-db` turns that into a classpath: with
`:dependencies` it resolves Leiningen coordinates through
`src/ejc_sql/deps_resolver.clj` (Pomegranate/Aether, one `resolve-dependencies`
call, remote repositories involved), with `:classpath` it derives coordinates
from the `~/.m2/repository` layout via `ejc-path-to-lein-artifact` and resolves
those. A jar outside `~/.m2` has no derivable coordinates, so it is added to the
classpath as is. Dependency resolution is the only request that can hit the
network, hence its own `ejc-dependencies-resolve-timeout` instead of the global
`nrepl-sync-request-timeout`.

**Database metadata is per-vendor.** `src/ejc_sql/structure.clj` holds a map of
SQL snippets per database type (`:oracle`, `:postgresql`, `:mysql`, `:sqlserver`,
`:informix`, `:h2`, `:sqlite`), selected by `select-db-meta-script` and keyed by
what is being asked for (`:tables`, `:columns`, `:procedures`, `:constraints`,
…). Adding support for a database means adding entries to those maps, not
writing new code paths. Results are memoized in `src/ejc_sql/cache.clj`, which is
what `ejc-print-cache` and `ejc-invalidate-cache` operate on; the cache lives in
memory only and is lost with the nREPL.

**Completion has three frontends over one core.** `ejc-completion-common.el`
produces the candidates (keywords, owners, tables, views, packages, columns);
`ejc-autocomplete.el`, `ejc-company.el` and `ejc-capf.el` are thin adapters for
auto-complete, company and Capf/corfu respectively. `ejc-capf.el` additionally
keeps a background, idle-timer-driven cache so typing never waits on the
database - column candidates are the exception, since they depend on the SQL
expression around point. `ejc-eldoc.el` reuses the same metadata for signatures.

**Output formatting** is Clojure-side (`src/ejc_sql/output.clj`, plain-text
tables with optional unicode borders and column width limits, tuned by the
`ejc-set-*` commands on `ejc-sql-connected-hook`). The Emacs side only displays
it, in `ejc-result-mode` or `orgtbl-mode` according to `ejc-result-table-impl`.

## Conventions

- Every `*.el` file ends with `(provide 'feature-name)`.
- Commit subjects that address an issue are prefixed `[#NNN]`.
- Releases are annotated tags `vX.Y.Z`. The version is written in three places
  and they are bumped together: `;; Version:` in `ejc-sql.el`, `defproject` in
  `project.clj` (as `-SNAPSHOT`) and two `[ejc-sql "…-SNAPSHOT"]` references in
  `README.md`.
- `README.md` carries a hand-maintained table of contents; a new heading needs
  an entry there.
