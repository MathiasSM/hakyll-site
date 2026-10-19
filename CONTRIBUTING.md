# Contributing

## Branches

Work happens on short-lived feature branches named `type/description`, merged
to `master` ONLY via a pull request. You may use
the [Conventional Commits](https://www.conventionalcommits.org/) types
(`feat/`, `fix/`, `docs/`, `chore/`, `ci/`, `refactor/`, `test/`, ...); not a strict policy.

## Development tools

Install these with ghcup, or via `cabal install PKG-VERSION`:

- **fourmolu** `0.20.1.0` — pinned to match CI (`FOURMOLU_VERSION`).
- **hlint** — use the **latest** if in doubt. It's intentionally not pinned
  here: CI runs hlint from inside the `haskell-actions/hlint-scan` action
  image, which bundles its own hlint version, so there's no in-repo version
  to mirror. Any recent hlint is fine for local use.

## Commits

Keep commits clean and focused. Run `fourmolu` and `hlint` before committing;
the repository ships a [pre-commit](https://pre-commit.com/) hook that does
this automatically. Install it with `mise install` (declared in `mise.toml`),
then enable the hook:

```sh
pre-commit install
```

Run it manually over the whole tree with `pre-commit run --all-files`.

## Releases

Version bumps happen only in a dedicated release PR: bump `version:` in
`mathiassm.cabal` and add a matching `[version]` entry to `CHANGELOG.md`.

Follow [RELEASE-CHECKLIST.md](RELEASE-CHECKLIST.md) for details.