# Contributing

## Branches

Work happens on short-lived feature branches named `type/description`, merged
to `master` ONLY via a pull request. You may use
the [Conventional Commits](https://www.conventionalcommits.org/) types
(`feat/`, `fix/`, `docs/`, `chore/`, `ci/`, `refactor/`, `test/`, ...); not a strict policy.

## Commits

Keep commits clean and focused. Run `fourmolu` and `hlint` before committing;
the repository ships a pre-commit hook that does this automatically. Opt in
once you have cloned the repo with:

```sh
git config core.hooksPath .githooks
```

## Releases

Version bumps happen only in a dedicated release PR: bump `version:` in
`mathiassm.cabal` and add a matching `[version]` entry to `CHANGELOG.md`.

Follow [RELEASE-CHECKLIST.md](RELEASE-CHECKLIST.md) for details.
