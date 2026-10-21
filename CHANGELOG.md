# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- Posts now fail the build when `lastModifiedAt` is before `date`.


## [1.0.0.0] - 2026-10-20

### Added

- Data list items can be hidden with `_show: false`, which omits them from the `$for(name)$` loop.
- A JSON Schema for the site configuration (`schema/website.schema.json`).
- Example configuration files (`schema/example.website.yaml` and `schema/example.socials.yaml`).

### Changed

- Moved `example.website.yaml` into `schema/`.
- Moved `socials.schema.json` into `schema/` from the content repository; the `website` and `socials` schemas are now closed (they reject unknown fields).

## [0.5.0.0] - 2026-10-19

### Added

- Generic data files: any YAML under `content/data/**/*.yaml` is exposed to templates by name, with no fixed schema (lists via `$for(name)$`, maps via `$name.key$`).
- An icon auto-transform: any field whose name contains `icon` also exposes `<field>Href` (its URL) and `<field>Raw` (the inlined SVG).
- Optional build-time JSON Schema validation for data files via a co-located `<name>.schema.json`.
- A Code of Conduct.

### Changed

- Replaced the hand-rolled `.githooks/pre-commit` hook with
  [pre-commit.com](https://pre-commit.com/), running `fourmolu`, `hlint`, and
  `actionlint`. The fourmolu hook now runs in check mode rather than rewriting
  files in place.
- Moved the mise configuration into a project-level `mise.toml` (declaring
  `pre-commit` and `actionlint`), replacing the parent `~/Desktop` config.
- `socials` icon fields renamed `iconPath`/`iconSource` to `iconHref`/`iconRaw`.
- Renamed `minimalCtx` to `globalContext`.
- Data files (`hobbies`, `experience`, `socials`) moved from TSV tables to YAML files.
- Split the test suite into `spec` (unit) and `golden` (snapshot) suites; CI publishes JUnit XML results to Codecov test analytics.

### Removed

- The strict `hobbies` and `experience` parsers and their schemas; these are now generic data files.
- The `validateHobbyPages` cross-check between hobbies and their posts.

### Fixed

- Removed a duplicated `pull_request` key in the OpenSSF Scorecard workflow,
  restoring the `branches: [master]` filter.
- Corrected the environment-variable documentation: removed the stale
  `TEMPLATES_DIR`/`ASSETS_DIR` references and clarified the variables are optional.

## [0.4.0.2] - 2026-10-17

### Added

- This CHANGELOG file. Added to the .cabal file to include in sdist package.
- A pre-commit git hook, opt-in, that runs hlint and fourmolu before a commit, and fails if it needed changes.
- A RELEASE-CHECKLIST, listing the steps to "release" a new version of this project.
- A CONTRIBUTING file, explaining briefly the expected workflow, the use of githooks and referencing RELEASE-CHECKLIST
- An OpenSSF Best Practices badge.
- A GitHub release workflow that tags the version and publishes GitHub and Hackage drafts.
- Basic styles for figures and images within posts.

### Changed

- README: Mentioning new githooks, referencing new CONTRIBUTING, RELEASE-CHECKLIST.
- Pinned dependencies via `cabal.project.freeze` and keyed the CI cache on it.
- CI: upload coverage to Codecov instead of GitHub Code Quality.
- Bumped GitHub Actions dependencies (scorecard-action, actions/cache, hlint-scan, download-artifact).

### Fixed

- Removed the broken `CONTENT_DIR`, `TEMPLATES_DIR`, and `ASSETS_DIR` environment-variable overrides.

### Security

- Hardened GitHub Actions (StepSecurity recommendations and `contents: read` permissions).
- Run OpenSSF Scorecard on pull requests.

## [0.4.0.0] - 2026-10-15

### Added

- Dev-only `/example/` preview pages behind the `SITE_ENV=dev` flag.
- Haddock documentation across all modules.
- Basic accessibility checks on the output HTML; e.g. every image requires `alt` text.

### Changed

- License changed to BSD-3-Clause.
- Site identity is read from content repo's `website.yaml`; some environment variables can override individual fields.
- Deployment configuration is read from the environment; the templates/assets directories and feed size are now configurable.
- Content model: pages and posts merged into `writings/`, post groups discovered from content, and the showcase treated as a group.
- Static site files are generated from `website.yaml`; dropped `assets/static`.
- Images moved into `content/media`.

### Fixed

- Sitemap no longer emits an empty `lastmod`.
- `just-one-title` accessibility validation.

### Security

- The build fails when `security.txt` is within a configured expiry margin.

## [0.3.0.0] - 2026-10-09

### Added

- Sitemap in `robots.txt`; `trust.txt`, `funding.json`, and `security.txt` well-known files.
- Atom feed.
- Navigation driven by content.
- Redirects/aliases from frontmatter.
- fourmolu + hlint formatting/linting.
- Snapshot (golden) testing.
- GitHub Actions CI.

### Changed

- Refactored rules to be filetype-agnostic; simplified single pages, metadata, and config.
- Moved content to the `web-writings` repository.
- Moved data to TSV with a typed parser (`Post`/`Project`/etc).
- Reorganized images/css/static into `assets/`; templated static files.

### Fixed

- Canonical paths/404s, post dates, favicon filenames, and the per-page `lang` attribute.

## [0.2.0.0] - 2024-04-07

### Changed

- New ghcup-based build configuration.

## [0.1.0.0] - 2021-12-05

### Added

- Initial version replicating a Gatsby site.
- GPL-3.0 license.



[Unreleased]: https://github.com/MathiasSM/hakyll-site/compare/v0.5.0.0...master
[0.5.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.4.0.2...v0.5.0.0
[0.4.0.2]: https://github.com/MathiasSM/hakyll-site/compare/v0.4.0.0...v0.4.0.2
[0.4.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.3.0.0...v0.4.0.0
[0.3.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/MathiasSM/hakyll-site/releases/tag/v0.1.0.0
