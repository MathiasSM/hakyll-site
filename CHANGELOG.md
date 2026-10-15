# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- This CHANGELOG file. Added to the .cabal file to include in sdist package.
- A pre-commit git hook, opt-in, that runs hlint and fourmolu before a commit, and fails if it needed changes.
- A RELEASE-CHECKLIST, listing the steps to "release" a new version of this project.
- A CONTRIBUTING file, explaining briefly the expected workflow, the use of githooks and referencing RELEASE-CHECKLIST

### Changed

- README: Mentioning new githooks, referencing new CONTRIBUTING, RELEASE-CHECKLIST.

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



[Unreleased]: https://github.com/MathiasSM/hakyll-site/compare/v0.4.0.0...master
[0.4.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.3.0.0...v0.4.0.0
[0.3.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/MathiasSM/hakyll-site/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/MathiasSM/hakyll-site/releases/tag/v0.1.0.0
