# Mathias' personal site

[![CI](https://github.com/MathiasSM/hakyll-site/actions/workflows/ci.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/ci.yml)
[![CodeQL](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml)

[![License](https://badgen.net/github/license/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![Top language](https://img.shields.io/github/languages/top/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![Dependabot](https://badgen.net/github/dependabot/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/MathiasSM/hakyll-site/badge)](https://securityscorecards.dev/viewer/?uri=github.com/MathiasSM/hakyll-site)

## Setup

1. Install ghcup (e.g. using `mise use -g ghcup`)
2. Use ghcup to install `ghc`, `cabal`, possibly `hls`.

## Build

1. The content lives on a different git repo (see [Content](#content)).
  - Clone it inside as `content/` (git-ignored): `git clone git@github.com:MathiasSM/web-writings.git content`
2. `cabal build`
3. `cabal exec site <CMD>`; these are hakyll commands
  - `cabal exec site build` (generate site)
  - `cabal exec site clean` (cleanup and remove cache)
  - `cabal exec site rebuild` (clean and build again)
  - `cabal exec site server` (run server on what's built)
  - `cabal exec site watch` (recompile server)
  - There's `-v` (verbose) and `-h` (help) flags

## Test

1. `cabal test spec` runs the fast unit tests (`test/`).
2. `cabal test snapshot` builds a small fixture site (`test/fixtures/content`) in-process and compares the output with the baseline in `test/snapshot`. When a change to the output is intended, review the diff and run `cabal test snapshot --test-options=--update`. (Needs ImageMagick, like the build itself.) The two suites are independent: bare `cabal test` runs both, but each can be run on its own.

CI is split across `.github/workflows/`:

- `ci.yml` verifies and builds every pushed branch (lint, format, build, unit tests, coverage, docs) via the reusable `reusable-*.yml` jobs, and on `master` assembles the `site` executable + templates + assets into a rolling `latest` release ("kit").
- `notify-content.yml` announces a newly published kit to `web-writings` so it can rebuild and deploy.
- `codeql.yml` scans the workflow files for security issues, `dependencies.yml` reports stale Haskell packages and reviews dependency changes in PRs, and `scorecard.yml` reports the OpenSSF security score.

It needs `ghc 9.10.3`, `cabal >= 3.14` and ImageMagick, and pins fourmolu 0.20.1.0 and hlint 3.10.

## Deploying

This repo no longer deploys the site; it ships a **generator kit** that `web-writings` uses to build and publish the content. On `master`, CI publishes a rolling `latest` release containing the `site` executable plus `templates/` and `assets/` (`site-kit.tar.gz` + a checksum); `notify-content.yml` then dispatches a `kit-released` event to `web-writings`.

`web-writings` (`.github/workflows/ci.yml`) downloads the kit, builds its own content (`CONTENT_DIR=.`, `SITE_DOMAIN` set per target), runs the checks, and deploys:

- **gamma** — `web-writings`' own GitHub Pages (`mathiassm.github.io/web-writings`), on pushes to `new`.
- **prod** — `mathiassm.github.io` (custom domain `mathiassm.dev`), on pushes to `master`.

The domain each build uses is set at build time via the `SITE_DOMAIN` environment variable (see `src/MathiasSM/Config.hs`).

One-time setup:

1. In this repo: the **secret** `WEB_WRITINGS_DISPATCH_TOKEN`, a fine-grained token with *Contents: read and write* on `MathiasSM/web-writings` (used by `notify-content.yml`).
2. In `web-writings`: enable GitHub Pages ("GitHub Actions"), create the `prod` environment, and add the **secret** `DEPLOY_TOKEN` (write on `MathiasSM/mathiassm.github.io`).

The `prod` environment and `DEPLOY_TOKEN` gate the real deployment. GitHub disables scheduled workflows in a public repo after 60 days without repository activity; a push re-enables them.

## Format and lint

1. Install the tools once (e.g. `ghcup install fourmolu` and `ghcup install hlint`, or `cabal install fourmolu hlint`).
2. `scripts/format` formats the Haskell sources (fourmolu, configured in `fourmolu.yaml`); `scripts/format --check` only checks, and fails with a diff if something would change.
3. `hlint app src test test-snapshot` lints them; it should report no hints.

## Content

The `content/` folder is its own git repo (`web-writings`), ignored by this one:

```
content/
  pages/       standalone pages: about, contact, 404, showcase, and one index page per post group
  projects/    one markdown file per showcase project (front matter only)
  tables/      hobbies.tsv, socials.tsv, experience.tsv
  blog/        posts
  creative-writing/  posts
```

The build stops with a list of anything missing from this set (see `requiredContent` in `src/MathiasSM/Config.hs`).

### Pages (`pages/<name>.md`)

Registered by name in `app/Main.hs` (`standalonePages`) and routed by their `path:`: `/contact` becomes `contact/index.html`, and a path with an extension, like `/404.html`, is kept as is.

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Page title. |
| `path` | yes | Public URL (e.g. `/contact`). |
| `nav` | no | Puts the page in the site menu with this label (HTML allowed, e.g. `'<i>Es</i>critos'`). Needs `navOrder`. |
| `navOrder` | with `nav` | Position in the menu, lowest first. |
| `aliases` | no | Old paths that redirect here (a list like `["/old-name"]`, or a single string). Each must start with `/`; collisions with other pages fail the build. |
| `description` | no | Summary shown on the page and in `<meta name="description">`. |
| `type` | no | Kind of page (currently `page`). |
| `language` | no | `en` (default), `es` or `jp`. |
| `home` | no | `true` for the home page; gives it top sitemap priority. |
| `templated` | no | `true` to allow template syntax (`$for(...)$`, `$partial(...)$`) in the body. |
| `shareTitle` | no | Title used for social sharing cards (falls back to `title`). |
| `shareDescription` | no | Description for social sharing cards (falls back to `description`). |

### Posts (`blog/`, `creative-writing/`)

Each group `<g>` needs an index page `pages/<g>.md`. Every file in a group is published unless marked `draft: true`, and the build fails (naming the file and every problem) if its front matter is invalid. Each hobby listed in `tables/hobbies.tsv` needs a blog post whose `path:` equals the row's `href`.

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Post title. |
| `date` | yes | Publication date, `YYYY-MM-DD`. |
| `path` | yes | Public URL (e.g. `/blog/my-post`). |
| `aliases` | no | Old paths that redirect here (a list like `["/old-name"]`, or a single string). Each must start with `/`; collisions with other pages fail the build. |
| `language` | no | `en` (default), `es` or `jp`. Anything else fails the build. |
| `lastModifiedAt` | no | Last edit date, `YYYY-MM-DD`. |
| `draft` | no | `true` to leave the post out of the site (and skip validation) while it's in progress. |
| `description`, `TOC`, `project` | no | Passed through to templates. |

### Projects (`projects/<name>.yaml`)

One YAML file per project, listed on `/showcase` (no front matter fences, no body). A missing or invalid key fails the build, naming the file. Order: projects with a `priority` first (lowest number first), then the rest, newest `startDate` first.

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Project name. |
| `href` | yes | Link target of the project card (`#` for none). |
| `startDate` | yes | Start date, `YYYY-MM-DD`; shown as "Started on". |
| `status` | yes | Shown as a badge: `ongoing`, `finished`, `alpha`, `unmaintained`. |
| `shortDescription` | yes | One-line summary. |
| `longDescription` | yes | Longer summary. |
| `endDate` | no | End date, `YYYY-MM-DD`; shown as "Finished". |
| `team` | no | List of collaborators. |
| `priority` | no | Whole number; lower numbers come first. |
