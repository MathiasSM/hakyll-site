# Mathias' personal site
[![License](https://badgen.net/github/license/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![Top language](https://img.shields.io/github/languages/top/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)

[![CI](https://github.com/MathiasSM/hakyll-site/actions/workflows/ci.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/ci.yml)
[![CodeQL](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml)
[![Dependabot](https://badgen.net/github/dependabot/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/MathiasSM/hakyll-site/badge)](https://scorecard.dev/viewer/?uri=github.com/MathiasSM/hakyll-site)

## Setup

1. Install ImageMagick (e.g. via `sudo apt instal imagemagick`).
2. Install ghcup (e.g. using `mise use -g ghcup`).
3. Use ghcup to install `ghc`, `cabal` (both required), `hls`, `fourmolu`, `hlint` (optional).
  - Remember to `ghcup set <TOOL>` (in particular, you might need to `ghcup set cabal 3.14` because of hlint support)

## Build

1. `cabal build`: First time will take a while, but dependencies won't need to recompile often.

## Format and lint

Either

- Via your editor (in my case, NeoVim's lsp registering the installed `hls`, `hlint` and `fourmolu`).
- Calling the exeutables directly over `app/`, `src/`, `test/`.

## Test

1. `cabal test` runs all tests (unit + golden).
  - Unit tests only: `cabal test --test-options='-p unit'`
  - Golden tests only: `cabal test --test-options='-p golden'`
  - Golden files are stored in `test/golden`
  - Updating golden files: `cabal test --test-options='-p golden --accept'`

## CI

Configured via Github Actions within `.github` directory.

- `_ci.yml` holds the main CI logic, while other files are either reusable pieces or workflows triggered separatedly. 
- `notify-content.yml` announces a newly published kit to `web-writings` so it can rebuild and deploy. TODO: Can the content repo listen to this repo instead of this repo having to send a message?
- Github doesn't support Haskell dependencies, so most dependency checks are focused on actions dependencies.

## Deployment

This project "deploys" as a release, via the CI workflow. Content repos can pull the latest release and execute it against their files. The release includes both the executable (not statically linked as of writing), as well as templates and assets.

## Content-agnostic repo (some day...)

The idea is for this repo to be content-agnostic, but the current customizations _require_ some minimum structure documented in `src/MathiasSM/Config.hs`'s `requiredContent`. Missing files will fail the site build.

## Execution: Site build

1. `cabal exec site <CMD>`:
  - `cabal exec site build` (generate site)
  - `cabal exec site clean` (cleanup and remove cache)
  - `cabal exec site rebuild` (clean and build again)
  - `cabal exec site server` (run server on what's built)
  - `cabal exec site watch` (recompile server)
  - `cabal exec site check` (check the built html for broken links)
