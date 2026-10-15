# Mathias' personal site
[![License](https://badgen.net/github/license/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![Top language](https://img.shields.io/github/languages/top/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)

[![CI](https://github.com/MathiasSM/hakyll-site/actions/workflows/_ci.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/_ci.yml)
[![CodeQL](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml/badge.svg)](https://github.com/MathiasSM/hakyll-site/actions/workflows/codeql.yml)
[![Dependabot](https://badgen.net/github/dependabot/MathiasSM/hakyll-site)](https://github.com/MathiasSM/hakyll-site)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/MathiasSM/hakyll-site/badge)](https://scorecard.dev/viewer/?uri=github.com/MathiasSM/hakyll-site)

This is a [hakyll](https://hackage.haskell.org/package/hakyll)-based site generator. It's main purpose is to build [mathiassm.dev (my personal site)](https://mathiassm.dev), but it can be used to build other kinds of sites with similar needs. In essence, it's a thin-ish layer of extra logic, some premade templates and styling, on top of the actual generator library by [jaspervdj](https://jaspervdj.be/).

## Usage

You may download the [latest release](https://github.com/MathiasSM/hakyll-site/releases/tag/latest), which should include an executable for ubuntu (I can tweak the release thing to provide statically-linked executables, or builds for other platforms, but I don't know of anyone needing them as of writing). You may run the executable in a directory like:

- `templates/` hakyll templates that define components of each (usually html) pages. There are included files in the release tarball and this repo, but you may change them as you need to.
- `assets/` holds styles and potentially other files that are logically coupled with templates/ and it didn't make much sense to "extract" into the content repo. Same as with templates, there's a bundle included, which is the one I use for my personal site; feel free to change to your liking.
- `content/` where all writing, images and other stuff lives. To build my personal site, I use a separate repo that holds these files.
  - While I try to keep this generator as content-agnostic as possible, right now there are some required files, given my personal site needs. These are "documented" in `src/MathiasSM/Config.hs` (`requiredContent :: SiteConfig -> [FilePath]`), but the build will fail and indicate if any is missing.
  - Besides actual content, the directory is expected to hold a `website.yaml` file with metadata about the site, used to build the different parts of it. See `example.website.yaml`.

The way to execute this is same as any hakyll site: `executable <CMD>`; if you build it from source, you can use `cabal exec site <CMD>` instead.

- CMD is any of:
  - `build` (generate site)
  - `clean` (cleanup and remove cache)
  - `rebuild` (clean and build again)
  - `server` (run server on what's built)
  - `watch` (recompile server)
  - `check` (check the built html for broken links)
    - Note: By default hakyll checks external links as well, but due to some broken external sites, I decided to disable it via compilation flags.

You may need to setup some required env variables; you may export them manually, use a .env file or whatever your preferred method is. See 

```sh
# Environment.
# dev: building might skip optimization steps, and create example/ pages
SITE_ENV=dev
# prod: building will run all optimizations and validations
# SITE_ENV=prod

# Wherever you're placing the directory with website.yaml
CONTENT_DIR=content
# Wherever you're placing the templates (you may use the included ones)
TEMPLATES_DIR=templates
# Wherever you're placing the assets (you may use the included ones)
ASSETS_DIR=assets

```

## Development

### Local

#### Setup

1. Install ImageMagick (e.g. via `sudo apt instal imagemagick`).
2. Install ghcup (e.g. using `mise use -g ghcup`).
3. Use ghcup to install `ghc`, `cabal` (both required), `hls`, `fourmolu`, `hlint` (optional).
  - Remember to `ghcup set <TOOL>` (in particular, you might need to `ghcup set cabal 3.14` because of hlint support)

#### Build

1. `cabal build all`: First time will take a while, but dependencies won't need to recompile often.

#### Format and lint

Either

- Via your editor (in my case, NeoVim's lsp registering the installed `hls`, configured with `hlint` and `fourmolu`).
- Calling the executables directly over `app/`, `src/`, `test/`.

#### Test

Using [Tasty](https://hackage.haskell.org/package/tasty) to organize/run tests.

- Run all tests: `cabal test`.
- Only Unit tests: `cabal test --test-options='-p unit'`
- Only Golden tests: `cabal test --test-options='-p golden'`
  - Golden files are stored in `test/golden`
  - Updating golden files: `cabal test --test-options='-p golden --accept'`

### CI

Configured via Github Actions within `.github` directory.

- `_ci.yml` holds the main CI logic, while other files are either reusable pieces or workflows triggered separatedly. 
- Github doesn't support Haskell dependencies, so most dependency checks are focused on actions dependencies.
- `notify-content.yml` announces a newly published kit to `web-writings` so it can rebuild and deploy.
  - Note: I haven't figured out a trivial or native way to avoid having _this_ repo notify the content repo, but rather have the content repo listen to or poll this one directly.

### CD / Deployment

This project "deploys" as a release, via the CI workflow. Content repos can pull the latest release and execute it against their files. The release includes both the executable (not statically linked as of writing), as well as templates and assets.

---

## Content-agnostic repo

The idea is for this repo to be content-agnostic, but the current customizations _require_ some minimum structure documented in `src/MathiasSM/Config.hs`'s `requiredContent`. Missing files will fail the site build.

Any release includes `templates/` and `assets/` alongside the executable; take them as the default structure and style of the built site. You may edit/bring your own templates/assets to build a site to your liking.

