# Mathias' personal site

## Setup

1. Install ghcup (e.g. using `mise use -g ghcup`)
2. Use ghcup to install `ghc`, `cabal`, possibly `hls`.

## Build

1. (Most) content lives on a different git repo.
  - Clone inside: `git clone git@github.com:MathiasSM/web-writings.git data/posts`
2. `cabal build`
3. `cabal exec site <CMD>`; these are hakyll commands
  - `cabal exec site build` (generate site)
  - `cabal exec site clean` (cleanup and remove cache)
  - `cabal exec site rebuild` (clean and build again)
  - `cabal exec site server` (run server on what's built)
  - `cabal exec site watch` (recompile server)
  - There's `-v` (verbose) and `-h` (help) flags

## Test

1. `cabal test` runs the unit tests (`test/`)
