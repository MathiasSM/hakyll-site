# Mathias' personal site

## How to develop?

1. Install `ghcup`
2. Use `ghcup` to install `cabal` and `ghc`
3. `cabal build`
4. (Most) content lives on a different git repo.
  - Clone inside: `git clone git@github.com:MathiasSM/web-writings.git data/posts`
5. `cabal exec site <CMD>`; these are hakyll commands
  - `cabal exec site build` (generate site)
  - `cabal exec site clean` (cleanup and remove cache)
  - `cabal exec site rebuild` (clean and build again)
  - `cabal exec site server` (run server on what's built)
  - `cabal exec site watch` (recompile server)
  - There's `-v` (verbose) and `-h` (help) flags
