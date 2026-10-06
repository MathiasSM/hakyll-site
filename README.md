# Mathias' personal site

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

1. `cabal test` runs the unit tests (`test/`)

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

The build stops with a list of anything missing from this set (see `requiredContent` in `src/MathiasSM/Config.hs`). Run `scripts/get-all-frontmatter-keys.sh` to list the front matter keys in use.

### Pages (`pages/<name>.md`)

Registered by name in `app/Main.hs` (`standalonePages`) and routed by their `path:`: `/contact` becomes `contact/index.html`, and a path with an extension, like `/404.html`, is kept as is.

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Page title. |
| `path` | yes | Public URL (e.g. `/contact`). |
| `description` | no | Summary shown on the page and in `<meta name="description">`. |
| `type` | no | Kind of page (currently `page`). |
| `language` | no | `en` (default), `es` or `jp`. |
| `home` | no | `true` for the home page; gives it top sitemap priority. |
| `templated` | no | `true` to allow template syntax (`$for(...)$`, `$partial(...)$`) in the body. |
| `shareTitle` | no | Title used for social sharing cards (falls back to `title`). |
| `shareDescription` | no | Description for social sharing cards (falls back to `description`). |

### Posts (`blog/`, `creative-writing/`)

Each group `<g>` needs an index page `pages/<g>.md`. A post needs `title`, `date` and `path`; files missing one are skipped by Hakyll. Each hobby listed in `tables/hobbies.tsv` needs a blog post whose `path:` equals the row's `href`.

### Projects (`projects/<name>.md`)

Listed on `/showcase`. Files missing a required key are skipped (`matchMetadata` in `Rules/Showcase.hs`).

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Project name. |
| `href` | yes | Link target of the project card (`#` for none). |
| `startDate` | yes | Start date; shown as "Started on". |
| `status` | yes | Shown as a badge: `ongoing`, `finished`, `alpha`, `unmaintained`. |
| `shortDescription` | yes | One-line summary (needs both descriptions). |
| `longDescription` | yes | Longer summary (needs both descriptions). |
| `date` | no | Placeholder date (used for sorting/sitemap only). |
| `team` | no | List of collaborators. |
| `priority` | no | Ordering hint (unused by templates so far). |
| `finishDate`, `finishedDate`, `lastDate` | no | Legacy end-date keys; the date template reads `endDate`, so these currently aren't shown. |
