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
2. `scripts/check-site` builds a small fixture site (`test/fixtures/content`) and compares the output with the golden snapshot in `test/golden`. When a change to the output is intended, review the diff and run `scripts/check-site --update`. (Needs ImageMagick, like the build itself.)

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
