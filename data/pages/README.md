# Page frontmatter

Standalone pages live in `data/pages/<name>.md` and are registered in `app/Main.hs`.
This README is not registered there and is excluded from the sitemap, so Hakyll
does not process it.

| Key | Required | Meaning |
| --- | --- | --- |
| `title` | yes | Page title. |
| `path` | yes | Public URL (e.g. `/contact`); the output route is derived from it (`/contact` becomes `contact/index.html`; a path with an extension, like `/404.html`, is kept). |
| `description` | no | Summary shown on the page and in `<meta name="description">`. |
| `type` | no | Kind of page (currently `page`). |
| `language` | no | `en` (default), `es` or `jp`. |
| `home` | no | `true` for the home page; gives it top sitemap priority. |
| `templated` | no | `true` to allow template syntax (`$for(...)$`, `$partial(...)$`) in the body. |
| `shareTitle` | no | Title used for social sharing cards (falls back to `title`). |
| `shareDescription` | no | Description for social sharing cards (falls back to `description`). |
