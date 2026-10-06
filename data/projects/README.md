# Project frontmatter

Projects live in `data/projects/` and are listed on `/showcase`. Files missing
a required key are skipped by Hakyll (`matchMetadata` in `Rules/Showcase.hs`),
so this README (which has no frontmatter) is not processed.

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
