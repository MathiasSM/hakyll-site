---
title: "HTML"
description: "Definition lists, fenced divs, and raw HTML"
path: "/example/html"
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Besides plain Markdown, the site supports a few richer block constructs.

## Definition lists

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris:

Markdown
:   A lightweight markup language, rendered here as a `<dl>` definition list.

Hakyll
:   The static site generator that turns these source files into HTML.

Pandoc
:   The document converter used for Markdown parsing.

## Fenced divs

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. A fenced div wraps content in a styled block:

::::: {.note}
This paragraph sits inside a `{.note}` div, handy for callouts, warnings, or asides.
:::::

::::: {.hobbies}
A second div, demonstrating that arbitrary classes can be applied.
:::::

## Raw HTML

Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Raw HTML blocks pass through untouched:

```{=html}
<details>
  <summary>Click to expand</summary>
  <p>This is raw HTML, inserted verbatim.</p>
</details>
```

## Inline HTML

Like keyboard keys: <kbd>Ctrl</kbd>. Marked text: <mark>like this</mark>.
