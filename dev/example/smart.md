---
title: "Smart typography"
description: "Quotes, dashes, and ellipses rewritten at parse time"
path: "/example/smart"
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pandoc rewrites punctuation as it parses, so "double quotes" and 'single quotes' become curly quotation marks, a -- dash -- becomes an en dash, a --- dash --- an em dash, and trailing dots... become an ellipsis.

## What gets rewritten

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat:

- Straight `"quotes"` turn into curly "quotes".
- Two hyphens `--` turn into an en dash -- in prose.
- Three hyphens `---` turn into an em dash --- in prose.
- Three periods `...` turn into an ellipsis...

## Escaping

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Code spans are always left alone, like `--flag`, and a backslash keeps a character literal in prose: \-- stays two hyphens, \... stays three dots.
