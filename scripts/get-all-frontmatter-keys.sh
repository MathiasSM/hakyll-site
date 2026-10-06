#!/usr/bin/env bash
# Lists all unique top-level keys used by the content: front matter in the markdown files
# (and html/org), and the whole file for the YAML projects.
{
  find content -type f \( -name '*.md' -o -name '*.markdown' -o -name '*.html' -o -name '*.org' \) -print0 |
    xargs -0 awk '
      FNR == 1 { infm = 0; started = 0 }
      /^---[ \t]*$/ { if (!started) { started = 1; infm = 1; next } else infm = 0 }
      infm && /^[A-Za-z_][A-Za-z0-9_-]*:/ { sub(/:.*/, ""); print }
    '
  find content -type f -name '*.yaml' -print0 |
    xargs -0 awk '/^[A-Za-z_][A-Za-z0-9_-]*:/ { sub(/:.*/, ""); print }'
} | sort -u
