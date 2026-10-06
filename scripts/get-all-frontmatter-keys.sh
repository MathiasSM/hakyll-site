#!/usr/bin/env bash
# Lists all unique top-level frontmatter keys from files in data/
find data -type f \( -name '*.md' -o -name '*.markdown' -o -name '*.html' -o -name '*.org' \) -print0 |
  xargs -0 awk '
    FNR == 1 { infm = 0; started = 0 }
    /^---[ \t]*$/ { if (!started) { started = 1; infm = 1; next } else infm = 0 }
    infm && /^[A-Za-z_][A-Za-z0-9_-]*:/ { sub(/:.*/, ""); print }
  ' | sort -u
