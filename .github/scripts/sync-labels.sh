#!/usr/bin/env bash
set -euo pipefail

repo="$GITHUB_REPOSITORY"
manifest=".github/labels.yaml"
prune="${LABELS_PRUNE:-false}"
dry_run="${LABELS_DRY_RUN:-false}"

run() {
  if [ "$dry_run" = "true" ]; then
    echo "[dry-run] $*"
  else
    "$@"
  fi
}

# Create or update every label in labels.yaml
while IFS=$'\t' read -r name color desc; do
  run gh label create "$name" \
    --color "${color#\#}" \
    --description "$desc" \
    --force --repo "$repo"
done < <(yq -r '.[] | [.name, .color, .description] | @tsv' "$manifest")

# Optionally delete labels not in labels.yaml
if [ "$prune" = "true" ]; then
  while read -r name; do
    run gh label delete "$name" --repo "$repo" --yes
  done < <(comm -23 \
    <(gh label list --repo "$repo" --json name --jq '.[].name' | sort) \
    <(yq -r '.[].name' "$manifest" | sort))
fi
