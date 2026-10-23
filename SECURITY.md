# Security Policy

How to report security vulnerabilities in this project, what is in scope, and
how reports are handled.

## Reporting a vulnerability

Use GitHub's **private vulnerability reporting**:

1. Open the [Security tab](https://github.com/MathiasSM/Cozyll/security).
2. Click **Report a vulnerability**.
3. Fill in the advisory with the details below.

Please do **not** open a public issue for security reports.

### What to include

- The affected component and version (from `CHANGELOG.md` or the release tag).
- Steps to reproduce, ideally as a minimal example.
- The potential impact.
- Any workarounds you have found.

## Scope

In scope:

- The site generator code in `src/` and `app/`.
- The build, CI, and release processes.
- Pinned dependencies and their configuration.

Out of scope:

- Issues specific to the content of a particular site (e.g. `mathiassm.dev`);
  report those via that site's `.well-known/security.txt`.
- Vulnerabilities in third-party dependencies themselves, unless this
  repository's usage or pinning is the cause (report those upstream).

## Supported versions

Only the latest release receives security fixes. Please upgrade before
reporting against an older release.

| Version        | Supported |
| -------------- | --------- |
| Latest release | Yes       |
| Older releases | No        |

## Response expectations

Reports are acknowledged within ~7 days. Fixes are prepared as soon as feasible;
there is no fixed fix-by date. Please keep details private until a fix is
released.

## Disclosure

Follows coordinated disclosure: a GitHub Security Advisory is used to
coordinate the fix, reporters are credited (unless they opt out), and details
are published alongside the release.

## security.txt

This repository also generates a `.well-known/security.txt` for sites built
with it. That file covers the *deployed site*; use it for website-specific
reports. This policy covers the *generator and its supply chain*.
