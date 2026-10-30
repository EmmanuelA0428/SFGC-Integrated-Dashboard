# Verification of the public source release

Verified on 2026-10-30 (UTC) in an isolated Linux environment using R 4.5.3, a disposable PostgreSQL 15 database with TLS, and local headless Chromium. Original production credentials were not loaded or used.

## Checks passed

- All 33 project R files parsed without syntax errors.
- The database-enabled suite passed all 15 test cases and 78 expectations, with zero failures, errors, warnings or skipped cases.
- The local-only suite also passed; three database-dependent cases are deliberately skipped when no test database is configured.
- Local demo data initialization succeeded; synthetic dataset registry and CSVs loaded.
- Database initialization succeeded against a new disposable PostgreSQL database, creating two synthetic datasets and the registry/authentication tables.
- PostgreSQL authentication tests exercised account creation, duplicate rejection, login, password changes, display-name changes, deactivation and audit-log attribution.
- Same-named datasets in different folders were tested for isolated metadata, timestamp, row-count and archive updates, both in local CSV mode and PostgreSQL. Ambiguous filename-only updates were rejected.
- Mock sessions verified isolated identities, admin/editor/viewer permissions and logout reset. A real browser test kept independent viewer and admin sessions open simultaneously: the viewer remained read-only after the admin logged in. A forged viewer manage event did not enable management mode.
- Browser login, Overview navigation and the Explore chart/table rendered without visible Shiny errors or JavaScript exceptions. The rendered synthetic chart and table were visually inspected.
- Public-file selection omitted runtime/client data, environment files, deployment files and old Git history. An exact-value comparison found no original database/API credentials in the selected public source files.

## Fixes included

1. Replaced process-global web identity with per-session reactive authentication state; logout clears the session identity.
2. Applied folder-plus-filename identifiers throughout affected dataset actions and database predicates.
3. Added authorization checks at mutation confirmation points and fail-closed handling for unknown roles.
4. Removed built-in application passwords. Demo login now needs explicit local-only configuration; initial database admin setup needs a supplied password.
5. Made tests use only an explicit disposable `TEST_DATABASE_URL`, not an inherited production database URL.
6. Added a non-overwriting synthetic demo initializer and an explicit acknowledgement for the destructive database reseed script.

## Limits and remaining warnings

- This is a focused functional verification, not a full security audit or exhaustive test of every dashboard action.
- The live Neon database, existing hosted dashboard and original client datasets were not accessed or changed.
- AI provider calls, Census/education/health refresh endpoints, report rendering, email/contact links and deployment to shinyapps.io were not end-to-end tested.
- The original `renv.lock` was preserved; an exact R 4.3.3 lockfile restore was not verified in this environment.
- Current bslib emitted pre-existing startup warnings about deprecated navbar options and navigation-container content. These did not prevent the checked login, navigation, chart or table from working.
- The public demo has fabricated values, not the production dashboard's complete registry or records. Production configuration and permission review remain necessary before deployment.
