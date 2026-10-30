# SFGC Integrated Dashboard

R/Shiny internal dashboard developed for the Success for Geneva's Children data-capstone project. Staff can explore registered datasets, maintain metadata, replace data, review change history, and manage access.

**This is a public, source-code-only release—not a public release of client records or an unrestricted live dashboard.** Original data, contacts, upload logs, backups, database credentials, API keys, deployment credentials, and the ZIP's old Git history are excluded. The examples are entirely fabricated and must not be interpreted as Geneva statistics.

## Quick start: local synthetic demo

1. Install R and restore the project packages. `renv.lock` records the original R 4.3.3 environment. Verification of this release used R 4.5.3; an exact lockfile restore was not independently verified.

   ```r
   install.packages("renv")
   renv::restore()
   ```

   On Linux, native packages may need a compiler and development libraries for PostgreSQL/libpq, libsodium, libuv, curl, OpenSSL and XML. Optional refreshers may require additional packages and API credentials.

2. From the repository root, initialize **synthetic** data:

   ```sh
   Rscript scripts/setup_demo.R
   ```

   This refuses to overwrite an existing `data/` directory. Keep any real data and its backups outside version control.

3. Copy `.env.example` to `.Renviron`. Uncomment only the settings you need. For the local demo, leave `DATABASE_URL` unset and supply your own passwords, at least eight characters:

   ```text
   SFGC_DEMO_MODE=true
   SFGC_DEMO_ADMIN_PASSWORD=replace-with-your-own-local-password
   SFGC_DEMO_EDITOR_PASSWORD=replace-with-another-local-password
   SFGC_DEMO_VIEWER_PASSWORD=replace-with-a-third-local-password
   ```

   Demo usernames are `admin`, `editor`, and `viewer`. There are **no built-in application passwords**. Demo authentication requires explicit opt-in, and is disabled whenever a database URL is configured. Do not enable demo mode for an internet-facing deployment.

4. Restart R so `.Renviron` is loaded, then run from the root:

   ```r
   shiny::runApp(".")
   ```

   Use the root `app.R`, which wraps the dashboard with authentication. Do not deploy `internal/app.R` directly.

## Project map

- `app.R`: authenticated application entry point.
- `internal/app.R`, `internal/modules/`: dashboard UI, session-scoped authorization, dataset I/O, editor, explorer, system management, optional assistant.
- `pipeline/`: optional external data refreshers.
- `scripts/`: demo initialization, database setup, tests and maintenance.
- `examples/demo-data/`: fabricated CSVs and dataset registry, including a same-filename regression example.
- `tests/testthat/`: local auth, session isolation, dataset identification and optional PostgreSQL tests.
- `data/`: runtime datasets, registry, logs and backups; intentionally ignored.

Datasets are identified by **folder plus filename**, not filename alone. Session identity and permissions are separate for each Shiny session.

## Database-backed deployment

The existing production design uses Neon PostgreSQL over TLS. Add your own `DATABASE_URL` to a private `.Renviron` or the host's secret settings; never commit it. Optional API keys belong there too. Supply a unique `BOOTSTRAP_ADMIN_PASSWORD` of at least 12 characters when initializing an empty users table, then manage accounts in the dashboard.

`scripts/setup_neon.R` is a **destructive initialization/reseed tool**, not a routine application-start command: it clears the registry and replaces registered data tables. Back up the target database and confirm the dataset registry and CSVs are approved before running:

```sh
Rscript scripts/setup_neon.R --confirm-reseed
```

Do not run this against an existing production database merely to try the demo. A new public GitHub repository does not redeploy or change the existing hosted app. Deploy the root directory with environment secrets supplied privately and review hosting access rules separately.

## Tests

```sh
Rscript scripts/run_tests.R
```

Local tests use synthetic fixtures and explicitly opted-in test passwords; those fixture values are not production credentials or application defaults. The runner ignores a production `DATABASE_URL` for tests. To exercise database integration, set `TEST_DATABASE_URL` to a **disposable PostgreSQL database**, plus `TEST_ADMIN_PASSWORD` (and `BOOTSTRAP_ADMIN_PASSWORD` for an empty users table). These tests create/update users, write logs and exercise dataset mutations. Never use production for testing.

See [VERIFICATION.md](VERIFICATION.md) for the tested scope and limits. This release is not a comprehensive security audit or a certification of every workflow or external service.

## Sensitive material in the original archive

The supplied archive contained environment credentials. They are not included in this repository. Rotate the original database and API credentials if that archive has been shared beyond trusted recipients; making this cleaned repository public does not revoke credentials in old copies.
