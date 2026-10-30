# Create Neon schemas, registry tables, one Postgres table per registered CSV, and seed data.
# Usage: Rscript scripts/setup_neon.R

if (file.exists(".Renviron")) readRenviron(".Renviron")

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

source("internal/modules/helpers.R")

PROJECT_ROOT <- normalizePath(".", winslash = "/")
META_PATH    <- file.path(PROJECT_ROOT, "data", "metadata", "datasets.csv")
LOG_PATH     <- file.path(PROJECT_ROOT, "data", "upload_log.csv")

create_app_schema <- function(con) {
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS app")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS app.datasets (
      id              SERIAL PRIMARY KEY,
      dataset         TEXT NOT NULL,
      goal            TEXT,
      folder          TEXT NOT NULL,
      pg_schema       TEXT NOT NULL,
      pg_table        TEXT NOT NULL,
      source_type     TEXT,
      display_name    TEXT,
      source_name     TEXT,
      source_url      TEXT,
      contact_name    TEXT,
      contact_email   TEXT,
      how_to_collect  TEXT,
      last_updated    DATE,
      archived        TEXT,
      fetcher         TEXT,
      row_count       INTEGER,
      year_min        INTEGER,
      year_max        INTEGER,
      UNIQUE (pg_schema, pg_table),
      UNIQUE (dataset, folder)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS app.upload_log (
      id        BIGSERIAL PRIMARY KEY,
      timestamp TIMESTAMPTZ,
      dataset   TEXT,
      action    TEXT,
      app_user  TEXT,
      note      TEXT
    )
  ")

  neon_ensure_users_table(con)
  neon_ensure_dataset_backups_table(con)
  neon_seed_admin_user(con)
}

create_data_table <- function(con, schema, table, df) {
  DBI::dbExecute(con, sprintf("CREATE SCHEMA IF NOT EXISTS %s", DBI::dbQuoteIdentifier(con, schema)))

  DBI::dbExecute(con, sprintf(
    "DROP TABLE IF EXISTS %s.%s",
    DBI::dbQuoteIdentifier(con, schema),
    DBI::dbQuoteIdentifier(con, table)
  ))

  col_defs <- vapply(names(df), function(col) {
    sprintf(
      "%s TEXT",
      DBI::dbQuoteIdentifier(con, sanitize_col_name(col))
    )
  }, character(1))

  create_sql <- sprintf(
    "CREATE TABLE %s.%s (row_id SERIAL PRIMARY KEY, %s)",
    DBI::dbQuoteIdentifier(con, schema),
    DBI::dbQuoteIdentifier(con, table),
    paste(col_defs, collapse = ", ")
  )
  DBI::dbExecute(con, create_sql)
}

seed_data_table <- function(con, schema, table, df) {
  if (!nrow(df)) return(0L)

  names(df) <- sanitize_col_name(names(df))

  for (col in names(df)) {
    df[[col]] <- as.character(df[[col]])
    df[[col]][is.na(df[[col]])] <- ""
  }

  tbl <- DBI::Id(schema = schema, table = table)
  DBI::dbExecute(con, sprintf(
    "TRUNCATE %s.%s",
    DBI::dbQuoteIdentifier(con, schema),
    DBI::dbQuoteIdentifier(con, table)
  ))

  DBI::dbWriteTable(con, tbl, df, append = TRUE, row.names = FALSE)
  nrow(df)
}

main <- function() {
  if (!"--confirm-reseed" %in% commandArgs(trailingOnly = TRUE)) {
    stop("This script replaces registered data. Back up your database, then pass --confirm-reseed for an approved target.")
  }
  if (!file.exists(META_PATH)) stop("Missing metadata: ", META_PATH)

  meta <- read.csv(META_PATH, stringsAsFactors = FALSE)
  con  <- db_connect_raw()
  on.exit(db_disconnect_all(), add = TRUE)

  cat("Connected to Neon\n")
  create_app_schema(con)
  neon_ensure_registry_columns(con)

  # Clear registry for clean re-seed (data tables truncated per dataset below)
  DBI::dbExecute(con, "TRUNCATE app.datasets RESTART IDENTITY")

  created <- 0L
  seeded  <- 0L
  skipped <- character()

  for (i in seq_len(nrow(meta))) {
    row <- meta[i, ]
    schema <- as.character(row$folder)
    table  <- csv_to_table_name(row$dataset)
    csv_path <- file.path(PROJECT_ROOT, "data", schema, row$dataset)

    if (!file.exists(csv_path)) {
      skipped <- c(skipped, sprintf("%s/%s (file missing)", schema, row$dataset))
      next
    }

    df <- tryCatch(
      read.csv(csv_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || !ncol(df)) {
      skipped <- c(skipped, sprintf("%s/%s (empty or unreadable)", schema, row$dataset))
      next
    }

    create_data_table(con, schema, table, df)
    n <- seed_data_table(con, schema, table, df)
    created <- created + 1L
    seeded  <- seeded + n

    last_up <- row$last_updated
    if (is.na(last_up) || !nzchar(trimws(last_up))) last_up <- NULL
    stats <- compute_dataset_stats(df)

    DBI::dbExecute(
      con,
      "INSERT INTO app.datasets (
         dataset, goal, folder, pg_schema, pg_table,
         source_type, display_name, source_name, source_url,
         contact_name, contact_email, how_to_collect,
         last_updated, archived, fetcher,
         row_count, year_min, year_max
       ) VALUES (
         $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15,
         $16, $17, $18
       )",
      params = list(
        row$dataset, row$goal, row$folder, schema, table,
        row$source_type, row$display_name, row$source_name, row$source_url,
        row$contact_name, row$contact_email, row$how_to_collect,
        last_up, row$archived, row$fetcher,
        stats$row_count, stats$year_min, stats$year_max
      )
    )

    cat(sprintf("  OK %s.%s — %d rows from %s\n", schema, table, n, row$dataset))
  }

  if (file.exists(LOG_PATH)) {
    log_df <- read.csv(LOG_PATH, stringsAsFactors = FALSE)
    DBI::dbExecute(con, "TRUNCATE app.upload_log RESTART IDENTITY")
    if (nrow(log_df)) {
      if (!"timestamp" %in% names(log_df)) log_df$timestamp <- NA
      log_df$timestamp <- as.POSIXct(log_df$timestamp, tz = "UTC")
      if ("user" %in% names(log_df)) {
        names(log_df)[names(log_df) == "user"] <- "app_user"
      }
      keep <- intersect(names(log_df), c("timestamp", "dataset", "action", "app_user", "note"))
      log_df <- log_df[, keep, drop = FALSE]
      DBI::dbWriteTable(con, DBI::Id(schema = "app", table = "upload_log"),
                         log_df, append = TRUE, row.names = FALSE)
      cat(sprintf("  upload_log: %d rows\n", nrow(log_df)))
    }
  }

  cat("\nDone.\n")
  cat(sprintf("  Tables created/updated: %d\n", created))
  cat(sprintf("  Data rows seeded: %d\n", seeded))
  if (length(skipped)) {
    cat("  Skipped:\n")
    for (s in skipped) cat("    - ", s, "\n", sep = "")
  }

  n_ds <- as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM app.datasets")$n)
  cat(sprintf("  Registry rows: %d\n", n_ds))
}

main()
