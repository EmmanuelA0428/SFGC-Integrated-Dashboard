# Neon / Postgres I/O for the SFGC dashboard.
# Activated when DATABASE_URL is set (see .Renviron).

has_val <- function(x) {
  !is.na(x) && nchar(trimws(as.character(x))) > 0
}

fp_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(a$md5, b$md5)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

parse_database_url <- function(url = Sys.getenv("DATABASE_URL")) {
  if (!nzchar(url)) {
    stop("DATABASE_URL is not set. Add it to .Renviron (see .env.example).")
  }
  base <- sub("\\?.*$", "", url)
  m <- regexec("^postgres(?:ql)?://([^:]+):([^@]+)@([^:/]+)(?::(\\d+))?/(.+)$", base)
  parts <- regmatches(base, m)[[1]]
  if (length(parts) < 6) stop("Could not parse DATABASE_URL")
  list(
    user     = parts[2],
    password = parts[3],
    host     = parts[4],
    port     = if (nzchar(parts[5])) as.integer(parts[5]) else 5432L,
    dbname   = parts[6]
  )
}

use_neon_db <- function() {
  nzchar(Sys.getenv("DATABASE_URL")) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RPostgres", quietly = TRUE)
}

db_connect_raw <- function() {
  if (!use_neon_db()) stop("Neon database is not configured.")
  cfg <- parse_database_url()
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = cfg$host,
    port     = cfg$port,
    user     = cfg$user,
    password = cfg$password,
    dbname   = cfg$dbname,
    sslmode  = "require"
  )
}

# Reuse one connection per R process (Shiny server). Opening/closing on every
# query added ~100–300ms latency to Neon; the pooler URL handles multiplexing.
.db_state <- new.env(parent = emptyenv())

get_db_con <- function() {
  if (exists("con", envir = .db_state, inherits = FALSE)) {
    con <- .db_state$con
    if (DBI::dbIsValid(con)) return(con)
  }
  .db_state$con <- db_connect_raw()
  .db_state$con
}

db_connect <- function() {
  get_db_con()
}

db_disconnect_all <- function() {
  if (exists("con", envir = .db_state, inherits = FALSE)) {
    con <- .db_state$con
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    rm("con", envir = .db_state)
  }
}

neon_ensure_registry_columns <- function(con = get_db_con()) {
  DBI::dbExecute(con, "
    ALTER TABLE app.datasets ADD COLUMN IF NOT EXISTS row_count INTEGER
  ")
  DBI::dbExecute(con, "
    ALTER TABLE app.datasets ADD COLUMN IF NOT EXISTS year_min INTEGER
  ")
  DBI::dbExecute(con, "
    ALTER TABLE app.datasets ADD COLUMN IF NOT EXISTS year_max INTEGER
  ")
}

csv_to_table_name <- function(dataset_file) {
  nm <- sub("\\.csv$", "", dataset_file, ignore.case = TRUE)
  nm <- gsub("-", "_", nm)
  nm <- gsub("[^a-zA-Z0-9_]", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- gsub("^_|_$", "", nm)
  tolower(nm)
}

sanitize_col_name <- function(nm) {
  nm <- as.character(nm)
  out <- vapply(nm, function(s) {
    s <- gsub("\\.", "_", s)
    s <- gsub("[^a-zA-Z0-9_]", "_", s)
    s <- gsub("_+", "_", s)
    s <- gsub("^_|_$", "", s)
    if (!nzchar(s)) s <- "col"
    s
  }, character(1), USE.NAMES = FALSE)
  seen <- character()
  for (i in seq_along(out)) {
    base <- out[i]
    if (base %in% seen) {
      k <- 2L
      candidate <- paste0(base, "_", k)
      while (candidate %in% seen) {
        k <- k + 1L
        candidate <- paste0(base, "_", k)
      }
      out[i] <- candidate
    }
    seen <- c(seen, out[i])
  }
  out
}

meta_pg_locators <- function(meta_row) {
  schema <- as.character(meta_row$folder %||% "")
  if ("pg_schema" %in% names(meta_row) && has_val(meta_row$pg_schema)) {
    schema <- as.character(meta_row$pg_schema)
  }
  table <- csv_to_table_name(meta_row$dataset)
  if ("pg_table" %in% names(meta_row) && has_val(meta_row$pg_table)) {
    table <- as.character(meta_row$pg_table)
  }
  list(schema = schema, table = table)
}

pg_qualify <- function(con, schema, table) {
  sprintf(
    "%s.%s",
    DBI::dbQuoteIdentifier(con, schema),
    DBI::dbQuoteIdentifier(con, table)
  )
}

# Neon transaction-mode pooler breaks prepared statements ($1, …). Inline literals.
pg_sql_str <- function(con, x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("NULL")
  DBI::dbQuoteString(con, as.character(x))
}

pg_sql_int <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("NULL")
  as.integer(x)
}

pg_sql_date <- function(con, x) {
  d <- trimws(as.character(x %||% ""))
  if (!nzchar(d)) d <- format(Sys.Date(), "%Y-%m-%d")
  paste0(DBI::dbQuoteString(con, d), "::date")
}

pg_sql_timestamp <- function(con, x) {
  if (is.null(x)) x <- Sys.time()
  tz <- Sys.timezone()
  # Store instant correctly as timestamptz (local wall time → Postgres UTC).
  paste0(
    DBI::dbQuoteString(con, format(x, "%Y-%m-%d %H:%M:%S", tz = tz)),
    "::timestamptz"
  )
}

df_for_pg_write <- function(df) {
  names(df) <- sanitize_col_name(names(df))
  for (col in names(df)) {
    df[[col]] <- as.character(df[[col]])
    df[[col]][is.na(df[[col]])] <- ""
  }
  df
}

neon_create_data_table <- function(con, schema, table, df) {
  DBI::dbExecute(con, sprintf(
    "CREATE SCHEMA IF NOT EXISTS %s",
    DBI::dbQuoteIdentifier(con, schema)
  ))
  qual <- pg_qualify(con, schema, table)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", qual))
  col_defs <- vapply(names(df), function(col) {
    sprintf("%s TEXT", DBI::dbQuoteIdentifier(con, sanitize_col_name(col)))
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (row_id SERIAL PRIMARY KEY, %s)",
    qual, paste(col_defs, collapse = ", ")
  ))
}

neon_write_table_data <- function(con, schema, table, df) {
  if (!nrow(df)) {
    qual <- pg_qualify(con, schema, table)
    DBI::dbExecute(con, sprintf("TRUNCATE %s", qual))
    return(0L)
  }
  df <- df_for_pg_write(df)
  neon_create_data_table(con, schema, table, df)
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = table),
    df, append = TRUE, row.names = FALSE
  )
  nrow(df)
}

neon_dataset_fingerprint <- function(meta_row) {
  loc <- meta_pg_locators(meta_row)
  con <- get_db_con()
  qual <- pg_qualify(con, loc$schema, loc$table)
  exists <- DBI::dbGetQuery(con, sprintf(
    "SELECT to_regclass('%s.%s') IS NOT NULL AS ok",
    loc$schema, loc$table
  ))$ok
  if (!isTRUE(exists)) return(NULL)
  stats <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*)::bigint AS n, COALESCE(MAX(row_id), 0)::bigint AS mx FROM %s",
    qual
  ))
  list(
    mtime = NULL,
    md5   = paste0("neon:", stats$n[[1]], ":", stats$mx[[1]])
  )
}

format_log_ts_local <- function(x) {
  if (!length(x)) return(character())
  tz <- Sys.timezone()
  vapply(x, function(raw) {
    if (inherits(raw, "POSIXct")) {
      return(format(raw, "%Y-%m-%d %H:%M:%S", tz = tz))
    }
    s <- trimws(as.character(raw))
    if (!nzchar(s)) return(s)
    s <- sub("T", " ", s, fixed = TRUE)
    s <- sub("\\.[0-9]+$", "", s)
    ts <- suppressWarnings(as.POSIXct(s, tz = "UTC"))
    if (is.na(ts)) ts <- suppressWarnings(as.POSIXct(s, tz = tz))
    if (is.na(ts)) return(s)
    format(ts, "%Y-%m-%d %H:%M:%S", tz = tz)
  }, character(1), USE.NAMES = FALSE)
}

neon_load_meta <- function() {
  con <- get_db_con()
  m <- DBI::dbGetQuery(con, "
    SELECT id, dataset, goal, folder, pg_schema, pg_table,
           source_type, display_name, source_name, source_url,
           contact_name, contact_email, how_to_collect,
           last_updated, archived, fetcher,
           row_count, year_min, year_max
    FROM app.datasets
    ORDER BY id
  ")
  if (nrow(m) == 0) return(m)
  if ("goal" %in% names(m)) m$goal <- as.character(m$goal)
  if ("folder" %in% names(m)) m$folder <- as.character(m$folder)
  if (!"archived" %in% names(m)) m$archived <- ""
  if (!"fetcher" %in% names(m)) m$fetcher <- ""
  m$archived <- ifelse(is.na(m$archived) | m$archived == "FALSE", "", as.character(m$archived))
  m$archived <- ifelse(m$archived == "TRUE", "yes", m$archived)
  m$fetcher <- ifelse(is.na(m$fetcher), "", as.character(m$fetcher))
  if ("last_updated" %in% names(m)) {
    m$last_updated <- as.character(m$last_updated)
    m$last_updated[m$last_updated == "NA"] <- ""
  }
  normalize_meta_columns(m)
}

neon_load_dataset <- function(meta_row) {
  loc <- meta_pg_locators(meta_row)
  con <- get_db_con()
  qual <- pg_qualify(con, loc$schema, loc$table)
  exists <- DBI::dbGetQuery(con, sprintf(
    "SELECT to_regclass('%s.%s') IS NOT NULL AS ok",
    loc$schema, loc$table
  ))$ok
  if (!isTRUE(exists)) return(NULL)
  df <- tryCatch(
    DBI::dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY row_id", qual)),
    error = function(e) NULL
  )
  if (is.null(df) || !ncol(df)) return(NULL)
  if ("row_id" %in% names(df)) df$row_id <- NULL
  ensure_df_colnames(df)
}

neon_load_upload_log <- function() {
  con <- get_db_con()
  df <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT timestamp, dataset, action, app_user AS user, note
      FROM app.upload_log
      ORDER BY id
    "),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      timestamp = character(), dataset = character(),
      action    = character(), user    = character(), note = character()
    ))
  }
  df$timestamp <- format_log_ts_local(df$timestamp)
  df
}

neon_dataset_where <- function(con, dataset_name, folder = NULL) {
  id <- dataset_identity(dataset_name, folder)
  sprintf("dataset = %s AND folder = %s",
          DBI::dbQuoteString(con, id$dataset), DBI::dbQuoteString(con, id$folder))
}

neon_update_last_updated <- function(dataset_name, folder = NULL) {
  con <- get_db_con()
  q_ds <- neon_dataset_where(con, dataset_name, folder)
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.datasets SET last_updated = %s WHERE %s",
      pg_sql_date(con, Sys.Date()), q_ds
    )
  )
}

neon_update_dataset_stats <- function(dataset_name, row_count, year_min, year_max, folder = NULL) {
  con <- get_db_con()
  q_n   <- if (is.na(row_count)) "NULL" else as.integer(row_count)
  q_min <- if (is.na(year_min)) "NULL" else as.integer(year_min)
  q_max <- if (is.na(year_max)) "NULL" else as.integer(year_max)
  q_ds  <- neon_dataset_where(con, dataset_name, folder)
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.datasets SET row_count = %s, year_min = %s, year_max = %s WHERE %s",
      q_n, q_min, q_max, q_ds
    )
  )
}

neon_append_upload_log <- function(dataset, action, note = "", user = NULL) {
  con <- get_db_con()
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO app.upload_log (timestamp, dataset, action, app_user, note)
       VALUES (%s, %s, %s, %s, %s)",
      pg_sql_timestamp(con, as.POSIXct(Sys.time(), tz = Sys.timezone())),
      pg_sql_str(con, dataset),
      pg_sql_str(con, action),
      pg_sql_str(con, user %||% Sys.info()[["user"]]),
      pg_sql_str(con, note)
    )
  )
}

neon_atomic_write_dataset <- function(df, meta_row, expected_fp = NULL) {
  if (!is.null(expected_fp)) {
    cur <- neon_dataset_fingerprint(meta_row)
    if (!fp_equal(cur, expected_fp)) {
      return(list(
        ok = FALSE,
        error = "The dataset changed since you opened it. Discard changes and try again."
      ))
    }
  }
  neon_backup_dataset_snapshot(meta_row)
  loc <- meta_pg_locators(meta_row)
  con <- get_db_con()
  n <- neon_write_table_data(con, loc$schema, loc$table, df)
  list(ok = TRUE, fp = neon_dataset_fingerprint(meta_row), rows = n)
}

neon_update_dataset_metadata <- function(dataset_name, fields = list(), folder = NULL) {
  con <- get_db_con()
  q_ds <- neon_dataset_where(con, dataset_name, folder)
  m <- DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM app.datasets WHERE %s", q_ds)
  )
  if (nrow(m) != 1L) {
    return(list(ok = FALSE, error = "Could not match dataset row in metadata."))
  }
  editable <- c(
    "display_name", "source_type", "source_name", "source_url",
    "contact_name", "contact_email", "how_to_collect"
  )
  updates <- list()
  for (nm in editable) {
    if (!is.null(fields[[nm]])) {
      val <- fields[[nm]]
      if (is.null(val) || length(val) == 0L) val <- ""
      updates[[nm]] <- as.character(val)
    }
  }
  if (!length(updates)) return(list(ok = TRUE))
  if ("source_type" %in% names(updates)) {
    t <- trimws(updates$source_type)
    if (!nzchar(t) || !t %in% c("public", "contact", "hybrid")) {
      row <- m[1, ]
      for (nm in names(updates)) row[[nm]] <- updates[[nm]]
      has_url <- nzchar(trimws(as.character(row$source_url %||% "")))
      has_contact <- nzchar(trimws(as.character(row$contact_name %||% ""))) ||
        nzchar(trimws(as.character(row$contact_email %||% "")))
      updates$source_type <- if (has_url && has_contact) "hybrid"
      else if (has_contact) "contact"
      else "public"
    }
  }
  for (nm in names(updates)) {
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE app.datasets SET %s = %s WHERE %s",
        DBI::dbQuoteIdentifier(con, nm),
        pg_sql_str(con, updates[[nm]]),
        q_ds
      )
    )
  }
  list(ok = TRUE)
}

neon_set_dataset_archived <- function(dataset_name, archived = TRUE, folder = NULL) {
  con <- get_db_con()
  flag <- if (isTRUE(archived)) "yes" else ""
  q_ds <- neon_dataset_where(con, dataset_name, folder)
  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE app.datasets SET archived = %s WHERE %s",
      pg_sql_str(con, flag), q_ds
    )
  )
  TRUE
}

neon_register_dataset_metadata <- function(row, replace_existing = FALSE) {
  con <- get_db_con()
  schema <- as.character(row$folder)
  table  <- csv_to_table_name(row$dataset)
  rc <- row$row_count %||% NA_integer_
  ymin <- row$year_min %||% NA_integer_
  ymax <- row$year_max %||% NA_integer_
  q_ds <- pg_sql_str(con, row$dataset)
  q_folder <- pg_sql_str(con, row$folder)

  if (isTRUE(replace_existing)) {
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE app.datasets SET
           goal = %s, folder = %s, pg_schema = %s, pg_table = %s,
           source_type = %s, display_name = %s, source_name = %s, source_url = %s,
           contact_name = %s, contact_email = %s, how_to_collect = %s,
           last_updated = %s, archived = %s, fetcher = %s,
           row_count = %s, year_min = %s, year_max = %s
         WHERE dataset = %s AND folder = %s",
        pg_sql_str(con, row$goal),
        q_folder,
        pg_sql_str(con, schema),
        pg_sql_str(con, table),
        pg_sql_str(con, row$source_type %||% "public"),
        pg_sql_str(con, row$display_name %||% ""),
        pg_sql_str(con, row$source_name %||% ""),
        pg_sql_str(con, row$source_url %||% ""),
        pg_sql_str(con, row$contact_name %||% ""),
        pg_sql_str(con, row$contact_email %||% ""),
        pg_sql_str(con, row$how_to_collect %||% ""),
        pg_sql_date(con, row$last_updated %||% Sys.Date()),
        pg_sql_str(con, row$archived %||% ""),
        pg_sql_str(con, row$fetcher %||% ""),
        pg_sql_int(rc), pg_sql_int(ymin), pg_sql_int(ymax),
        q_ds, q_folder
      )
    )
    return(list(ok = TRUE))
  }

  tryCatch({
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO app.datasets (
           dataset, goal, folder, pg_schema, pg_table,
           source_type, display_name, source_name, source_url,
           contact_name, contact_email, how_to_collect,
           last_updated, archived, fetcher,
           row_count, year_min, year_max
         ) VALUES (
           %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
           %s, %s, %s, %s, %s, %s
         )",
        q_ds,
        pg_sql_str(con, row$goal),
        q_folder,
        pg_sql_str(con, schema),
        pg_sql_str(con, table),
        pg_sql_str(con, row$source_type %||% "public"),
        pg_sql_str(con, row$display_name %||% ""),
        pg_sql_str(con, row$source_name %||% ""),
        pg_sql_str(con, row$source_url %||% ""),
        pg_sql_str(con, row$contact_name %||% ""),
        pg_sql_str(con, row$contact_email %||% ""),
        pg_sql_str(con, row$how_to_collect %||% ""),
        pg_sql_date(con, row$last_updated %||% Sys.Date()),
        pg_sql_str(con, row$archived %||% ""),
        pg_sql_str(con, row$fetcher %||% ""),
        pg_sql_int(rc), pg_sql_int(ymin), pg_sql_int(ymax)
      )
    )
    list(ok = TRUE)
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

neon_create_empty_dataset <- function(goal, folder, file_name, headers) {
  blank <- as.data.frame(lapply(headers, function(.) character(0)), stringsAsFactors = FALSE)
  names(blank) <- headers
  loc <- list(schema = folder, table = csv_to_table_name(file_name))
  con <- get_db_con()
  neon_create_data_table(con, loc$schema, loc$table, blank)
  list(ok = TRUE, schema = loc$schema, table = loc$table)
}

neon_drop_dataset_table <- function(folder, file_name) {
  con <- get_db_con()
  table <- csv_to_table_name(file_name)
  qual <- pg_qualify(con, folder, table)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", qual))
  invisible(TRUE)
}

neon_dataset_registered <- function(folder, file_name, active_only = TRUE) {
  con <- get_db_con()
  q_folder <- DBI::dbQuoteString(con, folder)
  q_ds <- DBI::dbQuoteString(con, file_name)
  rows <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT archived FROM app.datasets WHERE folder = %s AND dataset = %s",
      q_folder, q_ds
    )
  )
  if (nrow(rows) == 0) return(FALSE)
  if (!active_only) return(TRUE)
  !any(is_archived_flag(rows$archived))
}

neon_ensure_dataset_backups_table <- function(con = get_db_con()) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS app.dataset_backups (
      folder       TEXT NOT NULL,
      dataset      TEXT NOT NULL,
      row_count    INTEGER,
      col_count    INTEGER,
      csv_data     TEXT NOT NULL,
      backed_up_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      PRIMARY KEY (folder, dataset)
    )
  ")
}

df_to_csv_text <- function(df) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(df, tmp, row.names = FALSE)
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

csv_text_to_df <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(as.character(txt)))) return(NULL)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(as.character(txt), tmp, useBytes = TRUE)
  tryCatch(read.csv(tmp, stringsAsFactors = FALSE), error = function(e) NULL)
}

dataset_backup_ref <- function(meta_row) {
  sprintf("neon:%s/%s", as.character(meta_row$folder), as.character(meta_row$dataset))
}

neon_has_dataset_backup <- function(meta_row) {
  con <- get_db_con()
  folder <- as.character(meta_row$folder %||% "")
  dataset <- as.character(meta_row$dataset %||% "")
  if (!nzchar(folder) || !nzchar(dataset)) return(FALSE)
  n <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT COUNT(*)::int AS n FROM app.dataset_backups
       WHERE folder = %s AND dataset = %s",
      DBI::dbQuoteString(con, folder),
      DBI::dbQuoteString(con, dataset)
    )
  )$n
  isTRUE(n > 0L)
}

neon_fetch_dataset_backup_row <- function(meta_row) {
  con <- get_db_con()
  folder <- as.character(meta_row$folder %||% "")
  dataset <- as.character(meta_row$dataset %||% "")
  df <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT folder, dataset, row_count, col_count, csv_data, backed_up_at
       FROM app.dataset_backups
       WHERE folder = %s AND dataset = %s",
      DBI::dbQuoteString(con, folder),
      DBI::dbQuoteString(con, dataset)
    )
  )
  if (nrow(df) != 1L) return(NULL)
  df
}

neon_backup_dataset_snapshot <- function(meta_row) {
  tryCatch({
    df <- neon_load_dataset(meta_row)
    if (is.null(df) || !ncol(df)) return(list(ok = FALSE, skipped = TRUE))
    df <- ensure_df_colnames(df)
    csv_txt <- df_to_csv_text(df)
    con <- get_db_con()
    folder <- as.character(meta_row$folder %||% "")
    dataset <- as.character(meta_row$dataset %||% "")
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO app.dataset_backups
           (folder, dataset, row_count, col_count, csv_data, backed_up_at)
         VALUES (%s, %s, %d, %d, %s, NOW())
         ON CONFLICT (folder, dataset) DO UPDATE SET
           row_count = EXCLUDED.row_count,
           col_count = EXCLUDED.col_count,
           csv_data = EXCLUDED.csv_data,
           backed_up_at = EXCLUDED.backed_up_at",
        DBI::dbQuoteString(con, folder),
        DBI::dbQuoteString(con, dataset),
        as.integer(nrow(df)),
        as.integer(ncol(df)),
        DBI::dbQuoteString(con, csv_txt)
      )
    )
    list(ok = TRUE, ref = dataset_backup_ref(meta_row))
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

neon_backup_summary <- function(meta_row, preview_rows = 8L) {
  row <- neon_fetch_dataset_backup_row(meta_row)
  if (is.null(row)) return(NULL)
  df <- csv_text_to_df(row$csv_data[[1]])
  if (is.null(df) || !ncol(df)) return(NULL)
  df <- ensure_df_colnames(df)
  list(
    path = dataset_backup_ref(meta_row),
    file = "Previous version",
    saved_at = format(as.POSIXct(row$backed_up_at[[1]], tz = Sys.timezone()),
                      "%Y-%m-%d %H:%M:%S"),
    saved_at_ago = format_relative_time(row$backed_up_at[[1]]),
    rows = nrow(df),
    cols = ncol(df),
    colnames = colnames(df),
    preview = head(df, preview_rows)
  )
}

neon_restore_dataset_from_backup <- function(meta_row) {
  row <- neon_fetch_dataset_backup_row(meta_row)
  if (is.null(row)) {
    return(list(ok = FALSE, error = "No backup found in the database."))
  }
  df <- csv_text_to_df(row$csv_data[[1]])
  if (is.null(df) || !ncol(df)) {
    return(list(ok = FALSE, error = "Could not read backup from the database."))
  }
  df <- ensure_df_colnames(df)
  neon_backup_dataset_snapshot(meta_row)
  loc <- meta_pg_locators(meta_row)
  con <- get_db_con()
  neon_write_table_data(con, loc$schema, loc$table, df)
  update_last_updated(meta_row$dataset, folder = meta_row$folder)
  update_dataset_stats(meta_row$dataset, df, folder = meta_row$folder)
  list(ok = TRUE, rows = nrow(df), backup = dataset_backup_ref(meta_row))
}
