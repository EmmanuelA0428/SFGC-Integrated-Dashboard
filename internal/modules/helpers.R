# internal/modules/helpers.R
# Shared constants, helpers, and data I/O functions.

library(here)

# ── Constants ─────────────────────────────────────────────────────────────────

GOAL_LABELS <- c(
  "1"          = "Goal 1 — Healthy Births & Infant Health",
  "2"          = "Goal 2 — Child Health & Wellness",
  "3"          = "Goal 3 — Family Economic Stability",
  "4"          = "Goal 4 — Safe & Stable Families",
  "5"          = "Goal 5 — School Readiness",
  "6"          = "Goal 6 — Early Intervention & Special Ed",
  "7"          = "Goal 7 — Child Care Access",
  "8"          = "Goal 8 — Academic Achievement",
  "10"         = "Goal 10 — Civic Engagement",
  "background" = "Background — Community Context"
)

STALE_DAYS <- 400

# Milliseconds before showing a loading spinner (skip for fast registry reads).
LOADING_SPINNER_DELAY_MS <- 350L

# ── Loading UI ────────────────────────────────────────────────────────────────

loading_spinner_ui <- function(
    text = "Loading...",
    size = "sm",
    class = "dashboard-loading d-flex align-items-center gap-2 text-muted py-3"
) {
  sz <- if (identical(size, "sm")) "spinner-border-sm" else "spinner-border"
  htmltools::div(
    class = class,
    htmltools::tags$span(
      class = paste("spinner-border", sz),
      role = "status",
      `aria-hidden` = "true"
    ),
    htmltools::tags$span(text)
  )
}

loading_placeholder_ui <- function(text = "Loading...") {
  htmltools::div(
    class = "dashboard-loading-panel text-center py-4",
    loading_spinner_ui(text, size = "sm", class = "dashboard-loading justify-content-center")
  )
}

# ── String helpers ────────────────────────────────────────────────────────────

# Safe check: TRUE only if x is non-NA and non-blank
has_val <- function(x) {
  !is.na(x) && nchar(trimws(as.character(x))) > 0
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Year range string from a data frame ("2012–2023" or NULL)
get_year_range <- function(df) {
  if (is.null(df) || !"Year" %in% names(df)) return(NULL)
  yrs <- sort(unique(df$Year[!is.na(df$Year)]))
  if (length(yrs) == 0) return(NULL)
  if (length(yrs) == 1) return(as.character(yrs[1]))
  paste0(min(yrs), "\u2013", max(yrs))
}

compute_dataset_stats <- function(df) {
  n <- if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
  ymin <- NA_integer_
  ymax <- NA_integer_
  if (!is.null(df) && nrow(df) && "Year" %in% names(df)) {
    yrs <- suppressWarnings(as.numeric(df$Year))
    yrs <- yrs[!is.na(yrs)]
    if (length(yrs)) {
      ymin <- as.integer(min(yrs))
      ymax <- as.integer(max(yrs))
    }
  }
  list(row_count = as.integer(n), year_min = ymin, year_max = ymax)
}

meta_year_range <- function(r) {
  ymin <- suppressWarnings(as.integer(r$year_min %||% NA))
  ymax <- suppressWarnings(as.integer(r$year_max %||% NA))
  if (is.na(ymin) || is.na(ymax)) return(NULL)
  if (ymin == ymax) return(as.character(ymin))
  paste0(ymin, "\u2013", ymax)
}

meta_has_data <- function(r) {
  rc <- suppressWarnings(as.integer(r$row_count %||% NA))
  !is.na(rc) && rc > 0L
}

normalize_meta_columns <- function(m) {
  if (!nrow(m)) return(m)
  if ("goal" %in% names(m)) m$goal <- as.character(m$goal)
  if ("folder" %in% names(m)) m$folder <- as.character(m$folder)
  if (!"archived" %in% names(m)) m$archived <- ""
  if (!"fetcher" %in% names(m)) m$fetcher <- ""
  if (!"row_count" %in% names(m)) m$row_count <- NA_integer_
  if (!"year_min" %in% names(m)) m$year_min <- NA_integer_
  if (!"year_max" %in% names(m)) m$year_max <- NA_integer_
  m$fetcher <- ifelse(is.na(m$fetcher), "", as.character(m$fetcher))
  m
}

# Row-level source column in CSV (first matching name wins)
SOURCE_COLUMN_CANDIDATES <- c(
  "Source", "Sources", "source", "sources",
  "Source_URL", "source_url", "Data_Source", "data_source"
)

detect_source_column <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(NULL)
  nm <- names(df)
  hit <- SOURCE_COLUMN_CANDIDATES[SOURCE_COLUMN_CANDIDATES %in% nm]
  if (length(hit)) hit[[1]] else NULL
}

# Distinct non-empty values from the Source column (for UI disclosure)
distinct_sources_in_dataframe <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(character())
  col <- detect_source_column(df)
  if (is.null(col)) return(character())
  v <- trimws(as.character(df[[col]]))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(character())
  sort(unique(v))
}

shorten_source_display <- function(x, max_chars = 64L) {
  x <- as.character(x)
  if (!length(x)) return(x)
  vapply(seq_along(x), function(i) {
    s <- x[[i]]
    if (!nzchar(s) || nchar(s) <= max_chars) return(s)
    paste0(substr(s, 1L, max_chars - 1L), "\u2026")
  }, character(1))
}

# Classify a metadata row's source flavor.
#   "public"  — data comes from a public portal/URL (source_url set)
#   "contact" — data comes from a person/org (contact_name set, no URL)
#   "hybrid"  — both a public portal AND a human contact
#   "unknown" — nothing usable in metadata
# Falls back to inferring from source_url / contact_name so old rows keep
# rendering sanely during migration.
classify_source <- function(r) {
  if (is.null(r) || !length(r)) return("unknown")
  url <- trimws(as.character(r$source_url %||% ""))
  nm  <- trimws(as.character(r$contact_name %||% ""))
  em  <- trimws(as.character(r$contact_email %||% ""))
  has_url     <- nzchar(url) && grepl("^https?://", url, ignore.case = TRUE)
  has_contact <- nzchar(nm) || nzchar(em)
  t <- trimws(as.character(r$source_type %||% ""))
  if (t %in% c("public", "contact", "hybrid")) return(t)
  if (has_url && has_contact)  return("hybrid")
  if (has_url)                 return("public")
  if (has_contact)             return("contact")
  if (nzchar(trimws(as.character(r$source_name %||% "")))) return("contact")
  "unknown"
}

# Render a consistent "who owns this data" block for Overview / detail views.
# Uses only `tags$…` so callers can drop it anywhere they build HTML.
#   variant = "compact"  → single line (overview card)
#   variant = "detailed" → stacked list with icons (modals)
render_source_line <- function(r, variant = c("compact", "detailed")) {
  variant <- match.arg(variant)
  kind <- classify_source(r)

  url <- trimws(as.character(r$source_url %||% ""))
  src <- trimws(as.character(r$source_name %||% ""))
  nm  <- trimws(as.character(r$contact_name %||% ""))
  em  <- trimws(as.character(r$contact_email %||% ""))

  ext_icon <- shiny::icon("up-right-from-square", class = "small")

  public_link <- function() {
    label <- if (nzchar(src)) src else shorten_source_display(url)
    htmltools::tags$a(
      href = url, target = "_blank", rel = "noopener",
      class = "small fw-semibold text-decoration-none",
      title = url, label,
      htmltools::tags$span(class = "ms-1 text-muted", ext_icon)
    )
  }

  contact_text <- function() {
    label <- if (nzchar(nm)) nm
             else if (nzchar(src)) src
             else if (nzchar(em)) em
             else "Contact on file"
    if (nzchar(em)) {
      htmltools::tags$a(
        href = paste0("mailto:", em),
        class = "small fw-semibold text-decoration-none",
        title = em, label
      )
    } else {
      htmltools::tags$span(class = "small fw-semibold", label)
    }
  }

  if (identical(variant, "compact")) {
    body <- switch(
      kind,
      public  = public_link(),
      contact = htmltools::tagList(
        htmltools::tags$span(class = "text-muted me-1", shiny::icon("user")),
        contact_text()
      ),
      hybrid  = htmltools::tagList(
        public_link(),
        htmltools::tags$span(class = "mx-1 text-muted", "\u2022"),
        htmltools::tags$span(class = "text-muted me-1", shiny::icon("user")),
        contact_text()
      ),
      htmltools::tags$span(class = "text-muted small",
                           "No source recorded.")
    )
    label <- switch(kind,
                    public  = "Primary source",
                    contact = "Provided by",
                    hybrid  = "Primary source",
                    "Primary source")
    return(htmltools::tagList(
      htmltools::tags$div(class = "text-muted mb-1",
                          shiny::icon("bookmark"), " ", label),
      htmltools::tags$div(class = "mb-0 lh-sm", body)
    ))
  }

  # detailed variant
  items <- list()
  if (kind %in% c("public", "hybrid") && nzchar(url)) {
    items[[length(items) + 1]] <- htmltools::tags$p(
      class = "mb-1",
      shiny::icon("bookmark"), " ", public_link()
    )
  }
  if (kind %in% c("contact", "hybrid")) {
    if (nzchar(nm)) {
      items[[length(items) + 1]] <- htmltools::tags$p(
        class = "mb-1", shiny::icon("user"), " ", nm
      )
    }
    if (nzchar(em)) {
      items[[length(items) + 1]] <- htmltools::tags$p(
        class = "mb-1",
        shiny::icon("envelope"), " ",
        htmltools::tags$a(href = paste0("mailto:", em), em)
      )
    }
    if (!nzchar(nm) && !nzchar(em) && nzchar(src) && kind == "contact") {
      items[[length(items) + 1]] <- htmltools::tags$p(class = "mb-1", src)
    }
  }
  if (!length(items)) {
    items[[1]] <- htmltools::tags$p(class = "text-muted mb-0 fst-italic",
                                    "No source recorded.")
  }
  do.call(htmltools::tagList, items)
}

# Stable identifiers are folder/filename, not filenames alone.
dataset_key <- function(meta) paste0(meta$folder, "/", meta$dataset)

dataset_identity <- function(dataset_name, folder = NULL, meta = NULL) {
  dataset_name <- as.character(dataset_name)
  if (length(dataset_name) != 1L || is.na(dataset_name) || !nzchar(dataset_name)) {
    stop("Supply one dataset identifier.")
  }
  if (grepl("/", dataset_name, fixed = TRUE)) {
    key_folder <- dirname(dataset_name)
    if (!is.null(folder) && !identical(as.character(folder), key_folder)) {
      stop("Dataset folder does not match its identifier.")
    }
    folder <- key_folder
    dataset_name <- basename(dataset_name)
  }
  if (is.null(folder)) {
    if (is.null(meta)) meta <- load_meta()
    idx <- which(meta$dataset == dataset_name)
    if (length(idx) != 1L) stop("Dataset filename is missing or ambiguous; supply folder/filename.")
    folder <- as.character(meta$folder[idx])
  }
  folder <- as.character(folder)
  if (length(folder) != 1L || is.na(folder) || !nzchar(folder)) stop("Supply one dataset folder.")
  list(dataset = dataset_name, folder = folder)
}

dataset_row_index <- function(meta, dataset_name, folder = NULL) {
  id <- dataset_identity(dataset_name, folder, meta)
  idx <- which(meta$dataset == id$dataset & meta$folder == id$folder)
  if (length(idx) != 1L) stop("Could not uniquely match folder/filename in metadata.")
  idx
}

source(here("internal", "modules", "db_io.R"), local = FALSE)
source(here("internal", "modules", "auth_io.R"), local = FALSE)

# ── Data loading ──────────────────────────────────────────────────────────────

load_meta <- function() {
  if (use_neon_db()) return(neon_load_meta())
  path <- here("data", "metadata", "datasets.csv")
  if (!file.exists(path)) return(data.frame())
  m <- read.csv(path, stringsAsFactors = FALSE)
  normalize_meta_columns(m)
}

# TRUE iff the row has a non-empty fetcher handle registered in FETCHER_REGISTRY.
has_fetcher <- function(r) {
  if (is.null(r)) return(FALSE)
  handle <- r$fetcher %||% ""
  if (is.na(handle) || !nzchar(trimws(as.character(handle)))) return(FALSE)
  if (!exists("FETCHER_REGISTRY", inherits = TRUE)) return(FALSE)
  as.character(handle) %in% names(FETCHER_REGISTRY)
}

# Rows visible in Overview / Report Builder (exclude archived)
active_meta <- function(m) {
  if (!nrow(m)) return(m)
  if (!"archived" %in% names(m)) return(m)
  a <- trimws(as.character(m$archived))
  m[is.na(a) | a == "" | !(tolower(a) %in% c("yes", "y", "true", "1")), , drop = FALSE]
}

is_archived_flag <- function(x) {
  a <- trimws(as.character(x))
  !is.na(a) & tolower(a) %in% c("yes", "y", "true", "1")
}

goal_to_folder <- function(goal) {
  g <- as.character(goal)
  if (g == "background") return("background")
  paste0("goal", g)
}

dataset_path <- function(meta_row) {
  here("data", meta_row$folder, meta_row$dataset)
}

format_relative_time <- function(time) {
  if (is.null(time) || length(time) == 0L || all(is.na(time))) return("unknown time")
  ts <- as.POSIXct(time[[1]], tz = Sys.timezone())
  if (is.na(ts)) return(as.character(time[[1]]))
  sec <- as.numeric(difftime(Sys.time(), ts, units = "secs"))
  if (is.na(sec) || sec < 0) sec <- 0
  if (sec < 45) return("just now")
  mins <- floor(sec / 60)
  if (mins < 1L) return("just now")
  if (mins < 60) {
    return(paste0(mins, if (mins == 1L) " minute" else " minutes", " ago"))
  }
  hours <- floor(mins / 60)
  rem_mins <- mins %% 60
  if (hours < 24) {
    base <- paste0(hours, if (hours == 1L) " hour" else " hours")
    if (rem_mins == 0L) return(paste(base, "ago"))
    return(paste0(
      base, " and ", rem_mins,
      if (rem_mins == 1L) " minute" else " minutes", " ago"
    ))
  }
  days <- floor(hours / 24)
  rem_hours <- hours %% 24
  if (days < 45) {
    base <- paste0(days, if (days == 1L) " day" else " days")
    if (rem_hours == 0L) return(paste(base, "ago"))
    return(paste0(
      base, " and ", rem_hours,
      if (rem_hours == 1L) " hour" else " hours", " ago"
    ))
  }
  format(ts, "%b %d, %Y")
}

backup_dataset_snapshot <- function(meta_row) {
  if (!use_neon_db()) return(invisible(NULL))
  res <- neon_backup_dataset_snapshot(meta_row)
  if (isTRUE(res$ok)) return(res$ref)
  invisible(NULL)
}

list_dataset_backups <- function(meta_row, limit = 1L) {
  if (!use_neon_db()) return(character())
  if (neon_has_dataset_backup(meta_row)) return(dataset_backup_ref(meta_row))
  character()
}

backup_summary_from_ref <- function(meta_row, ref, preview_rows = 8L) {
  ref <- as.character(ref %||% "")
  if (!use_neon_db() || !grepl("^neon:", ref)) return(NULL)
  neon_backup_summary(meta_row, preview_rows)
}

restore_dataset_from_backup <- function(meta_row, backup_ref) {
  if (!use_neon_db()) {
    return(list(ok = FALSE, error = "Backups require a Neon database connection."))
  }
  ref <- as.character(backup_ref %||% "")
  if (!grepl("^neon:", ref)) {
    return(list(ok = FALSE, error = "Invalid backup reference."))
  }
  neon_restore_dataset_from_backup(meta_row)
}

ensure_df_colnames <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  nms <- as.character(colnames(df))
  empty <- is.na(nms) | !nzchar(trimws(nms))
  if (any(empty)) {
    nms[empty] <- paste0("Column_", seq_along(nms)[empty])
    colnames(df) <- nms
  }
  df
}

file_fingerprint <- function(path) {
  if (!length(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  list(
    mtime = file.info(path)$mtime,
    md5   = unname(tools::md5sum(path))
  )
}

fp_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(a$md5, b$md5)
}

dataset_fingerprint <- function(meta_row) {
  if (use_neon_db()) return(neon_dataset_fingerprint(meta_row))
  file_fingerprint(dataset_path(meta_row))
}

load_dataset <- function(meta_row) {
  if (use_neon_db()) return(neon_load_dataset(meta_row))
  path <- dataset_path(meta_row)
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

load_upload_log <- function() {
  if (use_neon_db()) return(neon_load_upload_log())
  path <- here("data", "upload_log.csv")
  if (!file.exists(path)) return(data.frame(
    timestamp = character(), dataset = character(),
    action    = character(), user    = character(), note = character()
  ))
  read.csv(path, stringsAsFactors = FALSE)
}

# ── Metadata helpers ──────────────────────────────────────────────────────────

update_last_updated <- function(dataset_name, folder = NULL) {
  if (use_neon_db()) {
    neon_update_last_updated(dataset_name, folder)
    return()
  }
  path <- here("data", "metadata", "datasets.csv")
  if (!file.exists(path)) return()
  meta <- read.csv(path, stringsAsFactors = FALSE)
  meta$last_updated[dataset_row_index(meta, dataset_name, folder)] <- as.character(Sys.Date())
  write.csv(meta, path, row.names = FALSE)
}

update_dataset_stats <- function(dataset_name, df, folder = NULL) {
  stats <- compute_dataset_stats(df)
  if (use_neon_db()) {
    neon_update_dataset_stats(
      dataset_name, stats$row_count, stats$year_min, stats$year_max, folder = folder
    )
    return()
  }
  path <- here("data", "metadata", "datasets.csv")
  if (!file.exists(path)) return()
  meta <- read.csv(path, stringsAsFactors = FALSE)
  idx <- dataset_row_index(meta, dataset_name, folder)
  if (!"row_count" %in% names(meta)) meta$row_count <- NA_integer_
  if (!"year_min" %in% names(meta)) meta$year_min <- NA_integer_
  if (!"year_max" %in% names(meta)) meta$year_max <- NA_integer_
  meta$row_count[idx] <- stats$row_count
  meta$year_min[idx] <- stats$year_min
  meta$year_max[idx] <- stats$year_max
  write.csv(meta, path, row.names = FALSE)
}

append_upload_log <- function(dataset, action, note = "", user = NULL) {
  app_user <- user %||% get_app_user()
  if (use_neon_db()) {
    neon_append_upload_log(dataset, action, note, user = app_user)
    return()
  }
  path  <- here("data", "upload_log.csv")
  entry <- data.frame(
    timestamp = as.character(Sys.time()),
    dataset   = dataset,
    action    = action,
    user      = app_user,
    note      = note,
    stringsAsFactors = FALSE
  )
  if (file.exists(path)) {
    existing <- read.csv(path, stringsAsFactors = FALSE)
    # Handle historical column mismatches safely.
    cols <- union(names(existing), names(entry))
    for (nm in setdiff(cols, names(existing))) existing[[nm]] <- ""
    for (nm in setdiff(cols, names(entry)))    entry[[nm]]    <- ""
    existing <- existing[, cols, drop = FALSE]
    entry    <- entry[, cols, drop = FALSE]
    write.csv(rbind(existing, entry), path, row.names = FALSE)
  } else {
    write.csv(entry, path, row.names = FALSE)
  }
}

# ── Save helper ───────────────────────────────────────────────────────────────
# force_replace = TRUE: overwrite entire file
# force_replace = FALSE (default): append-only, skip existing years

atomic_write_csv <- function(df, path, expected_fp = NULL) {
  if (!is.null(expected_fp)) {
    cur <- file_fingerprint(path)
    if (!fp_equal(cur, expected_fp))
      return(list(ok = FALSE, error = "The saved file changed since you opened it. Discard changes and try again."))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  write.csv(df, tmp, row.names = FALSE)
  if (file.exists(path)) file.remove(path)
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      unlink(tmp)
      return(list(ok = FALSE, error = "Could not write CSV file."))
    }
    unlink(tmp)
  }
  list(ok = TRUE, fp = file_fingerprint(path))
}

atomic_write_dataset <- function(df, meta_row, expected_fp = NULL) {
  if (use_neon_db()) return(neon_atomic_write_dataset(df, meta_row, expected_fp))
  atomic_write_csv(df, dataset_path(meta_row), expected_fp = expected_fp)
}

# Compare two data.frames ignoring row.names (editor validation)
df_equal_content <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  if (nrow(a) != nrow(b) || ncol(a) != ncol(b)) return(FALSE)
  if (!identical(names(a), names(b))) return(FALSE)
  isTRUE(all.equal(a, b, check.attributes = FALSE, convert = TRUE))
}

# Drop rows from the end while each is entirely NA or blank text (e.g. unused spare row)
drop_trailing_blank_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  cell_blank <- function(x) {
    if (length(x) != 1L) return(FALSE)
    if (is.na(x)) return(TRUE)
    if (is.character(x)) return(!nzchar(trimws(as.character(x))))
    FALSE
  }
  row_blank <- function(i) {
    r <- df[i, , drop = FALSE]
    all(vapply(seq_along(r), function(j) cell_blank(r[[j]]), logical(1)))
  }
  n <- nrow(df)
  while (n > 0 && row_blank(n)) {
    n <- n - 1L
  }
  df[seq_len(n), , drop = FALSE]
}

# Add mode: hot_to_r includes minSpareRows blanks — strip unused trailing blanks from
# new rows before dirty compare (so a fresh open is not "unsaved").
normalize_add_mode_hot_for_compare <- function(df, n0) {
  if (is.null(df) || nrow(df) <= n0) return(df)
  tail <- df[(n0 + 1):nrow(df), , drop = FALSE]
  tail <- drop_trailing_blank_rows(tail)
  rbind(df[seq_len(n0), , drop = FALSE], tail)
}

save_editor_dataset <- function(df, meta_row, expected_fp, mode, initial_df) {
  if (mode == "add") {
    n0 <- nrow(initial_df)
    if (nrow(df) > n0) {
      tail <- df[(n0 + 1):nrow(df), , drop = FALSE]
      tail <- drop_trailing_blank_rows(tail)
      df <- rbind(df[seq_len(n0), , drop = FALSE], tail)
    }
    if (nrow(df) <= n0)
      return(list(ok = FALSE, error = "Add at least one new row below existing data (fill the blank row at the bottom)."))
    if (!df_equal_content(df[seq_len(n0), , drop = FALSE], initial_df))
      return(list(ok = FALSE, error = "Existing rows changed. Switch to Edit mode to modify history, or discard."))
    df <- rbind(initial_df, df[(n0 + 1):nrow(df), , drop = FALSE])
  }
  res <- atomic_write_dataset(df, meta_row, expected_fp = expected_fp)
  if (!isTRUE(res$ok)) return(res)
  update_last_updated(meta_row$dataset, folder = meta_row$folder)
  update_dataset_stats(meta_row$dataset, df, folder = meta_row$folder)
  append_upload_log(
    meta_row$dataset, "editor_save",
    note = paste(mode, nrow(df), "rows;", Sys.info()[["user"]])
  )
  c(res, list(rows = nrow(df)))
}

save_dataset_csv <- function(df, dataset_name, meta_row, force_replace = FALSE) {
  out_path <- dataset_path(meta_row)

  if (use_neon_db()) {
    if (!force_replace) {
      existing <- neon_load_dataset(meta_row)
      if (!is.null(existing) && nrow(existing) && "Year" %in% names(df) && "Year" %in% names(existing)) {
        new_rows <- df[!df$Year %in% existing$Year, ]
        if (nrow(new_rows) == 0) return(list(status = "skipped", rows = 0))
        df <- rbind(existing, new_rows)
      }
    }
    fp_before <- neon_dataset_fingerprint(meta_row)
    res <- neon_atomic_write_dataset(df, meta_row, expected_fp = fp_before)
    if (!isTRUE(res$ok))
      return(list(status = "error", rows = 0, error = res$error))
    update_last_updated(dataset_name, folder = meta_row$folder)
    update_dataset_stats(dataset_name, df, folder = meta_row$folder)
    append_upload_log(dataset_name,
                      if (force_replace) "manual_replace" else "manual_upload",
                      note = paste(nrow(df), "total rows"))
    return(list(status = "saved", rows = nrow(df)))
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  fp_before <- if (file.exists(out_path)) file_fingerprint(out_path) else NULL
  
  if (!force_replace && file.exists(out_path) && "Year" %in% names(df)) {
    existing <- tryCatch(read.csv(out_path, stringsAsFactors = FALSE),
                         error = function(e) data.frame())
    if ("Year" %in% names(existing)) {
      new_rows <- df[!df$Year %in% existing$Year, ]
      if (nrow(new_rows) == 0) return(list(status = "skipped", rows = 0))
      df <- rbind(existing, new_rows)
    }
  }
  
  res <- atomic_write_csv(df, out_path, expected_fp = fp_before)
  if (!isTRUE(res$ok))
    return(list(status = "error", rows = 0, error = res$error))
  update_last_updated(dataset_name, folder = meta_row$folder)
  update_dataset_stats(dataset_name, df, folder = meta_row$folder)
  append_upload_log(dataset_name,
                    if (force_replace) "manual_replace" else "manual_upload",
                    note = paste(nrow(df), "total rows"))
  list(status = "saved", rows = nrow(df))
}

# Update editable metadata fields for one dataset. Only updates fields supplied.
update_dataset_metadata <- function(dataset_name, fields = list(), folder = NULL) {
  if (use_neon_db()) {
    res <- neon_update_dataset_metadata(dataset_name, fields, folder)
    if (!isTRUE(res$ok)) return(res)
    append_upload_log(dataset_name, "metadata_update",
                      note = paste(names(fields), collapse = ","))
    return(res)
  }
  path <- here("data", "metadata", "datasets.csv")
  if (!file.exists(path))
    return(list(ok = FALSE, error = "metadata/datasets.csv not found"))
  m <- read.csv(path, stringsAsFactors = FALSE)
  idx <- tryCatch(dataset_row_index(m, dataset_name, folder), error = function(e) NULL)
  if (is.null(idx)) return(list(ok = FALSE, error = "Supply a unique folder/filename for this dataset."))
  editable <- c(
    "display_name", "source_type", "source_name", "source_url",
    "contact_name", "contact_email", "how_to_collect"
  )
  for (nm in editable) {
    if (!is.null(fields[[nm]])) {
      if (!nm %in% names(m)) m[[nm]] <- ""
      val <- fields[[nm]]
      if (is.null(val) || length(val) == 0L) val <- ""
      m[[nm]][idx] <- as.character(val)
    }
  }
  if (!"source_type" %in% names(m)) m$source_type <- "public"
  valid_types <- c("public", "contact", "hybrid")
  t <- trimws(as.character(m$source_type[idx]))
  if (!nzchar(t) || !t %in% valid_types) {
    has_url     <- nzchar(trimws(as.character(m$source_url[idx] %||% "")))
    has_contact <- nzchar(trimws(as.character(m$contact_name[idx] %||% ""))) ||
                   nzchar(trimws(as.character(m$contact_email[idx] %||% "")))
    m$source_type[idx] <- if (has_url && has_contact) "hybrid"
                          else if (has_contact)       "contact"
                          else                        "public"
  }
  write.csv(m, path, row.names = FALSE)
  append_upload_log(dataset_name, "metadata_update",
                    note = paste(names(fields), collapse = ","))
  list(ok = TRUE)
}

set_dataset_archived <- function(dataset_name, archived = TRUE, folder = NULL) {
  if (use_neon_db()) return(neon_set_dataset_archived(dataset_name, archived, folder))
  path <- here("data", "metadata", "datasets.csv")
  if (!file.exists(path)) return(FALSE)
  m <- read.csv(path, stringsAsFactors = FALSE)
  idx <- tryCatch(dataset_row_index(m, dataset_name, folder), error = function(e) NULL)
  if (is.null(idx)) return(FALSE)
  if (!"archived" %in% names(m)) m$archived <- ""
  if (isTRUE(archived)) {
    r <- m[idx, , drop = FALSE]
    if (nrow(r) == 1 && "folder" %in% names(r)) {
      src <- here("data", r$folder[[1]], r$dataset[[1]])
      if (file.exists(src)) {
        dst_dir <- here("data", "archive", r$folder[[1]])
        dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
        stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
        dst <- file.path(dst_dir, paste0(r$dataset[[1]], ".", stamp, ".csv"))
        file.copy(src, dst, overwrite = TRUE)
      }
    }
  }
  m$archived[idx] <- if (archived) "yes" else ""
  write.csv(m, path, row.names = FALSE)
  TRUE
}

# Append one metadata row; write CSV file first, then register (caller
# responsibility). Resilient to column drift: fills any column on either side
# with "" so rbind never fails when we add new metadata fields later.
normalize_dataset_filename <- function(file_name) {
  fn <- trimws(file_name)
  if (!nzchar(fn)) return(fn)
  if (!grepl("\\.csv$", fn, ignore.case = TRUE)) fn <- paste0(fn, ".csv")
  fn
}

# Check whether a new dataset filename can be used in a goal folder.
validate_new_dataset_slot <- function(goal, file_name) {
  fn <- normalize_dataset_filename(file_name)
  if (!nzchar(fn)) {
    return(list(ok = FALSE, error = "Display name and filename are required."))
  }
  folder <- goal_to_folder(goal)
  path   <- here("data", folder, fn)
  m <- load_meta()
  rows <- if (nrow(m)) m[m$dataset == fn & m$folder == folder, , drop = FALSE] else m
  active <- if (nrow(rows)) rows[!is_archived_flag(rows$archived), , drop = FALSE] else rows
  if (nrow(active) > 0) {
    return(list(
      ok = FALSE,
      error = "An active dataset with this filename already exists in this goal."
    ))
  }
  replace_archived <- nrow(rows) > 0
  if (use_neon_db()) {
    if (neon_dataset_registered(folder, fn, active_only = TRUE)) {
      return(list(
        ok = FALSE,
        error = "An active dataset with this filename already exists in this goal."
      ))
    }
    replace_archived <- neon_dataset_registered(folder, fn, active_only = FALSE)
  } else if (file.exists(path) && !replace_archived) {
    return(list(ok = FALSE, error = "That file already exists on disk."))
  }
  list(ok = TRUE, folder = folder, file_name = fn, path = path,
       replace_archived = replace_archived)
}

register_dataset_metadata <- function(row, replace_existing = FALSE) {
  if (use_neon_db()) return(neon_register_dataset_metadata(row, replace_existing))
  path <- here("data", "metadata", "datasets.csv")
  m <- if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else data.frame()
  all_cols <- union(names(m), names(row))
  for (nm in setdiff(all_cols, names(m)))   m[[nm]]   <- ""
  for (nm in setdiff(all_cols, names(row))) row[[nm]] <- ""
  m   <- if (nrow(m)) m[, all_cols, drop = FALSE] else m
  row <- row[, all_cols, drop = FALSE]
  if (isTRUE(replace_existing) && nrow(m)) {
    idx <- which(m$dataset == row$dataset & m$folder == row$folder)
    if (length(idx)) {
      for (nm in names(row)) m[[nm]][idx] <- row[[nm]]
      write.csv(m, path, row.names = FALSE)
      return(list(ok = TRUE))
    }
  }
  if (nrow(m) == 0) {
    write.csv(row, path, row.names = FALSE)
    return(list(ok = TRUE))
  }
  m <- rbind(m, row)
  write.csv(m, path, row.names = FALSE)
  list(ok = TRUE)
}

# Report Builder: caption text for exports
# Create empty CSV + register row (after validation). Returns list(ok=, error=)
create_dataset_from_wizard <- function(goal, display_name, file_name, source_type,
                                       source_name, how,
                                       source_url    = "",
                                       contact_name  = "",
                                       contact_email = "",
                                       use_custom_headers = FALSE,
                                       headers_csv = "Year,Community,Value") {
  dn <- trimws(display_name)
  if (!nzchar(dn))
    return(list(ok = FALSE, error = "Display name and filename are required."))
  slot <- validate_new_dataset_slot(goal, file_name)
  if (!isTRUE(slot$ok)) return(list(ok = FALSE, error = slot$error))
  fn <- slot$file_name
  folder <- slot$folder
  path   <- slot$path
  hdrs <- if (isTRUE(use_custom_headers) && has_val(headers_csv)) {
    trimws(strsplit(headers_csv, ",", fixed = TRUE)[[1]])
  } else {
    c("Year", "Value")
  }
  hdrs <- hdrs[nzchar(hdrs)]
  if (length(hdrs) == 0)
    return(list(ok = FALSE, error = "Provide at least one column."))
  # Use character columns for empty templates (logical columns render as checkboxes in editor).
  blank <- as.data.frame(lapply(hdrs, function(.) character(0)), stringsAsFactors = FALSE)
  names(blank) <- hdrs
  stats <- compute_dataset_stats(blank)
  if (use_neon_db()) {
    res <- neon_create_empty_dataset(goal, folder, fn, hdrs)
    if (!isTRUE(res$ok)) return(list(ok = FALSE, error = "Could not create dataset table."))
  } else {
    res <- atomic_write_csv(blank, path, expected_fp = NULL)
    if (!isTRUE(res$ok))
      return(list(ok = FALSE, error = res$error %||% "Write failed"))
  }
  row <- data.frame(
    dataset        = fn,
    goal           = as.character(goal),
    folder         = folder,
    source_type    = source_type %||% "public",
    display_name   = dn,
    source_name    = source_name %||% "",
    source_url     = source_url %||% "",
    contact_name   = contact_name %||% "",
    contact_email  = contact_email %||% "",
    how_to_collect = how %||% "",
    last_updated   = as.character(Sys.Date()),
    archived       = "",
    fetcher        = "",
    row_count      = stats$row_count,
    year_min       = stats$year_min,
    year_max       = stats$year_max,
    stringsAsFactors = FALSE
  )
  reg <- register_dataset_metadata(row, replace_existing = isTRUE(slot$replace_archived))
  if (!isTRUE(reg$ok)) {
    if (use_neon_db()) neon_drop_dataset_table(folder, fn)
    else if (file.exists(path)) unlink(path)
    return(list(ok = FALSE, error = reg$error %||% "Could not register dataset metadata."))
  }
  append_upload_log(fn, "upload", note = "wizard empty dataset")
  list(ok = TRUE, path = path)
}

# Create dataset from an uploaded CSV and register metadata.
create_dataset_from_uploaded_csv <- function(goal, display_name, file_name, source_type,
                                             source_name, how, df,
                                             source_url    = "",
                                             contact_name  = "",
                                             contact_email = "") {
  dn <- trimws(display_name)
  if (!nzchar(dn))
    return(list(ok = FALSE, error = "Display name and filename are required."))
  slot <- validate_new_dataset_slot(goal, file_name)
  if (!isTRUE(slot$ok)) return(list(ok = FALSE, error = slot$error))
  fn <- slot$file_name
  folder <- slot$folder
  path   <- slot$path
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0)
    return(list(ok = FALSE, error = "Uploaded CSV is empty or invalid."))
  stats <- compute_dataset_stats(df)
  if (use_neon_db()) {
    loc <- list(schema = folder, table = csv_to_table_name(fn))
    con <- get_db_con()
    neon_write_table_data(con, loc$schema, loc$table, df)
  } else {
    res <- atomic_write_csv(df, path, expected_fp = NULL)
    if (!isTRUE(res$ok))
      return(list(ok = FALSE, error = res$error %||% "Write failed"))
  }
  row <- data.frame(
    dataset        = fn,
    goal           = as.character(goal),
    folder         = folder,
    source_type    = source_type %||% "public",
    display_name   = dn,
    source_name    = source_name %||% "",
    source_url     = source_url %||% "",
    contact_name   = contact_name %||% "",
    contact_email  = contact_email %||% "",
    how_to_collect = how %||% "",
    last_updated   = as.character(Sys.Date()),
    archived       = "",
    fetcher        = "",
    row_count      = stats$row_count,
    year_min       = stats$year_min,
    year_max       = stats$year_max,
    stringsAsFactors = FALSE
  )
  reg <- register_dataset_metadata(row, replace_existing = isTRUE(slot$replace_archived))
  if (!isTRUE(reg$ok)) {
    if (use_neon_db()) neon_drop_dataset_table(folder, fn)
    else if (file.exists(path)) unlink(path)
    return(list(ok = FALSE, error = reg$error %||% "Could not register dataset metadata."))
  }
  append_upload_log(fn, "upload", note = "wizard uploaded csv")
  list(ok = TRUE, path = path)
}

build_report_caption <- function(meta_row, df, extra = character()) {
  lines <- c(
    paste0("Dataset: ", meta_row$display_name %||% meta_row$dataset),
    if (has_val(meta_row$source_name))
      paste0("Source: ", meta_row$source_name) else NULL
  )
  if (!is.null(df) && "Year" %in% names(df)) {
    rng <- suppressWarnings(range(df$Year, na.rm = TRUE))
    if (all(is.finite(rng)))
      lines <- c(lines, paste0("Year range in view: ", rng[1], "\u2013", rng[2]))
  }
  if (length(extra)) lines <- c(lines, extra)
  paste(lines, collapse = "\n")
}

# ── UI helpers ────────────────────────────────────────────────────────────────

freshness_badge <- function(last_updated) {
  if (!has_val(last_updated))
    return(tags$span(class = "badge bg-secondary", "Never updated"))
  d <- tryCatch(as.Date(last_updated), error = function(e) NA)
  if (is.na(d)) return(tags$span(class = "badge bg-secondary", "Unknown"))
  age <- as.numeric(Sys.Date() - d)
  if (age <= STALE_DAYS)
    tags$span(class = "badge bg-success", paste("Updated", format(d, "%b %Y")))
  else
    tags$span(class = "badge bg-warning text-dark", paste("Stale since", format(d, "%b %Y")))
}