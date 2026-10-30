# Backfill row_count, year_min, year_max in app.datasets and local datasets.csv
# Usage: Rscript scripts/backfill_meta_stats.R

if (file.exists(".Renviron")) readRenviron(".Renviron")

source("internal/modules/helpers.R")

PROJECT_ROOT <- normalizePath(".", winslash = "/")
META_PATH    <- file.path(PROJECT_ROOT, "data", "metadata", "datasets.csv")

backfill_local_csv <- function() {
  if (!file.exists(META_PATH)) return()
  m <- read.csv(META_PATH, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(m))) {
    path <- file.path(PROJECT_ROOT, "data", m$folder[i], m$dataset[i])
    df <- if (file.exists(path)) {
      tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    } else NULL
    stats <- compute_dataset_stats(df)
    m$row_count[i] <- stats$row_count
    m$year_min[i]  <- stats$year_min
    m$year_max[i]  <- stats$year_max
  }
  write.csv(m, META_PATH, row.names = FALSE)
  cat("Updated local datasets.csv\n")
}

backfill_neon <- function() {
  if (!use_neon_db()) {
    cat("DATABASE_URL not set — skipping Neon backfill\n")
    return()
  }
  neon_ensure_registry_columns(get_db_con())
  m <- neon_load_meta()
  for (i in seq_len(nrow(m))) {
    r <- m[i, ]
    df <- neon_load_dataset(r)
    stats <- compute_dataset_stats(df)
    neon_update_dataset_stats(r$dataset, stats$row_count, stats$year_min, stats$year_max, folder = r$folder)
    cat(sprintf("  %s/%s: %d rows, years %s-%s\n",
                r$folder, r$dataset, stats$row_count,
                stats$year_min %||% "?", stats$year_max %||% "?"))
  }
  cat("Neon app.datasets stats updated\n")
}

backfill_local_csv()
backfill_neon()
if (use_neon_db()) db_disconnect_all()
