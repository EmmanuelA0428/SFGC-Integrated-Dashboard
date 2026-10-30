# run_pipeline.R
# Master pipeline orchestrator for SFG Dashboard
# Run annually to refresh all Track A (auto) datasets.
# Usage: source("pipeline/run_pipeline.R")

library(here)

# ── Configuration ─────────────────────────────────────────────────────────────

# Set the year you are fetching data for
FETCH_YEAR <- 2024

# Census API key — get one at: https://api.census.gov/data/key_signup.html
# Set it once per machine; tidycensus will remember it.
# census_api_key("YOUR_KEY_HERE", install = TRUE)

# ── Paths ─────────────────────────────────────────────────────────────────────

DATA_DIR   <- here("data")
LOG_FILE   <- file.path(DATA_DIR, "upload_log.csv")
SCRIPT_DIR <- here("pipeline")

# ── Helpers ───────────────────────────────────────────────────────────────────

log_run <- function(dataset, status, note = "") {
  entry <- data.frame(
    dataset   = dataset,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    uploader  = "pipeline",
    status    = status,
    note      = note,
    stringsAsFactors = FALSE
  )
  if (file.exists(LOG_FILE)) {
    existing <- read.csv(LOG_FILE, stringsAsFactors = FALSE)
    write.csv(rbind(existing, entry), LOG_FILE, row.names = FALSE)
  } else {
    write.csv(entry, LOG_FILE, row.names = FALSE)
  }
}

update_last_updated <- function(dataset_name) {
  meta_path <- here("data", "metadata", "datasets.csv")
  meta <- read.csv(meta_path, stringsAsFactors = FALSE)
  meta$last_updated[meta$dataset == dataset_name] <- Sys.Date()
  write.csv(meta, meta_path, row.names = FALSE)
}

safe_run <- function(script_name, label) {
  message("\n── Running ", label, " ─────────────────────")
  tryCatch({
    source(file.path(SCRIPT_DIR, script_name))
    message("✓ ", label, " completed")
    log_run(label, "success")
  }, error = function(e) {
    message("✗ ", label, " FAILED: ", conditionMessage(e))
    log_run(label, "error", conditionMessage(e))
  })
}

# ── Run fetch scripts ──────────────────────────────────────────────────────────

message("=== SFG Pipeline | Year: ", FETCH_YEAR, " | ", Sys.time(), " ===")

safe_run("fetch_census.R",      "Census ACS")
safe_run("fetch_nysdoh.R",      "NYSDOH Vital Statistics")
safe_run("fetch_nysdoh_chac.R", "NYSDOH CHAC Perinatal")
safe_run("fetch_nysed.R",       "NYSED Enrollment & Assessments")

message("\n=== Pipeline complete ===")
message("Log written to: ", LOG_FILE)
