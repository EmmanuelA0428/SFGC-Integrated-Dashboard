# Backward-compatible shim — canonical helpers live in internal/modules/db_io.R
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required to load db_io.R")
}
source(here::here("internal", "modules", "db_io.R"))
