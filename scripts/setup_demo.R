#!/usr/bin/env Rscript
# Run from the repository root. Never overwrite an existing data directory.
if (!file.exists("app.R") || !dir.exists("examples/demo-data")) {
  stop("Run this script from the repository root.")
}
if (dir.exists("data")) {
  stop("data/ already exists. Back it up or move it before creating the demo.")
}
if (!dir.create("data")) stop("Could not create data/.")
entries <- list.files("examples/demo-data", full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
if (!all(file.copy(entries, "data", recursive = TRUE))) {
  stop("Some demo files could not be copied; check data/ before continuing.")
}
message("Synthetic demo data copied to ignored data/. No real Geneva data is included.")
message("Set SFGC_DEMO_MODE=true and your own SFGC_DEMO_*_PASSWORD values (8+ characters).")
message("Leave DATABASE_URL unset, then run shiny::runApp('.').")
