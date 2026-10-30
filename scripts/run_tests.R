#!/usr/bin/env Rscript
# Run auth and helper tests.
# Usage: Rscript scripts/run_tests.R
# Requires DYLD_LIBRARY_PATH for RPostgres on macOS Homebrew libpq if using Neon.

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(".", winslash = "/")
if (!file.exists(file.path(root, "internal", "modules", "helpers.R"))) {
  root <- normalizePath(file.path(root, ".."), winslash = "/")
}
setwd(root)

if (file.exists(".Renviron")) readRenviron(".Renviron")

# Tests may mutate users and data: never inherit the production database.
Sys.setenv(DATABASE_URL = Sys.getenv("TEST_DATABASE_URL", unset = ""))

if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
  Sys.setenv(DYLD_LIBRARY_PATH = paste(
    c("/opt/homebrew/opt/libpq/lib", Sys.getenv("DYLD_LIBRARY_PATH", unset = "")),
    collapse = .Platform$path.sep
  ))
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  message("Installing testthat for this project...")
  renv::install("testthat")
}

if (!requireNamespace("sodium", quietly = TRUE)) {
  message("Installing sodium for auth tests...")
  renv::install("sodium")
}

suppressPackageStartupMessages(library(testthat))

cat("=== SFGC Dashboard tests ===\n")
cat("Neon:", if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) "yes" else "no (Neon tests skipped)\n")

res <- test_dir(
  file.path(root, "tests", "testthat"),
  reporter = "summary",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
)

result_table <- as.data.frame(res)
cat(sprintf("Test cases: %d | Passed expectations: %d | Skipped cases: %d\n",
            nrow(result_table), sum(result_table$passed, na.rm = TRUE),
            sum(result_table$skipped, na.rm = TRUE)))

failed <- sum(result_table$failed, na.rm = TRUE)
errors <- sum(as.data.frame(res)$error, na.rm = TRUE)
warnings <- sum(as.data.frame(res)$warning, na.rm = TRUE)

cat("\n=== Summary ===\n")
cat(sprintf("Failed: %d | Errors: %d | Warnings: %d\n", failed, errors, warnings))

if (failed > 0 || errors > 0) {
  quit(status = 1)
}

cat("All tests passed.\n")
