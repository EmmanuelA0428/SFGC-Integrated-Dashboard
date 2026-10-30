# Run with: Rscript scripts/run_tests.R  (preferred)
# Or from project root: Rscript -e 'testthat::test_dir("tests/testthat")'

if (file.exists(".Renviron")) readRenviron(".Renviron")

# Tests may mutate users and data: never inherit the production database.
Sys.setenv(DATABASE_URL = Sys.getenv("TEST_DATABASE_URL", unset = ""))

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat: renv::install('testthat')")
}

testthat::test_dir("tests/testthat", reporter = "summary")
