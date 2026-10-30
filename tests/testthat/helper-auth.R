# Shared setup for testthat auth tests.
Sys.setenv(SFGC_DEMO_MODE = "true", SFGC_DEMO_ADMIN_PASSWORD = "admin123",
           SFGC_DEMO_EDITOR_PASSWORD = "editor123", SFGC_DEMO_VIEWER_PASSWORD = "viewer123")
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Tests may mutate users and data: never inherit the production database.
Sys.setenv(DATABASE_URL = Sys.getenv("TEST_DATABASE_URL", unset = ""))

suppressPackageStartupMessages({
  library(here)
})

source(here("internal", "modules", "helpers.R"), local = FALSE)

has_neon <- function() {
  nzchar(Sys.getenv("DATABASE_URL", unset = ""))
}

test_admin_password <- function() {
  Sys.getenv("TEST_ADMIN_PASSWORD", unset = Sys.getenv("BOOTSTRAP_ADMIN_PASSWORD", unset = ""))
}

unique_test_username <- function(prefix = "testuser") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(999, 1))
}

if (has_neon()) {
  if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
    Sys.setenv(DYLD_LIBRARY_PATH = paste(
      c("/opt/homebrew/opt/libpq/lib", Sys.getenv("DYLD_LIBRARY_PATH", unset = "")),
      collapse = .Platform$path.sep
    ))
  }
  con <- get_db_con()
  neon_ensure_users_table(con)
  neon_seed_admin_user(con)
}
