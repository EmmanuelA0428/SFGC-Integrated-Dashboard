# Standalone auth smoke tests (no testthat required).
# Usage: Rscript scripts/test_auth.R

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

# Explicit local-only test fixtures; never used with a configured database.
if (!nzchar(Sys.getenv("DATABASE_URL"))) {
  Sys.setenv(SFGC_DEMO_MODE = "true", SFGC_DEMO_ADMIN_PASSWORD = "admin123",
             SFGC_DEMO_VIEWER_PASSWORD = "viewer123")
}
source(file.path(root, "internal", "modules", "helpers.R"), local = FALSE)

pass <- 0L
fail <- 0L

check <- function(name, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    cat("FAIL:", name, " — ", conditionMessage(e), "\n")
    FALSE
  })
  if (ok) {
    pass <<- pass + 1L
    cat("PASS:", name, "\n")
  } else if (!ok) {
    fail <<- fail + 1L
    if (!inherits(ok, "error")) cat("FAIL:", name, "\n")
  }
}

cat("=== Auth smoke tests ===\n")

check("local admin login", !is.null(local_auth_verify("admin", "admin123")))
check("local bad password", is.null(local_auth_verify("admin", "wrong")))
check("shinymanager shape", {
  if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
    isTRUE(auth_check_credentials("admin", Sys.getenv("TEST_ADMIN_PASSWORD", unset = Sys.getenv("BOOTSTRAP_ADMIN_PASSWORD", unset = "")))$result)
  } else {
    isTRUE(auth_check_credentials("viewer", "viewer123")$result)
  }
})

set_app_user("smoke_user")
check("get_app_user", identical(get_app_user(), "smoke_user"))

if (nzchar(Sys.getenv("DATABASE_URL", unset = ""))) {
  cat("--- Neon ---\n")
  neon_ensure_users_table(get_db_con())
  admin_pwd <- Sys.getenv("BOOTSTRAP_ADMIN_PASSWORD", unset = "")
  check("neon admin login", isTRUE(auth_check_credentials("admin", admin_pwd)$result))

  uname <- paste0("smoke_", format(Sys.time(), "%Y%m%d%H%M%S"))
  pwd <- "SmokeTest8!"
  cr <- auth_create_user(uname, pwd, "Smoke", FALSE)
  check("create user", isTRUE(cr$ok))
  check("login new user", isTRUE(auth_check_credentials(uname, pwd)$result))
  check("deactivate user", isTRUE(auth_deactivate_user(uname, "admin")$ok))
  check("login after deact", !isTRUE(auth_check_credentials(uname, pwd)$result))
} else {
  cat("Neon tests skipped (no DATABASE_URL)\n")
}

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
