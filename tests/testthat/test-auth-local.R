test_that("local demo login accepts valid credentials", {
  withr::local_envvar(c(DATABASE_URL = ""))
  row <- local_auth_verify("admin", "admin123")
  expect_false(is.null(row))
  expect_equal(row$username, "admin")
  expect_true(row$is_admin)

  row2 <- local_auth_verify("viewer", "viewer123")
  expect_false(is.null(row2))
  expect_false(row2$is_admin)
})

test_that("local demo login rejects invalid credentials", {
  withr::local_envvar(c(DATABASE_URL = ""))
  expect_null(local_auth_verify("admin", "wrong"))
  expect_null(local_auth_verify("nobody", "admin123"))
})

test_that("auth_check_credentials wraps local verify", {
  withr::local_envvar(c(DATABASE_URL = ""))
  row <- local_auth_verify("admin", "admin123")
  expect_false(is.null(row))
  ok <- auth_check_credentials("admin", "admin123")
  if (has_neon()) {
    skip_if_not(isTRUE(ok$result), "Set TEST_ADMIN_PASSWORD if admin password was changed")
  } else {
    expect_true(ok$result)
    expect_equal(ok$user_info$user, "admin")
    expect_true(ok$user_info$admin)
  }

  bad <- auth_check_credentials("admin", "nope")
  expect_false(bad$result)
})

test_that("set_app_user overrides get_app_user", {
  withr::local_envvar(c(DATABASE_URL = ""))
  old <- get_app_user()
  set_app_user("dashboard_tester")
  expect_equal(get_app_user(), "dashboard_tester")
  set_app_user("")
  expect_equal(get_app_user(), old)
})

test_that("auth_create_user requires Neon", {
  withr::local_envvar(c(DATABASE_URL = ""))
  if (has_neon()) skip("Neon tests cover user creation")
  res <- auth_create_user("x", "longpass1", "X", FALSE)
  expect_false(res$ok)
  expect_match(res$error, "Neon")
})
