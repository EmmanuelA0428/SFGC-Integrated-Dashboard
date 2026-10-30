test_that("Neon user lifecycle", {
  skip_if_not(has_neon(), "DATABASE_URL not set")

  admin_pwd <- test_admin_password()
  admin_ok <- auth_check_credentials("admin", admin_pwd)
  skip_if_not(isTRUE(admin_ok$result), "Admin login failed — set TEST_ADMIN_PASSWORD in .Renviron")

  uname <- unique_test_username("neon")
  pwd <- "TestPass8!"
  new_pwd <- "NewTestPass9!"

  create <- auth_create_user(uname, pwd, "Neon Test User", is_admin = FALSE)
  expect_true(create$ok)

  dup <- auth_create_user(uname, pwd, "Dup", FALSE)
  expect_false(dup$ok)

  short <- auth_create_user(paste0(uname, "2"), "short", "Short", FALSE)
  expect_false(short$ok)

  login <- auth_check_credentials(uname, pwd)
  expect_true(login$result)
  expect_equal(login$user_info$user, uname)

  bad_login <- auth_check_credentials(uname, "wrongpassword")
  expect_false(bad_login$result)

  chg <- auth_change_password(uname, new_pwd, current_password = pwd)
  expect_true(chg$ok)

  login2 <- auth_check_credentials(uname, new_pwd)
  expect_true(login2$result)

  wrong_cur <- auth_change_password(uname, "AnotherPass10!", current_password = "wrong")
  expect_false(wrong_cur$ok)

  dn <- auth_update_display_name(uname, "Updated Name")
  expect_true(dn$ok)

  self_deact <- auth_deactivate_user("admin", "admin")
  expect_false(self_deact$ok)

  deact <- auth_deactivate_user(uname, "admin")
  expect_true(deact$ok)

  after <- auth_check_credentials(uname, new_pwd)
  expect_false(after$result)
})

test_that("append_upload_log records set_app_user on Neon", {
  skip_if_not(has_neon(), "DATABASE_URL not set")
  skip_if_not(isTRUE(auth_check_credentials("admin", test_admin_password())$result),
              "Admin login required for log test")

  set_app_user("log_test_user")
  before <- nrow(neon_load_upload_log())
  append_upload_log("", "account", note = "auth test entry")
  log <- neon_load_upload_log()
  expect_gt(nrow(log), before)
  last <- log[nrow(log), ]
  expect_equal(last$user, "log_test_user")
  expect_equal(last$action, "account")
})
