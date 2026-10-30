test_that("web identities and permissions are isolated per session", {
  viewer <- shiny::MockShinySession$new()
  admin <- shiny::MockShinySession$new()
  withr::defer(viewer$close())
  withr::defer(admin$close())
  read_in <- function(session, fn) {
    shiny::withReactiveDomain(session, shiny::isolate(fn()))
  }
  shiny::withReactiveDomain(viewer, set_app_user("viewer_a", role = "viewer"))
  shiny::withReactiveDomain(admin, set_app_user("admin_b", role = "admin"))
  expect_identical(read_in(viewer, get_app_user), "viewer_a")
  expect_identical(read_in(viewer, get_app_role), "viewer")
  expect_false(read_in(viewer, get_app_can_edit))
  expect_false(read_in(viewer, get_app_is_admin))
  expect_identical(read_in(admin, get_app_user), "admin_b")
  expect_true(read_in(admin, get_app_is_admin))
  shiny::withReactiveDomain(admin, set_app_user("", role = "viewer"))
  expect_identical(read_in(admin, get_app_user), "")
  expect_false(read_in(admin, get_app_can_edit))
  expect_identical(read_in(viewer, get_app_user), "viewer_a")
})

test_that("permission reactives update on login and logout", {
  shiny::testServer(function(input, output, session) {
    shiny::observeEvent(input$identity, {
      set_app_user(input$identity, role = input$role)
    })
    permission <- shiny::reactive(get_app_can_edit())
    output$permission <- shiny::renderText(as.character(permission()))
  }, {
    expect_identical(output$permission, "FALSE")
    session$setInputs(identity = "editor_a", role = "editor")
    expect_identical(output$permission, "TRUE")
    session$setInputs(identity = "", role = "viewer")
    expect_identical(output$permission, "FALSE")
  })
})

test_that("demo authentication requires explicit local-only configuration", {
  withr::local_envvar(c(DATABASE_URL = "", SFGC_DEMO_MODE = "false",
                         SFGC_DEMO_ADMIN_PASSWORD = Sys.getenv("SFGC_DEMO_ADMIN_PASSWORD")))
  expect_null(local_auth_verify("admin", "admin123"))
  Sys.setenv(SFGC_DEMO_MODE = "true", SFGC_DEMO_ADMIN_PASSWORD = "")
  expect_null(local_auth_verify("admin", "admin123"))
  Sys.setenv(SFGC_DEMO_ADMIN_PASSWORD = "LocalTestPassword123!")
  expect_false(is.null(local_auth_verify("admin", "LocalTestPassword123!")))
  Sys.setenv(DATABASE_URL = "postgresql://unused:unused@127.0.0.1/unused")
  expect_null(local_auth_verify("admin", "LocalTestPassword123!"))
})

test_that("unknown roles fail closed", {
  expect_false(auth_can_edit("unknown"))
  expect_false(auth_can_edit(NULL))
  expect_false(auth_can_admin("editor"))
})