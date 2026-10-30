test_that("PostgreSQL dataset mutations isolate duplicate filenames", {
  skip_if_not(has_neon(), "TEST_DATABASE_URL not set")
  con <- get_db_con()
  # Transactions keep even a disposable test database tidy after this check.
  DBI::dbBegin(con)
  withr::defer(DBI::dbRollback(con))
  neon_ensure_registry_columns(con)
  folders <- paste0("qa_identity_", c("a", "b"), "_", sample.int(999999L, 1L))
  for (f in folders) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO app.datasets (dataset, folder, pg_schema, pg_table, ",
      "display_name, last_updated, archived, row_count) VALUES ",
      "('Income.csv', $1, $1, 'income', 'Synthetic QA', '2020-01-01', '', 2)"),
      params = list(f))
  }
  key <- paste0(folders[1], "/Income.csv")
  rows <- function() DBI::dbGetQuery(con,
    "SELECT * FROM app.datasets WHERE folder IN ($1,$2) ORDER BY folder",
    params = as.list(folders))
  expect_true(neon_update_dataset_metadata(key, list(display_name = "Only A"))$ok)
  expect_identical(rows()$display_name, c("Only A", "Synthetic QA"))
  neon_update_last_updated(key)
  expect_equal(as.character(rows()$last_updated), c(as.character(Sys.Date()), "2020-01-01"))
  neon_update_dataset_stats(key, 7L, 2022L, 2025L)
  expect_equal(rows()$row_count, c(7L, 2L))
  expect_true(neon_set_dataset_archived(key, TRUE))
  expect_identical(rows()$archived, c("yes", ""))
  expect_true(neon_set_dataset_archived(key, FALSE))
  expect_identical(rows()$archived, c("", ""))
  expect_error(neon_update_last_updated("Income.csv"), "ambiguous")
})
