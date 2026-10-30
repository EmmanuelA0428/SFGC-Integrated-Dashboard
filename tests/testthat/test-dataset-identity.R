local_fixture <- function() {
  root <- tempfile("sfgc-fixture-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  io <- new.env(parent = globalenv())
  sys.source(here::here("internal", "modules", "helpers.R"), envir = io)
  io$here <- function(...) file.path(root, ...)
  io$use_neon_db <- function() FALSE
  metadata <- data.frame(
    dataset = c("Income.csv", "Income.csv"), goal = c("3", "background"),
    folder = c("goal3", "background"),
    display_name = c("Synthetic A", "Synthetic B"),
    source_type = "public", source_name = "Synthetic QA data",
    source_url = "", contact_name = "", contact_email = "",
    how_to_collect = "", last_updated = "2020-01-01", archived = "",
    row_count = 2L, year_min = 2024L, year_max = 2025L,
    stringsAsFactors = FALSE
  )
  write.csv(metadata, io$here("data", "metadata", "datasets.csv"), row.names = FALSE)
  for (folder in metadata$folder) {
    dir.create(io$here("data", folder))
    write.csv(data.frame(Year = 2024:2025, Value = c(10, 20)),
              io$here("data", folder, "Income.csv"), row.names = FALSE)
  }
  list(root = root, io = io)
}

test_that("replacement updates only the targeted folder and its statistics", {
  fixture <- local_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  io <- fixture$io
  row <- io$load_meta()[1, , drop = FALSE]
  replacement <- data.frame(Year = 2023:2025, Value = c(1, 2, 3))
  res <- io$save_dataset_csv(replacement, "Income.csv", row, force_replace = TRUE)
  expect_identical(res$status, "saved")
  m <- io$load_meta()
  expect_identical(m$last_updated[m$folder == "background"], "2020-01-01")
  expect_equal(m$row_count[m$folder == "background"], 2L)
  expect_equal(m$row_count[m$folder == "goal3"], 3L)
  expect_equal(nrow(io$load_dataset(m[2, , drop = FALSE])), 2L)
})

test_that("metadata edits and archive use composite identifiers", {
  fixture <- local_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  io <- fixture$io
  expect_true(io$update_dataset_metadata("goal3/Income.csv",
               list(display_name = "Only A changes"))$ok)
  expect_identical(io$load_meta()$display_name, c("Only A changes", "Synthetic B"))
  expect_true(io$set_dataset_archived("goal3/Income.csv", TRUE))
  expect_identical(io$is_archived_flag(io$load_meta()$archived), c(TRUE, FALSE))
  expect_true(io$set_dataset_archived("goal3/Income.csv", FALSE))
  expect_false(any(io$is_archived_flag(io$load_meta()$archived)))
  before <- io$load_meta()
  expect_error(io$update_last_updated("Income.csv"), "ambiguous")
  expect_false(io$update_dataset_metadata("Income.csv", list(display_name = "Wrong"))$ok)
  expect_false(io$set_dataset_archived("Income.csv", TRUE))
  expect_identical(io$load_meta(), before)
})

test_that("SQL predicates include folder and filename with proper escaping", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  clause <- neon_dataset_where(con, "goal3/Income.csv")
  expect_match(clause, "dataset = 'Income.csv'", fixed = TRUE)
  expect_match(clause, "folder = 'goal3'", fixed = TRUE)
  escaped <- neon_dataset_where(con, "x'y.csv", folder = "goal3")
  expect_match(escaped, "'x''y.csv'", fixed = TRUE)
})