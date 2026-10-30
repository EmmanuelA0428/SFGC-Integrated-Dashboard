# fetch_nysed.R
# Fetches NYSED enrollment, graduation, and voting results for GCSD.
# Requires: rvest, dplyr, jsonlite, here
#
# NYSED institution IDs:
#   GCSD district:  800000040585
#   GHS August cohort: 800000040589

library(rvest)
library(dplyr)
library(jsonlite)
library(here)

library(httr)
httr::set_config(httr::config(ssl_verifypeer = FALSE))

if (!exists("FETCH_YEAR")) FETCH_YEAR <- 2024

GCSD_INSTID   <- "800000040585"
GHS_AUG_ID    <- "800000040589"

# ── Helpers ───────────────────────────────────────────────────────────────────

save_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  write.csv(df, path, row.names = FALSE)
  dataset_name <- basename(path)
  meta_path <- here("data", "metadata", "datasets.csv")
  if (file.exists(meta_path)) {
    meta <- read.csv(meta_path, stringsAsFactors = FALSE)
    meta$last_updated[meta$dataset == dataset_name] <- as.character(Sys.Date())
    write.csv(meta, meta_path, row.names = FALSE)
  }
  message("  Saved: ", path)
}

read_nysed_table <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) { warning("Could not fetch: ", url); return(NULL) }
  tables <- html_table(page, fill = TRUE)
  if (length(tables) == 0) return(NULL)
  tables[[which.max(sapply(tables, nrow))]]
}

# ── K-12 Enrollment ───────────────────────────────────────────────────────────

enroll_url <- sprintf(
  "https://data.nysed.gov/enrollment.php?year=%d&instid=%s",
  FETCH_YEAR, GCSD_INSTID
)

enroll_tbl <- read_nysed_table(enroll_url)

if (!is.null(enroll_tbl)) {
  # NYSED enrollment page: first table row after header = totals
  # Columns vary; look for "Total" row and K-12 count
  # Typical structure: Grade | Male | Female | Total
  # Sum all grade rows, or find the "Total" row
  total_row <- enroll_tbl[grepl("total", enroll_tbl[[1]], ignore.case = TRUE), ]
  
  enroll_new <- data.frame(
    Year         = sprintf("%d-%02d", FETCH_YEAR - 1, FETCH_YEAR %% 100),
    Source       = enroll_url,
    K12Enrollment = as.numeric(gsub(",", "", total_row[[ncol(total_row)]][1])),
    Yr           = FETCH_YEAR
  )
  
  enroll_path <- here("data", "goal8", "K12Enrollment.csv")
  if (file.exists(enroll_path)) {
    existing <- read.csv(enroll_path, stringsAsFactors = FALSE)
    if (!FETCH_YEAR %in% existing$Yr) save_csv(rbind(existing, enroll_new), enroll_path)
  } else {
    save_csv(enroll_new, enroll_path)
  }
}

# ── Economically Disadvantaged ────────────────────────────────────────────────
# Same enrollment page — look for "Economically Disadvantaged" rows

if (!is.null(enroll_tbl)) {
  disadv_row <- enroll_tbl[grepl("econom", enroll_tbl[[1]], ignore.case = TRUE), ]
  
  if (nrow(disadv_row) > 0) {
    econ_new <- data.frame(
      Year          = sprintf("%d-%02d", FETCH_YEAR - 1, FETCH_YEAR %% 100),
      Source        = enroll_url,
      Disadvantaged = as.numeric(gsub(",", "", disadv_row[[ncol(disadv_row)]][1])),
      Not           = NA,  # compute from total - disadvantaged if total row present
      Total         = as.numeric(gsub(",", "", total_row[[ncol(total_row)]][1])),
      Proportion    = NA
    )
    econ_new$Not        <- econ_new$Total - econ_new$Disadvantaged
    econ_new$Proportion <- round(econ_new$Disadvantaged / econ_new$Total, 4)
    
    econ_path <- here("data", "goal8", "EconDisadv.csv")
    if (file.exists(econ_path)) {
      existing <- read.csv(econ_path, stringsAsFactors = FALSE)
      school_yr <- sprintf("%d-%02d", FETCH_YEAR - 1, FETCH_YEAR %% 100)
      if (!school_yr %in% existing$Year) save_csv(rbind(existing, econ_new), econ_path)
    } else {
      save_csv(econ_new, econ_path)
    }
  }
}

# ── English Language Learners ─────────────────────────────────────────────────
# Same enrollment page — look for ELL rows

if (!is.null(enroll_tbl)) {
  ell_row <- enroll_tbl[grepl("english language learner|ELL", enroll_tbl[[1]], ignore.case = TRUE), ]
  
  if (nrow(ell_row) > 0 && !is.null(total_row)) {
    total_students <- as.numeric(gsub(",", "", total_row[[ncol(total_row)]][1]))
    ell_count      <- as.numeric(gsub(",", "", ell_row[[ncol(ell_row)]][1]))
    
    ell_new <- data.frame(
      Year               = sprintf("%d-%02d", FETCH_YEAR - 1, FETCH_YEAR %% 100),
      Source             = enroll_url,
      Count              = ell_count,
      Enrollment         = total_students,
      Proportion         = round(ell_count / total_students, 4),
      NonEnglishLearners = total_students - ell_count,
      Facet              = "2010-11 to Present"
    )
    
    ell_path <- here("data", "goal8", "EnglishLL.csv")
    if (file.exists(ell_path)) {
      existing <- read.csv(ell_path, stringsAsFactors = FALSE)
      school_yr <- sprintf("%d-%02d", FETCH_YEAR - 1, FETCH_YEAR %% 100)
      if (!school_yr %in% existing$Year) save_csv(rbind(existing, ell_new), ell_path)
    } else {
      save_csv(ell_new, ell_path)
    }
  }
}

# ── HS Graduation Rates (August Cohort) ───────────────────────────────────────

grad_url <- sprintf(
  "https://data.nysed.gov/gradrate.php?instid=%s&year=%d&cohortgroup=1",
  GHS_AUG_ID, FETCH_YEAR
)

grad_tbl <- read_nysed_table(grad_url)

if (!is.null(grad_tbl) && nrow(grad_tbl) > 0) {
  # NYSED grad rate page: first data row = All Students
  # Columns: Cohort | # in Cohort | # Graduated | % Graduated | ...
  # We capture cohort size and grad count for All Students + subgroups
  all_row <- grad_tbl[grepl("all student", grad_tbl[[1]], ignore.case = TRUE), ]
  
  if (nrow(all_row) > 0) {
    grad_new <- data.frame(
      Year        = FETCH_YEAR,
      Source      = grad_url,
      Community   = "GHS_Only_August",
      Total_Cohort = as.numeric(gsub(",", "", all_row[[2]][1])),
      Total_Grads  = as.numeric(gsub(",", "", all_row[[3]][1]))
      # Subgroup columns (White_Cohort, Black_Cohort, etc.) require
      # iterating over additional rows — add as needed
    )
    
    grad_path <- here("data", "goal8", "HSGradRates_August.csv")
    if (file.exists(grad_path)) {
      existing <- read.csv(grad_path, stringsAsFactors = FALSE)
      if (!FETCH_YEAR %in% existing$Year) save_csv(rbind(existing, grad_new), grad_path)
    } else {
      save_csv(grad_new, grad_path)
    }
  }
}

# ── Voter Turnout ─────────────────────────────────────────────────────────────
# NYSED voting results page lists all districts; filter to GCSD.

vote_url <- "http://www.p12.nysed.gov/mgtserv/votingresults/"
vote_page <- tryCatch(read_html(vote_url), error = function(e) NULL)

if (!is.null(vote_page)) {
  vote_links <- vote_page |>
    html_elements("a") |>
    html_attr("href")
  # Find the most recent results link (usually top of page)
  recent_link <- vote_links[grepl("\\.pdf|\\.htm", vote_links, ignore.case = TRUE)][1]
  message("  Voter results page found. Manual download may be needed for: ", vote_url)
  message("  Most recent link detected: ", recent_link)
  # NOTE: NYSED voting results are often PDFs or Excel files, not HTML tables.
  # Flag for manual review if automated extraction fails.
}

message("fetch_nysed.R complete for year ", FETCH_YEAR)