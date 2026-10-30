# fetch_nysdoh_chac.R
# Scrapes NYS DOH CHAC Perinatal Statistics for Ontario County.
# Requires: rvest, dplyr, here
#
# CHAC data is published in 3-year rolling windows, not single years.
# Example window: 2020-2022 → URL uses "2020-2022" in path.
# The most recent window lags by ~2 years.

library(rvest)
library(dplyr)
library(here)

if (!exists("FETCH_YEAR")) FETCH_YEAR <- 2024

# CHAC releases 3-year windows; most recent available window
# e.g., for FETCH_YEAR=2024, the latest window is likely 2020-2022
CHAC_END   <- FETCH_YEAR - 2
CHAC_START <- CHAC_END - 2
CHAC_WINDOW <- sprintf("%d-%d", CHAC_START, CHAC_END)

# Ontario County ZIP codes to collect
ONTARIO_ZIPS <- c(
  "14424", "14425", "14432", "14456", "14466", "14469",
  "14471", "14475", "14504", "14512", "14532", "14548",
  "14560", "14561", "14564"
)

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

# ── Fetch CHAC county-level page ──────────────────────────────────────────────
# CHAC perinatal data is at:
# https://www.health.ny.gov/statistics/chac/perinatal/county/{window}/ontario.htm

chac_url <- sprintf(
  "https://www.health.ny.gov/statistics/chac/perinatal/county/%s/ontario.htm",
  CHAC_WINDOW
)

message("  Fetching CHAC: ", chac_url)

page <- tryCatch({
  resp <- httr::GET(chac_url, httr::config(ssl_verifypeer = FALSE))
  read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
}, error = function(e) {
  warning("Could not fetch CHAC page: ", chac_url)
  NULL
})

if (!is.null(page)) {
  # Use header=FALSE to handle complex merged/colspan headers
  tables <- html_table(page, fill = TRUE, header = FALSE)
  
  if (length(tables) > 0) {
    # CHAC county page typically has one main table with all ZIP codes
    chac_raw <- tables[[which.max(sapply(tables, nrow))]]
    # Drop the first two rows (merged header rows) and reset column names
    chac_raw <- chac_raw[-(1:2), ]
    row.names(chac_raw) <- NULL
    
    # Standardize column names — CHAC table headers change slightly by year
    # Expected columns (approximate):
    # ZIP | Births | Premature | Low_Birth_Weight | Unmarried |
    # Medicaid_or_Self_pay | Late_or_No_Prenatal_Care |
    # Infant_Deaths | Infant_Death_Rate | Neo_Natal_Deaths |
    # Neo_Natal_Death_Rate | Teen_Birth_Rate | Teen_Pregnancy_Rate
    
    # Assign clean names (adjust count based on actual columns)
    col_map <- c(
      "Location", "Births", "Premature", "Low_Birth_Weight",
      "Unmarried", "Medicaid_or_Self_pay", "Late_or_No_Prenatal_Care",
      "Infant_Deaths", "Infant_Death_Rate", "Neo_Natal_Deaths",
      "Neo_Natal_Death_Rate", "Teen_Birth_Rate", "Teen_Pregnancy_Rate"
    )
    names(chac_raw)[seq_len(min(ncol(chac_raw), length(col_map)))] <-
      col_map[seq_len(min(ncol(chac_raw), length(col_map)))]
    
    chac_raw$Year   <- CHAC_WINDOW
    chac_raw$Source <- chac_url
    
    # Filter to Ontario ZIP codes + county total
    chac_filtered <- chac_raw |>
      filter(
        Location %in% ONTARIO_ZIPS |
          grepl("ontario", Location, ignore.case = TRUE) |
          grepl("new york state", Location, ignore.case = TRUE)
      )
    
    # Remove header rows that got pulled in as data
    chac_filtered <- chac_filtered[!grepl("zip|location|births",
                                          chac_filtered$Location,
                                          ignore.case = TRUE), ]
    
    births_zip_path <- here("data", "goal1", "BirthsByZip.csv")
    
    if (file.exists(births_zip_path)) {
      existing <- read.csv(births_zip_path, stringsAsFactors = FALSE)
      if (!CHAC_WINDOW %in% existing$Year) {
        save_csv(rbind(existing, chac_filtered), births_zip_path)
      } else {
        message("  BirthsByZip.csv already has window ", CHAC_WINDOW)
      }
    } else {
      save_csv(chac_filtered, births_zip_path)
    }
    
  } else {
    warning("No tables found on CHAC page: ", chac_url)
  }
} else {
  message("  CHAC fetch failed for window ", CHAC_WINDOW,
          " — flag for manual download at: https://www.health.ny.gov/statistics/chac/perinatal/")
}

message("fetch_nysdoh_chac.R complete for window ", CHAC_WINDOW)