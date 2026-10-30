# fetch_nysdoh.R
# Scrapes NYS DOH Vital Statistics HTML tables for Ontario County.
# Requires: rvest, dplyr, here

library(rvest)
library(dplyr)
library(here)

if (!exists("FETCH_YEAR")) FETCH_YEAR <- 2024

# NYS DOH releases vital stats with a 2-year lag
VS_YEAR <- FETCH_YEAR - 2

ONTARIO_COUNTY <- "Ontario"

# ── Helpers ───────────────────────────────────────────────────────────────────

# Build the URL for a given year and table number
vs_url <- function(year, table_num) {
  sprintf(
    "https://www.health.ny.gov/statistics/vital_statistics/%d/table%02d.htm",
    year, table_num
  )
}

# Read an HTML table from a vital stats page, return as data frame
read_vs_table <- function(year, table_num, county_filter = ONTARIO_COUNTY) {
  url  <- vs_url(year, table_num)
  page <- tryCatch({
    resp <- httr::GET(url, httr::config(ssl_verifypeer = FALSE))
    read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) NULL)
  if (is.null(page)) {
    warning("Could not fetch: ", url)
    return(NULL)
  }
  tables <- html_table(page, fill = TRUE)
  if (length(tables) == 0) {
    warning("No tables found: ", url)
    return(NULL)
  }
  # Most vital stats pages have one main data table; try the largest one
  tbl <- tables[[which.max(sapply(tables, nrow))]]
  # Filter to Ontario County rows if a filter is given
  if (!is.null(county_filter)) {
    tbl <- tbl[grepl(county_filter, tbl[[1]], ignore.case = TRUE), ]
  }
  tbl$fetch_year <- year
  tbl
}

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

# ── Table 07: Births by County ────────────────────────────────────────────────
# Columns in raw table vary by year; we standardize to: Community, Year, Source, Births

tbl07 <- read_vs_table(VS_YEAR, 7)

if (!is.null(tbl07) && nrow(tbl07) > 0) {
  # Column names vary — rename by position or known header
  # Typical structure: County | Births | ...
  # Adjust column index if the table structure changes
  births_new <- data.frame(
    Community = "Ontario County",
    Year      = VS_YEAR,
    Source    = vs_url(VS_YEAR, 7),
    Births    = as.numeric(gsub(",", "", tbl07[[2]][1]))  # col 2 = births
  )
  
  births_path <- here("data", "goal1", "Births.csv")
  if (file.exists(births_path)) {
    existing <- read.csv(births_path, stringsAsFactors = FALSE)
    # Normalize Community column name if needed
    if (!VS_YEAR %in% existing$Year) {
      save_csv(rbind(existing, births_new), births_path)
    } else {
      message("  Births.csv already has year ", VS_YEAR)
    }
  } else {
    save_csv(births_new, births_path)
  }
}

# ── Table 11: Infant Mortality ────────────────────────────────────────────────

tbl11 <- read_vs_table(VS_YEAR, 11)

if (!is.null(tbl11) && nrow(tbl11) > 0) {
  # Typical cols: County | Infant Deaths | Live Births | Infant Death Rate
  # Rate is per 1,000 live births
  im_new <- data.frame(
    Community        = "Ontario County",
    Year             = VS_YEAR,
    Source           = vs_url(VS_YEAR, 11),
    `Infant Death Rate` = as.numeric(tbl11[[4]][1])  # col 4 = rate
  )
  
  im_path <- here("data", "goal1", "InfantMortality.csv")
  if (file.exists(im_path)) {
    existing <- read.csv(im_path, stringsAsFactors = FALSE)
    if (!VS_YEAR %in% existing$Year) save_csv(rbind(existing, im_new), im_path)
  } else {
    save_csv(im_new, im_path)
  }
}

# ── Table 30: Low Birth Weight ────────────────────────────────────────────────
# Also used for PregnanciesPlus (teen birth component)

tbl30 <- read_vs_table(VS_YEAR, 30)

if (!is.null(tbl30) && nrow(tbl30) > 0) {
  # Typical cols: County | Live Births | Low Birth Weight (n) | Low Birth Weight (%)
  lbw_new <- data.frame(
    Community              = "Ontario County",
    Year                   = VS_YEAR,
    Source                 = vs_url(VS_YEAR, 30),
    `Percent Low Birth Weight` = as.numeric(tbl30[[4]][1])
  )
  
  lbw_path <- here("data", "goal1", "LowBirthWeight.csv")
  if (file.exists(lbw_path)) {
    existing <- read.csv(lbw_path, stringsAsFactors = FALSE)
    if (!VS_YEAR %in% existing$Year) save_csv(rbind(existing, lbw_new), lbw_path)
  } else {
    save_csv(lbw_new, lbw_path)
  }
}

# ── Table 53: Teen Pregnancies ────────────────────────────────────────────────

tbl53 <- read_vs_table(VS_YEAR, 53)

if (!is.null(tbl53) && nrow(tbl53) > 0) {
  # Typical cols: County | Total Pregnancies | Teen Pregnancies | Teen Preg Rate
  preg_new <- data.frame(
    Year              = VS_YEAR,
    Source            = paste(vs_url(VS_YEAR, 30), vs_url(VS_YEAR, 53), sep = " | "),
    Location          = "Ontario County",
    Total_Pregnancies = as.numeric(gsub(",", "", tbl53[[2]][1])),
    Teen_Pregnancies  = as.numeric(gsub(",", "", tbl53[[3]][1])),
    Rate              = as.numeric(tbl53[[4]][1]),
    FemaleTeens       = NA  # filled from ACS S0101 or B01001 separately
  )
  
  preg_path <- here("data", "goal5", "PregnanciesPlus.csv")
  if (file.exists(preg_path)) {
    existing <- read.csv(preg_path, stringsAsFactors = FALSE)
    if (!VS_YEAR %in% existing$Year) save_csv(rbind(existing, preg_new), preg_path)
  } else {
    save_csv(preg_new, preg_path)
  }
}

message("fetch_nysdoh.R complete for VS year ", VS_YEAR)