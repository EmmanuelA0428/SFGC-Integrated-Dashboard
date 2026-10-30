# fetch_census.R
# Fetches ACS 5-Year Estimates for Geneva, Ontario County, and NYS.
# Requires: tidycensus, dplyr, here
# Census API key must be installed: census_api_key("KEY", install = TRUE)

library(tidycensus)
library(dplyr)
library(here)

library(httr)
httr::set_config(httr::config(ssl_verifypeer = FALSE))

# Read FETCH_YEAR from parent environment (set in run_pipeline.R)
# When running standalone, set this manually:
if (!exists("FETCH_YEAR")) FETCH_YEAR <- 2023

# ACS releases lag by ~1 year; tidycensus uses the survey end year
ACS_YEAR <- FETCH_YEAR - 1  # e.g. 2024 run → ACS 2023 5-year

# ── Geography definitions ─────────────────────────────────────────────────────
# Geneva city FIPS: state=36, place=28640
# Ontario County FIPS: state=36, county=069

GENEVA_PLACE  <- "28640"
ONTARIO_FIPS  <- "069"
STATE_FIPS    <- "36"

# Helper: backup then overwrite
save_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  }
  write.csv(df, path, row.names = FALSE)
  # Update metadata last_updated
  dataset_name <- basename(path)
  meta_path <- here("data", "metadata", "datasets.csv")
  if (file.exists(meta_path)) {
    meta <- read.csv(meta_path, stringsAsFactors = FALSE)
    meta$last_updated[meta$dataset == dataset_name] <- as.character(Sys.Date())
    write.csv(meta, meta_path, row.names = FALSE)
  }
  message("  Saved: ", path)
}

# ── DP03: Income, Poverty, SNAP ───────────────────────────────────────────────
# Variables:
#   DP03_0062E = Median household income
#   DP03_0119PE = % families below poverty
#   DP03_0074PE = % households with SNAP

dp03 <- get_acs(
  geography = "place",
  state     = STATE_FIPS,
  variables = c(
    income   = "DP03_0062",
    pov_pct  = "DP03_0119P",
    snap_pct = "DP03_0074"
  ),
  year    = ACS_YEAR,
  survey  = "acs5",
  output  = "wide"
) |>
  filter(GEOID == paste0(STATE_FIPS, GENEVA_PLACE))

dp03_county <- get_acs(
  geography = "county",
  state     = STATE_FIPS,
  county    = ONTARIO_FIPS,
  variables = c(
    income   = "DP03_0062",
    pov_pct  = "DP03_0119P",
    snap_pct = "DP03_0074"
  ),
  year   = ACS_YEAR,
  survey = "acs5",
  output = "wide"
)

dp03_state <- get_acs(
  geography = "state",
  state     = STATE_FIPS,
  variables = c(
    income   = "DP03_0062",
    pov_pct  = "DP03_0119P",
    snap_pct = "DP03_0074"
  ),
  year   = ACS_YEAR,
  survey = "acs5",
  output = "wide"
)

# ── Income.csv ────────────────────────────────────────────────────────────────

build_income <- function(dp03_place, dp03_co, dp03_st, year) {
  bind_rows(
    data.frame(Community = "City of Geneva", Year = year,
               Source = paste0("ACS 5-Year ", year, " DP03"),
               Income = dp03_place$incomeE, Yr = year),
    data.frame(Community = "Ontario", Year = year,
               Source = paste0("ACS 5-Year ", year, " DP03"),
               Income = dp03_co$incomeE, Yr = year),
    data.frame(Community = "NYS", Year = year,
               Source = paste0("ACS 5-Year ", year, " DP03"),
               Income = dp03_st$incomeE, Yr = year)
  )
}

income_path <- here("data", "background", "Income.csv")
if (file.exists(income_path)) {
  existing_income <- read.csv(income_path, stringsAsFactors = FALSE)
  new_income <- build_income(dp03, dp03_county, dp03_state, ACS_YEAR)
  # Append only if year not already present
  if (!ACS_YEAR %in% existing_income$Year) {
    save_csv(rbind(existing_income, new_income), income_path)
  } else {
    message("  Income.csv already has year ", ACS_YEAR, ", skipping.")
  }
} else {
  save_csv(build_income(dp03, dp03_county, dp03_state, ACS_YEAR), income_path)
}

# ── DP02: Education, Language, Place of Birth ─────────────────────────────────
# Variables:
#   DP02_0067PE = % bachelor's or higher
#   DP02_0113PE = % foreign born
#   DP02_0111E  = total foreign born count
#   DP02_0115PE = % speak language other than English at home

dp02_vars <- c(
  bach_pct         = "DP02_0068P",
  foreign_born_pct = "DP02_0093P",
  lang_other_pct   = "DP02_0113P"
)

get_dp02 <- function(geo, state, county = NULL, place = NULL) {
  get_acs(
    geography = geo, state = state,
    county = county,
    variables = dp02_vars,
    year = ACS_YEAR, survey = "acs5", output = "wide"
  )
}

dp02_geneva  <- get_dp02("place", STATE_FIPS) |> filter(GEOID == paste0(STATE_FIPS, GENEVA_PLACE))
dp02_ontario <- get_dp02("county", STATE_FIPS, county = ONTARIO_FIPS)
dp02_nys     <- get_dp02("state", STATE_FIPS)

# LanguageAtHome.csv
lang <- bind_rows(
  data.frame(Community = "Geneva",   Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "English only",      PCT = 100 - dp02_geneva$lang_other_pctE),
  data.frame(Community = "Geneva",   Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "Other than English", PCT = dp02_geneva$lang_other_pctE),
  data.frame(Community = "Ontario",  Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "English only",      PCT = 100 - dp02_ontario$lang_other_pctE),
  data.frame(Community = "Ontario",  Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "Other than English", PCT = dp02_ontario$lang_other_pctE),
  data.frame(Community = "NYS",      Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "English only",      PCT = 100 - dp02_nys$lang_other_pctE),
  data.frame(Community = "NYS",      Year = ACS_YEAR, Source = paste0("ACS 5-Year ", ACS_YEAR, " DP02"), Language = "Other than English", PCT = dp02_nys$lang_other_pctE)
)

lang_path <- here("data", "background", "LanguageAtHome.csv")
if (file.exists(lang_path)) {
  existing <- read.csv(lang_path, stringsAsFactors = FALSE)
  if (!ACS_YEAR %in% existing$Year) save_csv(rbind(existing, lang), lang_path)
} else {
  save_csv(lang, lang_path)
}

# ── DP05: Age, Race ───────────────────────────────────────────────────────────
# Variables:
#   DP05_0018E = Median age
#   DP05_0037PE - DP05_0057PE = Race/ethnicity percentages

dp05_vars <- c(
  median_age      = "DP05_0018",
  white_pct       = "DP05_0037P",
  black_pct       = "DP05_0038P",
  aian_pct        = "DP05_0039P",
  asian_pct       = "DP05_0044P",
  hispanic_pct    = "DP05_0071P",
  multiracial_pct = "DP05_0035P"
)

get_dp05 <- function(geo, county = NULL) {
  get_acs(geography = geo, state = STATE_FIPS, county = county,
          variables = dp05_vars, year = ACS_YEAR, survey = "acs5", output = "wide")
}

dp05_geneva  <- get_dp05("place") |> filter(GEOID == paste0(STATE_FIPS, GENEVA_PLACE))
dp05_ontario <- get_dp05("county", county = ONTARIO_FIPS)
dp05_nys     <- get_dp05("state")

# MedianAge.csv
build_age <- function(place, county, state, yr) {
  bind_rows(
    data.frame(Community = "City of Geneva", Year = yr, Source = paste0("ACS 5-Year ", yr, " DP05"), MedianAge = place$median_ageE),
    data.frame(Community = "Ontario",        Year = yr, Source = paste0("ACS 5-Year ", yr, " DP05"), MedianAge = county$median_ageE),
    data.frame(Community = "NYS",            Year = yr, Source = paste0("ACS 5-Year ", yr, " DP05"), MedianAge = state$median_ageE)
  )
}

age_path <- here("data", "background", "MedianAge.csv")
if (file.exists(age_path)) {
  existing <- read.csv(age_path, stringsAsFactors = FALSE)
  if (!ACS_YEAR %in% existing$Year) save_csv(rbind(existing, build_age(dp05_geneva, dp05_ontario, dp05_nys, ACS_YEAR)), age_path)
} else {
  save_csv(build_age(dp05_geneva, dp05_ontario, dp05_nys, ACS_YEAR), age_path)
}

# Race.csv
build_race <- function(df, community, yr) {
  races <- c("White" = "white_pctE", "Black or African American" = "black_pctE",
             "American Indian / Alaska Native" = "aian_pctE", "Asian" = "asian_pctE",
             "Hispanic or Latino" = "hispanic_pctE", "Two or More Races" = "multiracial_pctE")
  purrr::map_dfr(names(races), function(r) {
    data.frame(Community = community, Year = yr,
               Source = paste0("ACS 5-Year ", yr, " DP05"),
               Race = r, PCT = df[[races[r]]])
  })
}

race_new <- bind_rows(
  build_race(dp05_geneva,  "City of Geneva", ACS_YEAR),
  build_race(dp05_ontario, "Ontario",        ACS_YEAR),
  build_race(dp05_nys,     "NYS",            ACS_YEAR)
)

race_path <- here("data", "background", "Race.csv")
if (file.exists(race_path)) {
  existing <- read.csv(race_path, stringsAsFactors = FALSE)
  if (!ACS_YEAR %in% existing$Year) save_csv(rbind(existing, race_new), race_path)
} else {
  save_csv(race_new, race_path)
}

# ── S1701: Poverty ────────────────────────────────────────────────────────────
# S1701_C03_001E = poverty rate (all people)
# S1701_C03_002E = poverty rate under 18

s1701_vars <- c(
  pov_rate       = "S1701_C03_001",
  pov_count      = "S1701_C02_001",
  pov_youth_rate = "S1701_C03_002",
  total_pop      = "S1701_C01_001"
)

get_s1701 <- function(geo, county = NULL) {
  get_acs(geography = geo, state = STATE_FIPS, county = county,
          variables = s1701_vars, year = ACS_YEAR, survey = "acs5", output = "wide")
}

s1701_geneva  <- get_s1701("place") |> filter(GEOID == paste0(STATE_FIPS, GENEVA_PLACE))
s1701_ontario <- get_s1701("county", county = ONTARIO_FIPS)
s1701_nys     <- get_s1701("state")

# PovertyIndividuals.csv
build_pov_ind <- function(g, o, n, yr) {
  bind_rows(
    data.frame(Community = "City of Geneva", Year = yr, Source = paste0("ACS S1701 ", yr),
               Population = g$total_popE, Poverty = g$pov_countE,
               PCT = g$pov_rateE, Proportion = g$pov_rateE / 100),
    data.frame(Community = "Ontario", Year = yr, Source = paste0("ACS S1701 ", yr),
               Population = o$total_popE, Poverty = o$pov_countE,
               PCT = o$pov_rateE, Proportion = o$pov_rateE / 100),
    data.frame(Community = "NYS", Year = yr, Source = paste0("ACS S1701 ", yr),
               Population = n$total_popE, Poverty = n$pov_countE,
               PCT = n$pov_rateE, Proportion = n$pov_rateE / 100)
  )
}

pov_ind_path <- here("data", "background", "PovertyIndividuals.csv")
if (file.exists(pov_ind_path)) {
  existing <- read.csv(pov_ind_path, stringsAsFactors = FALSE)
  new_rows <- build_pov_ind(s1701_geneva, s1701_ontario, s1701_nys, ACS_YEAR)
  if (!ACS_YEAR %in% existing$Year) save_csv(rbind(existing, new_rows), pov_ind_path)
} else {
  save_csv(build_pov_ind(s1701_geneva, s1701_ontario, s1701_nys, ACS_YEAR), pov_ind_path)
}

# PovertyYouth.csv
build_pov_youth <- function(g, o, n, u, yr) {
  bind_rows(
    data.frame(Community = "City of Geneva", Year = yr, Source = paste0("ACS S1701 ", yr), Rate = g$pov_youth_rateE),
    data.frame(Community = "Ontario",        Year = yr, Source = paste0("ACS S1701 ", yr), Rate = o$pov_youth_rateE),
    data.frame(Community = "NYS",            Year = yr, Source = paste0("ACS S1701 ", yr), Rate = n$pov_youth_rateE)
  )
}

# US-level: get separately
s1701_us <- get_acs(geography = "us", variables = c(pov_youth_rate = "S1701_C03_002"),
                    year = ACS_YEAR, survey = "acs5", output = "wide")
if (nrow(s1701_us) > 0) {
  us_row <- data.frame(Community = "US", Year = ACS_YEAR,
                       Source = paste0("ACS S1701 ", ACS_YEAR),
                       Rate = s1701_us$pov_youth_rateE)
}

pov_youth_path <- here("data", "background", "PovertyYouth.csv")
if (file.exists(pov_youth_path)) {
  existing <- read.csv(pov_youth_path, stringsAsFactors = FALSE)
  new_rows <- bind_rows(
    build_pov_youth(s1701_geneva, s1701_ontario, s1701_nys, NULL, ACS_YEAR),
    if (exists("us_row")) us_row
  )
  if (!ACS_YEAR %in% existing$Year) save_csv(rbind(existing, new_rows), pov_youth_path)
} else {
  save_csv(build_pov_youth(s1701_geneva, s1701_ontario, s1701_nys, NULL, ACS_YEAR), pov_youth_path)
}

message("fetch_census.R complete for ACS year ", ACS_YEAR)