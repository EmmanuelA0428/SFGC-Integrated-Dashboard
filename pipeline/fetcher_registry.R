# fetcher_registry.R
# Registry of dataset fetchers that the Overview "Refresh" button can call.
#
# Each fetcher is a pure function: given a vector of target years, it returns
# a data frame that matches the dataset's CSV schema exactly (same columns in
# the same order). Fetchers NEVER write to disk — the Shiny overview module
# handles previewing (diff) and writing after user approval.
#
# Only datasets whose fetchers are considered reliable should be registered.
# Today that means the Census ACS 5-Year fetchers via tidycensus.

suppressWarnings(suppressMessages({
  # Lazy — only loaded when a fetcher actually runs.
  NULL
}))

# ── Helpers ──────────────────────────────────────────────────────────────────

# Resolve latest ACS 5-Year whose data is published.
# ACS 5-Year ending year N is released in December N+1. Err on the safe side
# by returning `current_year - 2` so a call in January still succeeds.
.census_latest_year <- function() {
  as.integer(format(Sys.Date(), "%Y")) - 2L
}

.geneva_place <- "28640"
.ontario_fips <- "069"
.state_fips   <- "36"

# Safe tidycensus call: returns NULL on failure with an attached warning.
.safe_get_acs <- function(...) {
  args <- list(..., show_call = FALSE)
  tryCatch(
    suppressMessages(do.call(tidycensus::get_acs, args)),
    error   = function(e) structure(NULL, fetch_error = conditionMessage(e)),
    warning = function(w) {
      v <- tryCatch(suppressWarnings(do.call(tidycensus::get_acs, args)),
                    error = function(e) NULL)
      attr(v, "fetch_warning") <- conditionMessage(w)
      v
    }
  )
}

.census_requires <- function() {
  if (!requireNamespace("tidycensus", quietly = TRUE))
    return(list(ok = FALSE, reason = "tidycensus package not installed"))
  if (!requireNamespace("dplyr", quietly = TRUE))
    return(list(ok = FALSE, reason = "dplyr package not installed"))
  if (!nzchar(Sys.getenv("CENSUS_API_KEY")))
    return(list(ok = FALSE,
                reason = "CENSUS_API_KEY not set. Install with tidycensus::census_api_key('KEY', install = TRUE)"))
  list(ok = TRUE)
}

# Run a callable for each year, bind_rows, stop early if any year fails hard.
.per_year <- function(years, fn) {
  rows <- list()
  for (y in years) {
    out <- fn(as.integer(y))
    if (is.null(out)) next
    rows[[length(rows) + 1L]] <- out
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# ── Census fetchers ──────────────────────────────────────────────────────────

.fetch_census_income <- function(years) {
  .per_year(years, function(y) {
    vars <- c(income = "DP03_0062")
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS 5-Year ", y, " DP03")
    data.frame(
      Community = c("City of Geneva", "Ontario", "NYS"),
      Year      = y,
      Source    = src,
      Income    = c(place$incomeE[1], county$incomeE[1], st$incomeE[1]),
      Yr        = y,
      stringsAsFactors = FALSE
    )
  })
}

.fetch_census_language <- function(years) {
  .per_year(years, function(y) {
    vars <- c(lang_other_pct = "DP02_0113P")
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS 5-Year ", y, " DP02")
    other <- c(place$lang_other_pctE[1], county$lang_other_pctE[1], st$lang_other_pctE[1])
    communities <- c("Geneva", "Ontario", "NYS")
    data.frame(
      Community = rep(communities, each = 2),
      Year      = y,
      Source    = src,
      Language  = rep(c("English only", "Other than English"), times = 3),
      PCT       = as.vector(rbind(100 - other, other)),
      stringsAsFactors = FALSE
    )
  })
}

.fetch_census_median_age <- function(years) {
  .per_year(years, function(y) {
    vars <- c(median_age = "DP05_0018")
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS 5-Year ", y, " DP05")
    data.frame(
      Community = c("City of Geneva", "Ontario", "NYS"),
      Year      = y,
      Source    = src,
      MedianAge = c(place$median_ageE[1], county$median_ageE[1], st$median_ageE[1]),
      stringsAsFactors = FALSE
    )
  })
}

.fetch_census_race <- function(years) {
  race_pairs <- c(
    "White"                            = "white_pct",
    "Black or African American"        = "black_pct",
    "American Indian / Alaska Native"  = "aian_pct",
    "Asian"                            = "asian_pct",
    "Hispanic or Latino"               = "hispanic_pct",
    "Two or More Races"                = "multiracial_pct"
  )
  vars <- c(white_pct = "DP05_0037P", black_pct = "DP05_0038P",
            aian_pct = "DP05_0039P", asian_pct = "DP05_0044P",
            hispanic_pct = "DP05_0071P", multiracial_pct = "DP05_0035P")

  .per_year(years, function(y) {
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS 5-Year ", y, " DP05")

    one <- function(df, community) {
      data.frame(
        Community = community,
        Year      = y,
        Source    = src,
        Race      = names(race_pairs),
        PCT       = vapply(paste0(race_pairs, "E"),
                           function(col) as.numeric(df[[col]][1]),
                           numeric(1)),
        stringsAsFactors = FALSE
      )
    }
    rbind(one(place, "City of Geneva"),
          one(county, "Ontario"),
          one(st,     "NYS"))
  })
}

.fetch_census_poverty_individuals <- function(years) {
  vars <- c(pov_rate = "S1701_C03_001",
            pov_count = "S1701_C02_001",
            total_pop = "S1701_C01_001")
  .per_year(years, function(y) {
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS S1701 ", y)

    one <- function(df, community) {
      rate  <- as.numeric(df$pov_rateE[1])
      data.frame(
        Community  = community,
        Year       = y,
        Source     = src,
        Population = as.numeric(df$total_popE[1]),
        Poverty    = as.numeric(df$pov_countE[1]),
        PCT        = rate,
        Proportion = rate / 100,
        stringsAsFactors = FALSE
      )
    }
    rbind(one(place, "City of Geneva"),
          one(county, "Ontario"),
          one(st,     "NYS"))
  })
}

# ── Goal-level variants ──────────────────────────────────────────────────────
# These produce rows shaped to match the goal-tab CSVs (different column order,
# "YYYY-YYYY" window labels, data.census.gov URLs in Source).

# ACS 5-Year ending year Y covers the 5-year window Y-4 … Y.
.acs_window <- function(y) sprintf("%d-%d", as.integer(y) - 4L, as.integer(y))

.dp03_url <- function(y) sprintf(
  "https://data.census.gov/table/ACSDP5Y%d.DP03?g=160XX00US3628640_050XX00US36069_040XX00US36&d=ACS+5-Year+Estimates+Data+Profiles",
  as.integer(y))

.dp02_url <- function(y) sprintf(
  "https://data.census.gov/table/ACSDP5Y%d.DP02?g=160XX00US3628640_050XX00US36069_010XX00US_040XX00US36&d=ACS+5-Year+Estimates+Data+Profiles",
  as.integer(y))

# Goal 3 Income.csv — schema: Community, Source, Year, Income, Yr
.fetch_census_income_goal3 <- function(years) {
  .per_year(years, function(y) {
    vars <- c(income = "DP03_0062")
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- .dp03_url(y)
    data.frame(
      Community = c("City of Geneva", "Ontario", "NYS"),
      Source    = src,
      Year      = y,
      Income    = c(place$incomeE[1], county$incomeE[1], st$incomeE[1]),
      Yr        = .acs_window(y),
      stringsAsFactors = FALSE
    )
  })
}

# Goal 3 HealthInsCoverage.csv — schema: Community, Source, Year, PCT
# DP03_0096PE = % Civilian noninstitutionalized population with health insurance
.fetch_census_health_ins_goal3 <- function(years) {
  .per_year(years, function(y) {
    vars <- c(ins_pct = "DP03_0096P")
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- .dp03_url(y)
    data.frame(
      Community = c("City of Geneva", "NYS", "Ontario"),
      Source    = src,
      Year      = y,
      PCT       = c(place$ins_pctE[1], st$ins_pctE[1], county$ins_pctE[1]),
      stringsAsFactors = FALSE
    )
  })
}

# Goal 8 ComputerAccess.csv — schema: Location, Source, Access, PCT, Year
# DP02_0152PE (2019-)  = % with a computer
# DP02_0153PE (2019-)  = % with broadband Internet subscription
# NOTE: Variable codes changed pre-2019; we only support 2019+.
.fetch_census_computer_access_goal8 <- function(years) {
  .per_year(years, function(y) {
    if (as.integer(y) < 2019L) {
      warning("ComputerAccess fetcher only supports ACS 5-Year >= 2019 ",
              "(pre-2019 used S2801 with different codes).")
      return(NULL)
    }
    vars <- c(computer_pct = "DP02_0152P", broadband_pct = "DP02_0153P")
    us      <- .safe_get_acs(geography = "us",     variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state",  state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    place   <- .safe_get_acs(geography = "place",  state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    if (is.null(us) || is.null(st) || is.null(county) || is.null(place))
      return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- .dp02_url(y)

    locs <- c("US", "NYS", "Ontario", "Geneva")
    bb   <- c(us$broadband_pctE[1], st$broadband_pctE[1],
              county$broadband_pctE[1], place$broadband_pctE[1])
    comp <- c(us$computer_pctE[1],  st$computer_pctE[1],
              county$computer_pctE[1], place$computer_pctE[1])
    data.frame(
      Location = rep(locs, times = 2),
      Source   = src,
      Access   = rep(c("BroadbandInternet", "Computer"), each = 4),
      PCT      = c(bb, comp),
      Year     = y,
      stringsAsFactors = FALSE
    )
  })
}

.fetch_census_poverty_youth <- function(years) {
  vars <- c(pov_youth_rate = "S1701_C03_002")
  .per_year(years, function(y) {
    place   <- .safe_get_acs(geography = "place", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    county  <- .safe_get_acs(geography = "county", state = .state_fips,
                             county = .ontario_fips, variables = vars,
                             year = y, survey = "acs5", output = "wide")
    st      <- .safe_get_acs(geography = "state", state = .state_fips,
                             variables = vars, year = y, survey = "acs5",
                             output = "wide")
    us      <- .safe_get_acs(geography = "us", variables = vars,
                             year = y, survey = "acs5", output = "wide")
    if (is.null(place) || is.null(county) || is.null(st)) return(NULL)
    place <- place[place$GEOID == paste0(.state_fips, .geneva_place), , drop = FALSE]
    src <- paste0("ACS S1701 ", y)
    rows <- data.frame(
      Community = c("City of Geneva", "Ontario", "NYS"),
      Year      = y,
      Source    = src,
      Rate      = c(place$pov_youth_rateE[1],
                    county$pov_youth_rateE[1],
                    st$pov_youth_rateE[1]),
      stringsAsFactors = FALSE
    )
    if (!is.null(us) && nrow(us) > 0) {
      rows <- rbind(rows, data.frame(
        Community = "US", Year = y, Source = src,
        Rate = as.numeric(us$pov_youth_rateE[1]),
        stringsAsFactors = FALSE
      ))
    }
    rows
  })
}

# ── Registry ─────────────────────────────────────────────────────────────────

FETCHER_REGISTRY <- list(

  census_income = list(
    label             = "ACS 5-Year · Median Household Income",
    source_name       = "US Census Bureau ACS DP03",
    source_url        = "https://data.census.gov/table?q=DP03",
    dataset_file      = "Income.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_income
  ),

  census_language_at_home = list(
    label             = "ACS 5-Year · Language at Home",
    source_name       = "US Census Bureau ACS DP02",
    source_url        = "https://data.census.gov/table?q=DP02",
    dataset_file      = "LanguageAtHome.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community", "Language"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_language
  ),

  census_median_age = list(
    label             = "ACS 5-Year · Median Age",
    source_name       = "US Census Bureau ACS DP05",
    source_url        = "https://data.census.gov/table?q=DP05",
    dataset_file      = "MedianAge.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_median_age
  ),

  census_race = list(
    label             = "ACS 5-Year · Population by Race/Ethnicity",
    source_name       = "US Census Bureau ACS DP05",
    source_url        = "https://data.census.gov/table?q=DP05",
    dataset_file      = "Race.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community", "Race"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_race
  ),

  census_poverty_individuals = list(
    label             = "ACS 5-Year · Poverty (All People)",
    source_name       = "US Census Bureau ACS S1701",
    source_url        = "https://data.census.gov/table?q=S1701",
    dataset_file      = "PovertyIndividuals.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_poverty_individuals
  ),

  census_poverty_youth = list(
    label             = "ACS 5-Year · Poverty (Under 18)",
    source_name       = "US Census Bureau ACS S1701",
    source_url        = "https://data.census.gov/table?q=S1701",
    dataset_file      = "PovertyYouth.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_poverty_youth
  ),

  # ── Goal-tab variants ──────────────────────────────────────────────────

  census_income_goal3 = list(
    label             = "ACS 5-Year · Median Household Income (Goal 3 layout)",
    source_name       = "US Census Bureau ACS DP03",
    source_url        = "https://data.census.gov/table?q=DP03",
    dataset_file      = "goal3/Income.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_income_goal3
  ),

  census_health_ins_goal3 = list(
    label             = "ACS 5-Year · Health Insurance Coverage",
    source_name       = "US Census Bureau ACS DP03",
    source_url        = "https://data.census.gov/table?q=DP03",
    dataset_file      = "goal3/HealthInsCoverage.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Community"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_health_ins_goal3
  ),

  census_computer_access_goal8 = list(
    label             = "ACS 5-Year · Computer & Broadband Access",
    source_name       = "US Census Bureau ACS DP02",
    source_url        = "https://data.census.gov/table?q=DP02",
    dataset_file      = "goal8/ComputerAccess.csv",
    year_column       = "Year",
    key_columns       = c("Year", "Location", "Access"),
    requires          = .census_requires,
    latest_year       = .census_latest_year,
    fetch             = .fetch_census_computer_access_goal8
  )
)

# ── Public API ───────────────────────────────────────────────────────────────

fetcher_handles <- function() names(FETCHER_REGISTRY)

fetcher_for <- function(handle) {
  if (is.null(handle) || !nzchar(handle)) return(NULL)
  FETCHER_REGISTRY[[handle]]
}

fetcher_available <- function(handle) {
  f <- fetcher_for(handle)
  if (is.null(f)) return(list(ok = FALSE, reason = "Unknown fetcher"))
  if (is.null(f$requires)) return(list(ok = TRUE))
  f$requires()
}

fetcher_latest_year <- function(handle) {
  f <- fetcher_for(handle)
  if (is.null(f) || is.null(f$latest_year)) return(NA_integer_)
  tryCatch(as.integer(f$latest_year()), error = function(e) NA_integer_)
}

# Produce a preview without writing: fetch rows for `years` and classify each
# fetched row against an existing data frame into new / conflicting / identical.
#
# Returns a list:
#   ok               logical
#   error            character(0) or "why"
#   fetched          data.frame (rows pulled from the source, coerced to df's schema)
#   new_rows         data.frame (rows whose key isn't in the existing file)
#   conflict_rows    data.frame (rows whose key exists but values differ; appended with
#                    a "Status" = "Conflict" column)
#   identical_rows   data.frame (rows whose key exists and all values match)
#   missing_cols     character — columns present in existing but not fetched
#   extra_cols       character — columns fetched but not in existing
fetcher_dry_run <- function(handle, years, existing_df) {
  f <- fetcher_for(handle)
  if (is.null(f))
    return(list(ok = FALSE, error = "Unknown fetcher"))
  avail <- fetcher_available(handle)
  if (!isTRUE(avail$ok))
    return(list(ok = FALSE, error = avail$reason %||% "Fetcher not available"))
  if (!length(years) || all(is.na(years)))
    return(list(ok = FALSE, error = "No years to fetch"))

  years <- as.integer(years[!is.na(years)])
  fetched <- tryCatch(f$fetch(years),
                      error = function(e) structure(NULL,
                                                    fetch_error = conditionMessage(e)))
  if (is.null(fetched) || !nrow(fetched)) {
    err <- attr(fetched, "fetch_error") %||% "Fetcher returned no rows"
    return(list(ok = FALSE, error = err))
  }

  # Align columns with existing schema, where possible.
  if (!is.null(existing_df) && ncol(existing_df) > 0) {
    common <- intersect(names(existing_df), names(fetched))
    missing_cols <- setdiff(names(existing_df), names(fetched))
    extra_cols   <- setdiff(names(fetched), names(existing_df))
    for (c in missing_cols) fetched[[c]] <- NA
    fetched <- fetched[, names(existing_df), drop = FALSE]
  } else {
    common <- names(fetched)
    missing_cols <- character()
    extra_cols   <- character()
  }

  keys <- f$key_columns %||% c(f$year_column %||% "Year")
  keys <- intersect(keys, names(fetched))
  if (!length(keys)) keys <- names(fetched)[1]

  key_str <- function(df) {
    do.call(paste, c(lapply(keys, function(k) as.character(df[[k]])),
                     list(sep = "\u001f")))
  }

  if (is.null(existing_df) || !nrow(existing_df)) {
    return(list(
      ok             = TRUE,
      error          = NULL,
      fetched        = fetched,
      new_rows       = fetched,
      conflict_rows  = fetched[integer(0), , drop = FALSE],
      identical_rows = fetched[integer(0), , drop = FALSE],
      missing_cols   = missing_cols,
      extra_cols     = extra_cols,
      key_columns    = keys
    ))
  }

  ex_keys <- key_str(existing_df)
  ft_keys <- key_str(fetched)
  is_new  <- !(ft_keys %in% ex_keys)

  compare_rows <- function(a, b) {
    # a (fetched row) and b (existing row) — compare non-key columns.
    cols <- setdiff(intersect(names(a), names(b)), keys)
    for (c in cols) {
      av <- a[[c]]; bv <- b[[c]]
      if (is.numeric(av) && is.numeric(bv)) {
        same <- isTRUE(all.equal(as.numeric(av), as.numeric(bv),
                                 tolerance = 1e-6))
      } else {
        same <- identical(
          ifelse(is.na(av), NA_character_, as.character(av)),
          ifelse(is.na(bv), NA_character_, as.character(bv))
        )
      }
      if (!isTRUE(same)) return(FALSE)
    }
    TRUE
  }

  is_conflict <- logical(nrow(fetched))
  for (i in which(!is_new)) {
    j <- which(ex_keys == ft_keys[i])[1]
    is_conflict[i] <- !compare_rows(fetched[i, , drop = FALSE],
                                    existing_df[j, , drop = FALSE])
  }
  is_identical <- !is_new & !is_conflict

  list(
    ok             = TRUE,
    error          = NULL,
    fetched        = fetched,
    new_rows       = fetched[is_new, , drop = FALSE],
    conflict_rows  = fetched[is_conflict, , drop = FALSE],
    identical_rows = fetched[is_identical, , drop = FALSE],
    missing_cols   = missing_cols,
    extra_cols     = extra_cols,
    key_columns    = keys
  )
}

# Given a staged data frame of new rows (already aligned to existing schema)
# and a mode, return the data frame that should be written to disk.
#   mode = "append"    -> existing + new_rows (skipping duplicates by key)
#   mode = "overwrite" -> existing rows with matching Year replaced by fetched;
#                         otherwise unchanged.
fetcher_apply_rows <- function(existing_df, fetched_df, keys,
                               mode = c("append", "overwrite")) {
  mode <- match.arg(mode)
  if (!nrow(fetched_df)) return(existing_df)
  if (is.null(existing_df) || !nrow(existing_df)) return(fetched_df)

  keys <- intersect(keys, names(existing_df))
  if (!length(keys)) keys <- intersect(names(existing_df), names(fetched_df))[1]

  key_str <- function(df) {
    do.call(paste, c(lapply(keys, function(k) as.character(df[[k]])),
                     list(sep = "\u001f")))
  }

  if (mode == "append") {
    ex_keys <- key_str(existing_df)
    ft_keys <- key_str(fetched_df)
    add <- fetched_df[!(ft_keys %in% ex_keys), , drop = FALSE]
    if (!nrow(add)) return(existing_df)
    rbind(existing_df, add)
  } else {
    # overwrite — by year (so a year present in fetched replaces the whole
    # block of that year in existing)
    year_col <- intersect(c("Year", "Yr"), names(existing_df))[1]
    if (is.na(year_col)) year_col <- keys[1]
    keep <- !(existing_df[[year_col]] %in% unique(fetched_df[[year_col]]))
    rbind(existing_df[keep, , drop = FALSE], fetched_df)
  }
}
