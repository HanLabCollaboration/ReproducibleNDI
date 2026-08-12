#' Fetch raw ACS data for the Powell-Wiley NDI variables
#'
#' @description
#' This is an internal helper function that queries the Census API for all
#' variables required to compute the Powell-Wiley Neighborhood Deprivation
#' Index (Andrews et al. 2020), following the variable definitions of the
#' `ndi` package (`ndi::powell_wiley`).
#'
#' The Powell-Wiley variable set draws on ACS detailed tables (B/C),
#' data-profile tables (DP), and subject tables (S). The DP and S tables are
#' not published below the tract level, so this method is only available for
#' `geo = "county"` or `"tract"`.
#'
#' @param geo Geography level (`"county"` or `"tract"`).
#' @param year The ACS5 year. For years before 2015 the "no telephone service"
#'   variable is `DP04_0074P`; from 2015 onward it is `DP04_0075P`.
#' @param states A vector of state abbreviations or 'all'.
#' @param get_geometry Boolean, whether to download geometry.
#'
#' @return A tidycensus data frame with an `acs_vars` attribute.

fetchPowellWileyAcs <- function(geo, year, states, get_geometry) {
  # --- Variable Definitions (from ndi::powell_wiley) ---
  var_POPTOT  <- "B01001_001"   # total population
  var_MHHINC  <- "B19013_001"   # median household income
  # PctRecvIDR: households with interest, dividends, or net rental income
  var_IDR     <- "B19054_002"
  var_IDR_den <- "B19054_001"
  # PctPubAsst: households with public assistance income
  var_PUB     <- "B19058_002"
  var_PUB_den <- "B19058_001"
  var_MVALUE  <- "B25077_001"   # median home value
  # PctMgmtBusScArti: management, business, science, and arts occupations
  var_MGMT     <- "C24060_002"
  var_MGMT_den <- "C24060_001"
  # PctFemHeadKids: female-headed households with own children
  var_FHH     <- c("B11005_007", "B11005_010")
  var_FHH_den <- "B11005_001"
  var_OWNOCC  <- "DP04_0046P"   # owner-occupied housing units (percent)
  # PctNoPhone: no telephone service available (percent) - variable changed in 2015
  var_NOPHONE <- if (year < 2015) "DP04_0074P" else "DP04_0075P"
  var_NCPLMB  <- "DP04_0073P"   # lacking complete plumbing facilities (percent)
  # PctEduc*: educational attainment (25+), used for HS+ and Bachelors+
  var_EDU     <- paste0("S1501_C01_0", c("09", "10", "11", "12", "13"))
  var_EDU_BCH <- paste0("S1501_C01_0", c("12", "13"))
  var_EDU_den <- "S1501_C01_006"
  var_FAMPOV  <- "S1702_C02_001"  # families below poverty (percent)
  var_UNEMP   <- "S2301_C04_001"  # unemployment rate (percent)

  vars <- c(var_POPTOT, var_MHHINC,
            var_IDR, var_IDR_den,
            var_PUB, var_PUB_den,
            var_MVALUE,
            var_MGMT, var_MGMT_den,
            var_FHH, var_FHH_den,
            var_OWNOCC, var_NOPHONE, var_NCPLMB,
            var_EDU, var_EDU_den,
            var_FAMPOV, var_UNEMP)

  # Keep track of the original names for the calculation step
  acs_vars <- list(
    vars        = paste0(vars, "E"),
    var_POPTOT  = paste0(var_POPTOT, "E"),
    var_MHHINC  = paste0(var_MHHINC, "E"),
    var_IDR     = paste0(var_IDR, "E"),
    var_IDR_den = paste0(var_IDR_den, "E"),
    var_PUB     = paste0(var_PUB, "E"),
    var_PUB_den = paste0(var_PUB_den, "E"),
    var_MVALUE  = paste0(var_MVALUE, "E"),
    var_MGMT     = paste0(var_MGMT, "E"),
    var_MGMT_den = paste0(var_MGMT_den, "E"),
    var_FHH     = paste0(var_FHH, "E"),
    var_FHH_den = paste0(var_FHH_den, "E"),
    var_OWNOCC  = paste0(var_OWNOCC, "E"),
    var_NOPHONE = paste0(var_NOPHONE, "E"),
    var_NCPLMB  = paste0(var_NCPLMB, "E"),
    var_EDU     = paste0(var_EDU, "E"),
    var_EDU_BCH = paste0(var_EDU_BCH, "E"),
    var_EDU_den = paste0(var_EDU_den, "E"),
    var_FAMPOV  = paste0(var_FAMPOV, "E"),
    var_UNEMP   = paste0(var_UNEMP, "E")
  )

  # --- Data Fetching ---
  # tidycensus resolves the mixed detailed/profile/subject variables in a
  # single request.
  fetch_args <- list(
    geography   = geo,
    year        = year,
    output      = "wide",
    variables   = acs_vars$vars,
    survey      = "acs5",
    geometry    = get_geometry,
    cache_table = TRUE,
    state       = states
  )

  ndi_data_raw <- do.call(tidycensus::get_acs, fetch_args)

  # Attach var names as an attribute for the next step
  ndi_data_raw <- ndi_data_raw |>
    dplyr::select(GEOID, NAME, dplyr::ends_with('E'), dplyr::ends_with('M'))
  attr(ndi_data_raw, "acs_vars") <- acs_vars
  return(ndi_data_raw)
}
