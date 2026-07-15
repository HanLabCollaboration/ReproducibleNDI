#' Fetch raw ACS data for the Messer NDI variables
#'
#' @description
#' This is an internal helper function that queries the Census API for all
#' variables required to compute the Messer Neighborhood Deprivation Index
#' (Messer et al. 2006), following the variable definitions of the
#' `ndi` package (`ndi::messer`).
#'
#' @param geo Geography level (e.g., "county", "tract", "block group").
#' @param year The ACS5 year.
#' @param states A vector of state abbreviations or 'all'.
#' @param get_geometry Boolean, whether to download geometry.
#'
#' @return A tidycensus data frame with an `acs_vars` attribute.

fetchMesserAcs <- function(geo, year, states, get_geometry) {
  # --- Variable Definitions (from ndi::messer) ---
  var_POPTOT  <- "B01003_001"
  # OCC: males in management, business, science, and arts occupations
  var_OCC     <- c("C24030_018", "C24030_019")
  var_OCC_den <- "C24030_002"
  # CWD: crowded housing (> 1 occupant per room)
  var_CWD     <- c(paste0("B25014_00", 5:7), paste0("B25014_0", 11:13))
  var_CWD_den <- "B25014_001"
  # POV: households below the poverty line
  var_POV     <- "B17017_002"
  var_POV_den <- "B17017_001"
  # FHH: female-headed households with dependent children
  var_FHH     <- c("B25115_012", "B25115_025")
  var_FHH_den <- "B25115_001"
  # PUB: households on public assistance or food stamps
  var_PUB     <- "B19058_002"
  var_PUB_den <- "B19058_001"
  # U30: households earning under $30,000 per year
  var_U30     <- paste0("B19001_00", 2:6)
  var_U30_den <- "B19001_001"
  # EDU: adults (25+) with less than a high school education
  var_EDU     <- "B06009_002"
  var_EDU_den <- "B06009_001"
  # EMP: unemployed (of the civilian labor force)
  var_EMP     <- "B23025_005"
  var_EMP_den <- "B23025_003"

  vars <- c(var_POPTOT,
            var_OCC, var_OCC_den,
            var_CWD, var_CWD_den,
            var_POV, var_POV_den,
            var_FHH, var_FHH_den,
            var_PUB, var_PUB_den,
            var_U30, var_U30_den,
            var_EDU, var_EDU_den,
            var_EMP, var_EMP_den)

  # Keep track of the original names for the calculation step
  acs_vars <- list(
    vars        = paste0(vars, "E"),
    var_POPTOT  = paste0(var_POPTOT, "E"),
    var_OCC     = paste0(var_OCC, "E"),
    var_OCC_den = paste0(var_OCC_den, "E"),
    var_CWD     = paste0(var_CWD, "E"),
    var_CWD_den = paste0(var_CWD_den, "E"),
    var_POV     = paste0(var_POV, "E"),
    var_POV_den = paste0(var_POV_den, "E"),
    var_FHH     = paste0(var_FHH, "E"),
    var_FHH_den = paste0(var_FHH_den, "E"),
    var_PUB     = paste0(var_PUB, "E"),
    var_PUB_den = paste0(var_PUB_den, "E"),
    var_U30     = paste0(var_U30, "E"),
    var_U30_den = paste0(var_U30_den, "E"),
    var_EDU     = paste0(var_EDU, "E"),
    var_EDU_den = paste0(var_EDU_den, "E"),
    var_EMP     = paste0(var_EMP, "E"),
    var_EMP_den = paste0(var_EMP_den, "E")
  )

  # --- Data Fetching ---
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
