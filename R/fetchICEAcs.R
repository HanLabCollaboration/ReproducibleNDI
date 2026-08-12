#' Fetch raw ACS data for the Krieger ICE variables
#'
#' @description
#' This is an internal helper function that queries the Census API for all
#' variables required to compute the Index of Concentration at the Extremes
#' (ICE; Krieger et al.), following the variable definitions of the `ndi`
#' package (`ndi::krieger`).
#'
#' All ICE variables come from ACS detailed (`B`) tables, which the Census
#' Bureau publishes down to the block-group level, so ICE is available for
#' `geo = "county"`, `"tract"`, or `"block group"`.
#'
#' @param geo Geography level (`"county"`, `"tract"`, or `"block group"`).
#' @param year The ACS5 year.
#' @param states A vector of state abbreviations or 'all'.
#' @param get_geometry Boolean, whether to download geometry.
#'
#' @return A tidycensus data frame with an `acs_vars` attribute.

fetchICEAcs <- function(geo, year, states, get_geometry) {
  # --- Variable Definitions (from ndi::krieger) ---
  # Household income (B19001): low = _002.._006, high = _014.._017
  var_INC_tot  <- "B19001_001"
  var_INC_low  <- paste0("B19001_00", 2:6)
  var_INC_high <- c("B19001_014", "B19001_015", "B19001_016", "B19001_017")

  # Household income, White alone (not Hispanic) (B19001H)
  var_INCW_low  <- paste0("B19001H_00", 2:6)
  var_INCW_high <- c("B19001H_014", "B19001H_015", "B19001H_016", "B19001H_017")

  # Household income, Black or African American alone (B19001B): low brackets only
  var_INCB_low  <- paste0("B19001B_00", 2:6)

  # Educational attainment for population 25+ (B15002)
  var_EDU_tot <- "B15002_001"
  # Less than high school: male _003.._010, female _020.._027
  var_EDU_ltHS <- c(paste0("B15002_00", 3:9), "B15002_010",
                    paste0("B15002_0", 20:27))
  # Bachelor's or higher: male _015.._018, female _032.._035
  var_EDU_BDup <- c(paste0("B15002_0", 15:18), paste0("B15002_0", 32:35))

  # Race/ethnicity (B03002)
  var_RE_tot <- "B03002_001"
  var_RE_nhw <- "B03002_003"   # non-Hispanic White alone
  var_RE_nhb <- "B03002_004"   # non-Hispanic Black alone

  vars <- c(var_INC_tot, var_INC_low, var_INC_high,
            var_INCW_low, var_INCW_high, var_INCB_low,
            var_EDU_tot, var_EDU_ltHS, var_EDU_BDup,
            var_RE_tot, var_RE_nhw, var_RE_nhb)

  # Keep track of the original names for the calculation step
  acs_vars <- list(
    vars         = paste0(vars, "E"),
    var_INC_tot  = paste0(var_INC_tot, "E"),
    var_INC_low  = paste0(var_INC_low, "E"),
    var_INC_high = paste0(var_INC_high, "E"),
    var_INCW_low  = paste0(var_INCW_low, "E"),
    var_INCW_high = paste0(var_INCW_high, "E"),
    var_INCB_low  = paste0(var_INCB_low, "E"),
    var_EDU_tot  = paste0(var_EDU_tot, "E"),
    var_EDU_ltHS = paste0(var_EDU_ltHS, "E"),
    var_EDU_BDup = paste0(var_EDU_BDup, "E"),
    var_RE_tot = paste0(var_RE_tot, "E"),
    var_RE_nhw = paste0(var_RE_nhw, "E"),
    var_RE_nhb = paste0(var_RE_nhb, "E")
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

  ice_data_raw <- do.call(tidycensus::get_acs, fetch_args)

  # Attach var names as an attribute for the next step
  ice_data_raw <- ice_data_raw |>
    dplyr::select(GEOID, NAME, dplyr::ends_with('E'), dplyr::ends_with('M'))
  attr(ice_data_raw, "acs_vars") <- acs_vars
  return(ice_data_raw)
}
