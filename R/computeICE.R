#' Compute the Index of Concentration at the Extremes (ICE)
#'
#' @description
#' Fetches and processes ACS data to calculate Krieger's Index of Concentration
#' at the Extremes (ICE), following the implementation of `ndi::krieger`.
#'
#' Unlike the Neighborhood Deprivation Index measures in \code{\link{computeNDI}}
#' (which rank or factor-analyze geographies against one another), each ICE
#' measure is a self-contained per-geography ratio of the form
#' \eqn{(Advantaged - Deprived) / Total}. Because it involves no cross-geography
#' ranking or principal component analysis, ICE has no `scope` argument.
#'
#' Five measures are returned:
#' \itemize{
#'   \item `ICE_inc`: income concentration (high-income minus low-income households)
#'   \item `ICE_edu`: education concentration (bachelor's+ minus less-than-high-school, 25+)
#'   \item `ICE_rewb`: race/ethnicity concentration (non-Hispanic White minus
#'     non-Hispanic Black)
#'   \item `ICE_wbinc`: racialized economic segregation (high-income non-Hispanic
#'     White minus low-income Black households)
#'   \item `ICE_wpcinc`: income concentration among non-Hispanic White households
#' }
#' Each ranges from -1 (most deprived/segregated) to +1 (most privileged).
#'
#' All ICE variables come from ACS detailed (`B`) tables, which are published
#' down to the block-group level; ICE is therefore available for `geo =
#' "county"`, `"tract"`, or `"block group"` (unlike `ndi::krieger`, which is
#' limited to county and tract).
#'
#' @param geo The geography level. One of `"county"`, `"tract"`, or
#'   `"block group"` (or its alias `"cbg"`).
#' @param year The 5-year ACS survey year (e.g., 2020). Must be >= 2011, and
#'   >= 2013 for census block group. The income-bracket cutpoints switch at
#'   2016 (see \code{\link{calculateICEVars}}).
#' @param states A vector of state abbreviations (e.g., `c("CA", "NY")`) or
#'   `'all'` (default) for the entire US.
#' @param impute Logical. If `TRUE`, missing values in each of the five ICE
#'   measures are spatially imputed from Queen-contiguity neighbors via
#'   \code{\link{imputeMissingICE}} (geometry is required, so `keep_geometry` is
#'   forced to `TRUE`). Only geographies with population (`tot_pop > 0`) are
#'   imputed or used as neighbors; a geography with no population keeps its raw
#'   measures and is annotated `"No population"`. The returned `df_ice` then carries the imputed measures
#'   plus `nvar_imputed`, `nvar_still_missing`, `vars_imputed`, and an
#'   `annotation` column, and the pre-imputation baseline is also returned as
#'   `df_ice_raw`. Defaults to `FALSE`.
#' @param keep_geometry Logical. If `TRUE` (default), fetches geometry and
#'   returns it in `df_geometry`. Forced to `TRUE` when `impute = TRUE`.
#' @param quiet Logical. If `TRUE`, suppresses messages. Defaults to `FALSE`.
#' @param return_format Return `"detailed"` (default) or `"minimal"`.
#'   `"minimal"` returns a single tibble of `GEOID`, geography name columns, and
#'   the five `ICE_*` measures.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' When `return_format = "minimal"`, a tibble with `GEOID`, geography name
#' columns, and the five `ICE_*` measures, arranged by `GEOID`. When
#' `impute = TRUE`, the five measures are imputed and the tibble also carries
#' `nvar_imputed`, `nvar_still_missing`, `vars_imputed`, and `annotation`.
#'
#' When `return_format = "detailed"` (default), a list containing:
#' \itemize{
#'   \item `df_ice`: `GEOID`, geography name columns, and the five `ICE_*`
#'     measures. When `impute = TRUE`, the measures are imputed and the frame
#'     also includes `nvar_imputed`, `nvar_still_missing`, `vars_imputed`, and
#'     `annotation`.
#'   \item `df_raw_values`: the building-block component sums and totals.
#'   \item `df_geometry`: `GEOID` and geometry (when `keep_geometry = TRUE`).
#'   \item `missing`: a per-measure missingness summary (of the raw measures).
#'   \item `df_ice_raw`: the pre-imputation baseline measures (only when
#'     `impute = TRUE`).
#' }
#'
#' @references
#' Krieger, N., Waterman, P.D., Spasojevic, J., et al. (2016). Public health
#' monitoring of privilege and deprivation with the Index of Concentration at
#' the Extremes. American Journal of Public Health, 106(2), 256-263.
#'
#' Methodology ported from the `ndi` package by Ian D. Buller
#' (\url{https://CRAN.R-project.org/package=ndi}).
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr select any_of all_of arrange mutate n group_by summarise case_when
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Requires a Census API Key
#'   # tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
#'
#'   # ICE for Rhode Island tracts, 2022
#'   ice_ri <- computeICE(geo = "tract", year = 2022, states = "RI")
#'   head(ice_ri$df_ice)
#'
#'   # Block-group ICE (not possible for Powell-Wiley), minimal output
#'   ice_bg <- computeICE(geo = "block group", year = 2022, states = "RI",
#'                        return_format = "minimal")
#' }

computeICE <- function(
    geo           = "tract",
    year          = 2020,
    states        = "all",
    impute        = FALSE,
    keep_geometry = TRUE,
    quiet         = FALSE,
    return_format = "detailed",
    ...
) {

  geo <- rlang::arg_match(geo, values = c("county", "tract", "block group", "cbg"))
  return_format <- rlang::arg_match(return_format, values = c("minimal", "detailed"))

  if (!is.numeric(year) || year < 2011) {
    stop(glue::glue("'year' must be a number >= 2011. Got: {year}"))
  }

  # Normalize the block-group alias so tidycensus receives a valid geography.
  if (geo == "cbg") geo <- "block group"

  if (geo == "block group" && year < 2013) {
    stop(glue::glue(
      "geo = 'block group' requires year >= 2013. Got year = {year}."
    ))
  }

  # Spatial imputation needs geometry to build the neighbor graph.
  if (isTRUE(impute) && !isTRUE(keep_geometry)) {
    if (!quiet) message("impute = TRUE requires geometry; setting keep_geometry = TRUE.")
    keep_geometry <- TRUE
  }

  # Resolve states (same validation pattern as computeNDI)
  all_geos <- unique(tidycensus::fips_codes$state)
  non_states <- c("PR", "AS", "GU", "MP", "VI", "UM")
  valid_states <- setdiff(all_geos, non_states)
  if (states[1] == "all") {
    states <- valid_states
  } else {
    invalid_states <- setdiff(states, valid_states)
    if (length(invalid_states) > 0) {
      invalid_states_str <- glue::glue_collapse(invalid_states, sep = ", ")
      stop(glue::glue("Invalid state abbreviations provided: {invalid_states_str}.\n Please use standard 2-letter postal codes for the 50 US states or DC."))
    }
  }

  if (!quiet) message("Fetching raw ACS data...")
  ice_data_raw <- fetchICEAcs(geo, year, states, keep_geometry)
  acs_vars <- attr(ice_data_raw, "acs_vars")
  ice_data_clean <- cleanAcsNames(dframe = ice_data_raw, geo = geo)

  if (!quiet) message("Calculating ICE measures...")
  ice_data <- calculateICEVars(ice_data_clean, acs_vars = acs_vars, year = year)

  ice_cols   <- c("ICE_inc", "ICE_edu", "ICE_rewb", "ICE_wbinc", "ICE_wpcinc")
  block_cols <- c("A_inc", "P_inc", "TotalPop_inc", "A_edu", "P_edu",
                  "TotalPop_edu", "NHoLW", "NHoLB", "TotalPop_re",
                  "A_wbinc", "P_wbinc", "A_wpcinc", "P_wpcinc", "tot_pop")
  name_cols  <- c("block_group", "tract", "county", "state")

  # df_ice_raw: baseline (no imputation) — GEOID, name columns, five ICE measures
  df_ice_raw <- ice_data |>
    dplyr::select(GEOID, dplyr::any_of(name_cols), dplyr::all_of(ice_cols))
  if (inherits(df_ice_raw, "sf")) df_ice_raw <- sf::st_drop_geometry(df_ice_raw)
  df_ice_raw <- dplyr::arrange(df_ice_raw, GEOID)

  # missing: per-measure missingness summary (always on the raw source measures)
  missing <- df_ice_raw |>
    dplyr::select(dplyr::all_of(ice_cols)) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "measure", values_to = "val") |>
    dplyr::group_by(measure) |>
    dplyr::summarise(
      total = dplyr::n(),
      n_missing = sum(is.na(val)),
      percent_missing = paste0(round(mean(is.na(val)) * 100, 2), " %"),
      .groups = "drop"
    )

  # df_ice: imputed when impute = TRUE, otherwise the baseline
  if (isTRUE(impute)) {
    if (!inherits(ice_data, "sf") || !"geometry" %in% colnames(ice_data)) {
      stop("Imputation requires geometry; re-run with keep_geometry = TRUE.")
    }
    if (!quiet) message("Imputing missing ICE measures via spatial neighbors...")

    # Only geographies with population are imputed, and only they serve as
    # neighbors (as in computeNDI). The others keep their raw measures.
    has_pop <- !is.na(ice_data$tot_pop) & ice_data$tot_pop > 0
    if (!quiet) message(glue::glue("... there are {sum(!has_pop)} geoids with no population."))

    ice_no_pop <- sf::st_drop_geometry(ice_data[!has_pop, ])
    ice_no_pop$nvar_imputed       <- rep(0, nrow(ice_no_pop))
    ice_no_pop$nvar_still_missing <- rowSums(is.na(ice_no_pop[, ice_cols, drop = FALSE]))
    ice_no_pop$vars_imputed       <- rep(NA_character_, nrow(ice_no_pop))

    if (any(has_pop)) {
      ice_imp <- imputeMissingICE(ice_data[has_pop, ], weight_var = "none",
                                  measures = ice_cols, quiet = quiet)
      ice_imp <- dplyr::bind_rows(ice_imp, ice_no_pop)
    } else {
      ice_imp <- ice_no_pop
    }

    df_ice <- ice_imp |>
      dplyr::select(GEOID, dplyr::any_of(name_cols), dplyr::all_of(ice_cols),
                    tot_pop, nvar_imputed, nvar_still_missing, vars_imputed) |>
      dplyr::mutate(
        annotation = dplyr::case_when(
          tot_pop == 0           ~ "No population",
          nvar_still_missing > 0 ~ "Imputation incomplete",
          nvar_imputed > 0       ~ "Imputation completed",
          TRUE                   ~ "Complete data"
        )
      ) |>
      dplyr::select(-tot_pop) |>
      dplyr::arrange(GEOID)
  } else {
    df_ice <- df_ice_raw
  }

  if (return_format == "minimal") {
    return(df_ice)
  }

  # df_raw_values: building-block sums and totals (no geometry)
  df_raw_values <- ice_data |>
    dplyr::select(GEOID, dplyr::any_of(name_cols), dplyr::all_of(block_cols))
  if (inherits(df_raw_values, "sf")) df_raw_values <- sf::st_drop_geometry(df_raw_values)
  df_raw_values <- dplyr::arrange(df_raw_values, GEOID)

  # df_geometry
  df_geometry <- ice_data |>
    dplyr::select(GEOID, dplyr::any_of("geometry"))

  out <- list(
    df_ice = df_ice,
    df_raw_values = df_raw_values,
    df_geometry = df_geometry,
    missing = missing
  )
  # When imputing, also return the pre-imputation baseline for comparison.
  if (isTRUE(impute)) out$df_ice_raw <- df_ice_raw

  return(out)
}
