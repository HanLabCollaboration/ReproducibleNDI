#' Compute the Neighborhood Deprivation Index (NDI)
#'
#' @description
#' Fetches and processes ACS data to calculate the Neighborhood Deprivation
#' Index (NDI), a composite measure of neighborhood-level socioeconomic
#' deprivation.
#'
#' The `type` parameter selects the NDI methodology. Currently implemented:
#' \itemize{
#'   \item `"messer"`: Messer et al. (2006), following the implementation of
#'     `ndi::messer`. A single principal component is extracted from the
#'     correlation matrix of 8 ACS-derived component variables (OCC, CWD,
#'     POV, FHH, PUB, U30, EDU, EMP), and the NDI score is
#'     `PC1 / eigenvalue^2`. Unlike `ndi::messer`, the component sign is
#'     aligned so that `POV` loads positively (higher NDI = more
#'     deprivation), which keeps the orientation consistent across scope
#'     groups.
#' }
#' `"powell_wiley"` is reserved and not yet implemented.
#'
#' The function automates: 1) data fetching from `tidycensus`, 2) calculation
#' of the method's component variables, 3) optional spatial mean imputation
#' for missing values using queen contiguity, and 4) the principal component
#' analysis to compute the final composite index.
#'
#' The output always includes a baseline NDI (`df_ndi_raw`, no imputation)
#' alongside the requested version (`df_ndi`), so results can be directly
#' compared. The requested NDI column is named dynamically: `NDI` or
#' `NDIImputed`.
#'
#' @param type The NDI methodology. Must be `"messer"` (default).
#'   `"powell_wiley"` is reserved for a future release.
#' @param geo The geography level to compute the index for. Must be one of
#'   `"county"`, `"tract"`, or `"block group"` (or `"cbg"`).
#' @param year The 5-year ACS survey year (e.g., 2022). Must be >= 2011, and
#'   must be >= 2013 for census block group.
#' @param states A vector of state abbreviations (e.g., `c("CA", "NY")`) or
#'   `'all'` to run for the entire US.
#' @param impute Logical. If `TRUE`, performs a simple spatial mean imputation
#'   for missing data using 'queen' contiguity.
#' @param scope Calculate the index using a `"national"` (default), `"state"`, or
#'   `"county"` scope. `"national"` fits one PCA over all geographies
#'   (automatically sets `states = 'all'`). `"state"` fits the PCA within
#'   each state separately. `"county"` fits the PCA within each county
#'   separately (only valid for `geo = "tract"` or `"block group"`).
#' @param keep_geometry Logical. If `TRUE` (default), attaches geometry to
#'   `df_geometry` in the returned list. Geometry is always fetched when
#'   `impute = TRUE`.
#' @param quiet Logical. If `TRUE`, suppresses all messages and warnings.
#' @param return_format Return `"detailed"` (default) or `"minimal"`.
#'   `"minimal"` returns only `GEOID`, the requested NDI column, its quartile,
#'   and `annotation`.
#' @param weight_var The variable to use for weighted spatial imputation.
#'   Must be `"tot_pop"` (default) or `"none"`. Passed to `imputeMissing()` as
#'   `weight_var_sub`. Only relevant when `impute = TRUE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' A list containing the following elements:
#' \itemize{
#'   \item `df_ndi_raw`: Baseline NDI (no imputation) with `NDI` and
#'     `NDIQuartile`. Identical to `df_ndi` when no imputation is requested.
#'   \item `df_ndi`: Requested NDI with dynamically named columns depending on
#'     parameters: `NDI` or `NDIImputed`, plus the corresponding quartile column.
#'   \item `df_raw_values`: Raw input data without geometry.
#'   \item `df_geometry`: Data frame containing only GEOID and geometry.
#'   \item `df_imputed`: Imputed data without geometry.
#'   \item `df_components`: Component variables used for the PCA.
#'   \item `obj_pca`: The `psych::principal` object (or a list of objects if
#'     `scope = "state"` or `scope = "county"`).
#' }
#'
#' @references
#' Messer, L.C., Laraia, B.A., Kaufman, J.S., et al. (2006). The development
#' of a standardized neighborhood deprivation index. Journal of Urban Health,
#' 83(6), 1041-1062.
#'
#' Methodology ported from the `ndi` package by Ian D. Buller
#' (\url{https://CRAN.R-project.org/package=ndi}).
#'
#' @importFrom glue glue
#' @importFrom dplyr group_split select any_of arrange rename mutate case_when
#' @importFrom purrr map map_dfr map_chr keep
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Requires a Census API Key
#'   # tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
#'
#'   # Compute Messer NDI for tracts in California for 2022
#'   ndi_ca_tracts <- computeNDI(
#'     type   = "messer",
#'     geo    = "tract",
#'     year   = 2022,
#'     states = "CA",
#'     impute = TRUE,
#'     scope  = "state"
#'   )
#'
#'   # Compare raw vs requested side by side
#'   head(ndi_ca_tracts$df_ndi_raw)   # NDI, NDIQuartile
#'   head(ndi_ca_tracts$df_ndi)       # NDIImputed, NDIImputedQuartile
#'
#'   # Compute index for all counties in New England (state scope)
#'   ndi_ne <- computeNDI(
#'     geo    = "county",
#'     year   = 2022,
#'     states = c("ME", "NH", "VT", "MA", "RI", "CT"),
#'     scope  = "state",
#'     return_format = "minimal"
#'   )
#' }

computeNDI <- function(
    type          = "messer",
    geo           = "tract",
    year          = 2022,
    states        = "all",
    impute        = FALSE,
    scope         = "national",
    keep_geometry = TRUE,
    quiet         = FALSE,
    return_format = "detailed",
    weight_var    = "tot_pop",
    ...
) {

type <- rlang::arg_match(type, values = c("messer", "powell_wiley"))
if (type == "powell_wiley") {
  stop("type = 'powell_wiley' is not yet implemented. Use type = 'messer'.")
}
geo <- rlang::arg_match(geo, values = c("county", "tract",
                                        "block group", "cbg"))
return_format <- rlang::arg_match(return_format, values = c("minimal",
                                                            "detailed"))
scope <- rlang::arg_match(scope, values = c("national", "state", "county"))
weight_var <- rlang::arg_match(weight_var, values = c("tot_pop", "none"))
if (!is.numeric(year) || year < 2011) {
  stop(glue::glue("'year' must be a number >= 2011. Got: {year}"))
}

if (scope == "county" && !geo %in% c("tract", "block group", "cbg")) {
  stop("scope = 'county' can only be used with geo = 'tract', 'block group', or 'cbg'")
}
if (scope == "national") {
  message(glue::glue("Since the scope == 'national', it will pull all states"))
  states <- "all"
}
# block group requires year >= 2013
if (geo %in% c("block group", "cbg") && year < 2013) {
  stop(glue::glue(
    "geo = 'block group' requires year >= 2013. Got year = {year}."
  ))
}

# --- Method dispatch: each NDI type has its own fetch/calculate/process workers ---
fetch_fun     <- switch(type, messer = fetchMesserAcs)
calculate_fun <- switch(type, messer = calculateMesserVars)
process_fun   <- switch(type, messer = processMesserScope)
varlist       <- switch(type, messer = c("OCC", "CWD", "POV", "FHH",
                                         "PUB", "U30", "EDU", "EMP"))

get_geometry <- impute || keep_geometry
if (states[1] == "all") {
  all_geos <- unique(tidycensus::fips_codes$state)
  non_states <- c("PR", "AS", "GU", "MP", "VI", "UM")
  states <- setdiff(all_geos, non_states)
}
else {
  all_geos <- unique(tidycensus::fips_codes$state)
  non_states <- c("PR", "AS", "GU", "MP", "VI", "UM")
  valid_states <- setdiff(all_geos, non_states)
  invalid_states <- setdiff(states, valid_states)
  if (length(invalid_states) > 0) {
    invalid_states_str <- glue::glue_collapse(invalid_states,
                                              sep = ", ")
    stop(glue::glue("Invalid state abbreviations provided: {invalid_states_str}.\n Please use standard 2-letter postal codes for the 50 US states or DC."))
  }
}
if (!quiet)
  message("Fetching raw ACS data...")
ndi_data_raw <- fetch_fun(geo, year, states, get_geometry)
acs_vars <- attr(ndi_data_raw, "acs_vars")
ndi_data_clean <- cleanAcsNames(dframe = ndi_data_raw,
                                geo = geo)
if (!quiet)
  message(glue::glue("Calculating {type} NDI component variables..."))
ndi_data <- dplyr::select(
  calculate_fun(ndi_data_clean, acs_vars = acs_vars), GEOID,
  NAME, dplyr::any_of(c("block_group", "tract", "county",
                        "state")), tot_pop, dplyr::all_of(varlist),
  dplyr::any_of("geometry"))
ndi_col <- dplyr::case_when(impute ~ "NDIImputed",
                            TRUE ~ "NDI")
ndi_quartile_col <- paste0(ndi_col, "Quartile")
if (!quiet)
  message(switch(scope, state = "Scope is 'state'. Processing each state separately",
                 county = "Scope is 'county'. Processing each county separately",
                 national = "Scope is 'national'. Imputing, harmonizing and computing PCA for all geoIDs."))
if (scope == "state") {
  list_of_state_data <- dplyr::group_split(ndi_data, state)
  results_list <- purrr::map(list_of_state_data, ~process_fun(
    ndi_data_sub = .x,
    impute_sub = impute, quiet_sub = quiet,
    return_format_sub = return_format,
    weight_var_sub = weight_var))

  results_list <- purrr::keep(results_list, ~!is.null(.x))
  df_ndi <- purrr::map_dfr(results_list, "df_ndi")
  df_raw_values <- purrr::map_dfr(results_list, "df_raw_values")
  df_geometry <- purrr::map_dfr(results_list, "df_geometry")
  df_imputed <- purrr::map_dfr(results_list, "df_imputed")
  df_components <- purrr::map_dfr(results_list, "df_components")
  list_of_pca_objects <- purrr::map(results_list, "pca_object")
  names(list_of_pca_objects) <- purrr::map_chr(results_list,
                                               ~unique(.x$df_raw_values$state))
  out_obj_pca <- list_of_pca_objects
}
else if (scope == "county") {
  ndi_data <- dplyr::mutate(ndi_data, county_state = paste(county,
                                                           state, sep = ", "))
  list_of_county_data <- dplyr::group_split(ndi_data,
                                            county_state)
  results_list <- purrr::map(list_of_county_data, ~process_fun(
    ndi_data_sub = .x,
    impute_sub = impute, quiet_sub = quiet,
    return_format_sub = return_format,
    weight_var_sub = weight_var))

  results_list <- purrr::keep(results_list, ~!is.null(.x))
  df_ndi <- purrr::map_dfr(results_list, "df_ndi")
  df_raw_values <- purrr::map_dfr(results_list, "df_raw_values")
  df_geometry <- purrr::map_dfr(results_list, "df_geometry")
  df_imputed <- purrr::map_dfr(results_list, "df_imputed")
  df_components <- purrr::map_dfr(results_list, "df_components")
  list_of_pca_objects <- purrr::map(results_list, "pca_object")
  names(list_of_pca_objects) <- purrr::map_chr(results_list,
                                               ~unique(.x$df_raw_values$county_state))
  out_obj_pca <- list_of_pca_objects
}
else {
  single_result <- process_fun(
    ndi_data_sub = ndi_data,
    impute_sub = impute, quiet_sub = quiet,
    return_format_sub = return_format,
    weight_var_sub = weight_var)

  df_ndi <- single_result$df_ndi
  df_raw_values <- single_result$df_raw_values
  df_geometry <- single_result$df_geometry
  df_imputed <- single_result$df_imputed
  df_components <- single_result$df_components
  out_obj_pca <- single_result$pca_object
}
df_ndi <- dplyr::rename(df_ndi, `:=`(!!ndi_col, NDI),
                        `:=`(!!ndi_quartile_col, NDIQuartile))
if (impute) {
  if (!quiet)
    message("Computing baseline NDI (no imputation) for df_ndi_raw...")
  if (scope == "state") {
    list_of_state_data <- dplyr::group_split(ndi_data, state)
    results_list_raw <- purrr::map(
      list_of_state_data,
      ~process_fun(ndi_data_sub = .x,
                   impute_sub = FALSE, quiet_sub = TRUE,
                   return_format_sub = return_format,
                   weight_var_sub = weight_var))
    results_list_raw <- purrr::keep(results_list_raw,
                                    ~!is.null(.x))
    df_ndi_raw <- purrr::map_dfr(results_list_raw, "df_ndi")
  }
  else if (scope == "county") {
    list_of_county_data <- dplyr::group_split(ndi_data,
                                              county_state)
    results_list_raw <- purrr::map(
      list_of_county_data,
      ~process_fun(ndi_data_sub = .x,
                   impute_sub = FALSE, quiet_sub = TRUE,
                   return_format_sub = return_format,
                   weight_var_sub = weight_var))
    results_list_raw <- purrr::keep(results_list_raw,
                                    ~!is.null(.x))
    df_ndi_raw <- purrr::map_dfr(results_list_raw, "df_ndi")
  }
  else {
    baseline_result <- process_fun(
      ndi_data_sub = ndi_data,
      impute_sub = FALSE, quiet_sub = TRUE,
      return_format_sub = return_format,
      weight_var_sub = weight_var)
    df_ndi_raw <- baseline_result$df_ndi
  }
}
else {
  df_ndi_raw <- df_ndi
}
if (return_format == "minimal") {
  return(dplyr::arrange(df_ndi, GEOID))
}
out <- list(df_ndi_raw = dplyr::arrange(df_ndi_raw, GEOID),
            df_ndi = dplyr::arrange(df_ndi, GEOID), df_raw_values = df_raw_values,
            df_geometry = df_geometry, df_imputed = df_imputed, df_components = df_components,
            obj_pca = out_obj_pca)
return(out)
}
