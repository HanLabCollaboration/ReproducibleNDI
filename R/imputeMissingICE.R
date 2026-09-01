#' Impute Missing ICE Measures using Spatial Neighbors
#'
#' @description
#' Spatially imputes missing values for the Index of Concentration at the
#' Extremes (ICE) measures returned by \code{\link{computeICE}}, using the mean
#' (or population-weighted mean) of contiguous Queen-neighbor geographies.
#'
#' Unlike \code{\link{imputeMissing}} — which imputes the component variables
#' that feed a single principal-component index — each ICE measure is a
#' self-contained per-geography ratio. This function therefore imputes each of
#' the five \code{ICE_*} measures \strong{directly and independently}, applying
#' the same spatial-neighbor procedure as \code{\link{imputeMissing}} to each.
#'
#' @param dfGeo An `sf` data frame. Must contain `GEOID`, a `geometry` column,
#'   and one or more ICE measure columns (see `measures`). Any other columns
#'   (geography names, building-block totals, the weight variable) are passed
#'   through unchanged.
#' @param weight_var A string naming the column to weight the neighbor mean by
#'   (e.g. `"tot_pop"`). `"none"` (default), `NULL`, or `""` gives an
#'   unweighted arithmetic mean.
#' @param measures Character vector of measure columns to impute. Defaults to
#'   whichever of the five ICE measures (`ICE_inc`, `ICE_edu`, `ICE_rewb`,
#'   `ICE_wbinc`, `ICE_wpcinc`) are present in `dfGeo`.
#' @param quiet Logical. If `FALSE` (default), prints progress messages.
#'
#' @return A `tibble` (geometry dropped) containing every column of `dfGeo`,
#'   with the requested ICE `measures` imputed in place, plus three metadata
#'   columns: `nvar_imputed` and `nvar_still_missing` (counts across the ICE
#'   measures for each geography), and `vars_imputed` (the names of the measures
#'   that were imputed for that geography, collapsed with `_`; `NA` when none).
#'
#' @seealso \code{\link{imputeMissing}}, \code{\link{computeICE}}
#'
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom dplyr select all_of left_join
#' @importFrom glue glue glue_collapse
#' @export
imputeMissingICE <- function(dfGeo, weight_var = "none", measures = NULL, quiet = FALSE) {

  # --- 1. Validation & measure detection ---
  if (!all(c("GEOID", "geometry") %in% colnames(dfGeo))) {
    stop("Data frame must contain 'GEOID' and 'geometry' columns.")
  }
  if (!inherits(dfGeo, "sf")) dfGeo <- sf::st_as_sf(dfGeo)

  ice_default <- c("ICE_inc", "ICE_edu", "ICE_rewb", "ICE_wbinc", "ICE_wpcinc")
  if (is.null(measures)) {
    measures <- intersect(ice_default, colnames(dfGeo))
  }
  if (length(measures) == 0) {
    stop("No ICE measure columns found to impute. Pass `measures` explicitly.")
  }
  missing_cols <- setdiff(measures, colnames(dfGeo))
  if (length(missing_cols) > 0) {
    stop(glue::glue(
      "Measure column(s) not found in data: ",
      "{glue::glue_collapse(missing_cols, sep = ', ')}."
    ))
  }

  use_weights <- !is.null(weight_var) && weight_var != "none" && weight_var != ""
  if (use_weights && !weight_var %in% colnames(dfGeo)) {
    stop(glue::glue("Weight variable '{weight_var}' not found in data."))
  }

  if (!quiet) {
    message(glue::glue(
      "Imputing {length(measures)} ICE measure(s): ",
      "{glue::glue_collapse(measures, sep = ', ')}."
    ))
  }

  # --- 2. Impute ONLY the ICE measures via the shared spatial routine ---
  # imputeMissing() imputes every non-meta column it is handed, so pass it a
  # minimal sf carrying just GEOID, geometry, the measures, and (optionally) the
  # weight column. This guarantees the ICE measures are imputed with exactly the
  # same neighbor procedure as imputeMissing(), and nothing else is touched.
  keep_cols <- c("GEOID", measures, if (use_weights) weight_var)
  df_min <- dfGeo |> dplyr::select(dplyr::all_of(keep_cols))  # sf keeps geometry

  imp <- imputeMissing(df_min, weight_var_sub = weight_var, quiet = quiet)

  # --- 3. Re-attach pass-through columns (names, building blocks, etc.) ---
  passthrough <- dfGeo |>
    sf::st_drop_geometry() |>
    dplyr::select(-dplyr::all_of(measures))

  out <- passthrough |>
    dplyr::left_join(
      imp |> dplyr::select(dplyr::all_of(c("GEOID", measures)),
                           nvar_imputed, nvar_still_missing),
      by = "GEOID"
    )

  # --- 4. Record WHICH measures were imputed (NA before, filled now) ---
  # `vars_imputed` names the imputed measures for each geography, collapsed with
  # "_"; NA when none were imputed. (Measure names contain underscores, so this
  # is a human-readable label rather than a machine-parseable list.)
  before_na <- is.na(sf::st_drop_geometry(dfGeo)[, measures, drop = FALSE])
  rownames(before_na) <- dfGeo$GEOID
  before_na <- before_na[match(out$GEOID, rownames(before_na)), , drop = FALSE]
  after_na  <- is.na(out[, measures, drop = FALSE])
  imputed_mat <- before_na & !after_na

  out$vars_imputed <- vapply(seq_len(nrow(out)), function(i) {
    m <- measures[imputed_mat[i, ]]
    if (length(m) == 0) NA_character_ else paste(m, collapse = "_")
  }, character(1))

  return(out)
}
