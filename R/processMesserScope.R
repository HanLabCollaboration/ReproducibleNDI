#' Process a single scope for the Messer NDI
#'
#' @description
#' This is the primary internal workhorse function for `computeNDI` with
#' `type = "messer"`. It processes a single "scope" (e.g., all national data,
#' or data for a single state).
#'
#' It handles:
#' 1. Filtering geographies with no population.
#' 2. Optionally imputing missing data (spatial neighbors, queen contiguity).
#' 3. Running the principal component analysis (`psych::principal`) with
#'    robust error handling, following the Messer methodology
#'    (1 component on the correlation matrix of the 8 raw component variables).
#' 4. Calculating the final NDI score (`PC1 / eigenvalue^2`, as in `ndi::messer`)
#'    and quartiles.
#' 5. Annotating the final data based on data quality/imputation status.
#'
#' Note: unlike `ndi::messer`, the sign of the component is aligned so that
#' `POV` (proportion of households below poverty) loads positively - i.e.,
#' higher NDI always means more deprivation. This guarantees consistent
#' orientation when the PCA is fit separately per scope group
#' (`scope = "state"` or `"county"`).
#'
#' @param ndi_data_sub An `sf` data frame subset for a single scope (e.g., one state).
#' @param impute_sub Logical, whether to perform spatial imputation.
#' @param quiet_sub Logical, whether to suppress messages and warnings.
#' @param return_format_sub The desired return format (minimal or detailed).
#'   (Note: This is handled by the parent `computeNDI` function).
#' @param weight_var_sub A string. The name of the column to use for weighting.
#'
#' @return
#' A list containing six elements:
#' * `df_ndi`: Data frame with GEOID, NDI, NDIQuartile, and annotation (no geometry).
#' * `df_raw_values`: The raw input data without geometry.
#' * `df_geometry`: Data frame containing only GEOID and geometry.
#' * `df_imputed`: The imputed data without geometry.
#' * `df_components`: The component variables used for the PCA.
#' * `pca_object`: The `psych::principal` object, or `NULL` if the analysis failed.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter select any_of mutate across all_of
#' @importFrom dplyr left_join case_when ntile
#' @importFrom glue glue
#' @importFrom psych principal
#' @importFrom stats quantile

processMesserScope <- function(
    ndi_data_sub,
    impute_sub,
    quiet_sub,
    return_format_sub,
    weight_var_sub = NULL) {

  varlist <- c("OCC", "CWD", "POV", "FHH", "PUB", "U30", "EDU", "EMP")

  # Holding the input before manipulating
  raw_ndi_data_sub <- ndi_data_sub

  # Filter out no-population areas for this subset
  ndi_data_sub <- ndi_data_sub |> dplyr::filter(tot_pop > 0)

  rows_with_no_pop <- nrow(raw_ndi_data_sub) - nrow(ndi_data_sub)
  if (!quiet_sub) message(glue::glue("... there are {rows_with_no_pop} geoids with no population."))
  if (nrow(ndi_data_sub) == 0) return(NULL) # Skip if a scope has no valid data

  ndi_input_data <- ndi_data_sub |>
    dplyr::select(GEOID, tot_pop, dplyr::all_of(varlist), dplyr::any_of("geometry"))

  max_missing <- ndi_input_data |>
    dplyr::select(-GEOID) |>
    sf::st_drop_geometry() |>
    is.na() |>
    rowSums() |>
    max()

  if (impute_sub & max_missing > 0) { ## Impute --------
    if (!quiet_sub) message(glue::glue("... imputing missing data."))

    if (weight_var_sub == 'tot_pop') message(glue::glue("... imputation will be weighted by total population."))
    if (weight_var_sub == 'none') message(glue::glue("... imputation will average the neighbors."))

    ndi_input_data <- imputeMissing(
      dfGeo = ndi_input_data,
      weight_var_sub = weight_var_sub,
      quiet = quiet_sub
    )

  } else { ## Complete Cases only --------

    ndi_input_data <- ndi_input_data |>
      dplyr::mutate(
        nvar_still_missing = rowSums(
          is.na(dplyr::across(dplyr::all_of(varlist))), na.rm = FALSE
        ),
        nvar_imputed = 0
      )
  }

  # Only keep complete rows from now on.
  ndi_inp_data_pca <- ndi_input_data |>
    dplyr::filter(nvar_still_missing == 0) |>
    dplyr::select(GEOID, dplyr::all_of(varlist)) |>
    sf::st_drop_geometry()

  # ====================================================================
  # === Principal Component Analysis with Error Handling ===
  # ====================================================================

  # Get metadata for our warning message
  current_scope <- "Current Scope" # Default
  if ("state" %in% colnames(raw_ndi_data_sub)) {
    # Get the state name from the *original* data for this scope
    current_scope <- unique(raw_ndi_data_sub$state)[1]
  }

  n_obs <- nrow(ndi_inp_data_pca)
  # -1 for GEOID column
  n_vars <- ncol(ndi_inp_data_pca) - 1

  ndi_pca <- NULL # Initialize as NULL

  # 1. Pre-check: Skip if not enough observations
  if (n_obs < n_vars + 2) { # Rule of thumb: need more obs than vars
    if (!quiet_sub) {
      warning(glue::glue(
        "Skipping PCA for '{current_scope}': ",
        "Insufficient complete observations ({n_obs}) for {n_vars} variables."
      ))
    }
  } else {
    # 2. tryCatch: Attempt the PCA (Messer: 1 component, correlation matrix)
    set.seed(5757)
    ndi_pca <- tryCatch({
      psych::principal(
        ndi_inp_data_pca |> dplyr::select(-GEOID),
        nfactors = 1, n.obs = n_obs, covar = FALSE, scores = TRUE
      )
    }, error = function(e) {
      if (!quiet_sub) {
        warning(glue::glue(
          "PCA failed for '{current_scope}' ",
          "which had {n_obs} complete observations. ",
          "Error: {e$message}"
        ))
      }
      return(NULL) # Return NULL on failure
    })
  }

  # 3. Post-check: Handle the result (success or failure)
  if (is.null(ndi_pca)) {
    # PCA failed or was skipped, so NDI score is NA
    ndi_inp_data_pca$NDI <- NA_real_

  } else {
    # PCA succeeded, calculate NDI score (Messer scaling: PC1 / eigenvalue^2)
    ndi_scores <- ndi_pca$scores[, 1] / ndi_pca$values[1]^2

    ## making sure the component aligns with poverty (higher NDI = more deprivation)
    pos_pov <- grepl('POV', rownames(ndi_pca$loadings)) |> which()
    factor_sign <- ifelse(ndi_pca$loadings[pos_pov] < 0, -1, 1)
    ndi_inp_data_pca$NDI <- ndi_scores * factor_sign
  }

  # Step 5: Preparing output
  ndi_output_data <- raw_ndi_data_sub |>
    dplyr::left_join(
      ndi_inp_data_pca |> dplyr::select(GEOID, NDI), by = "GEOID"
    ) |>
    dplyr::left_join(
      ndi_input_data |>
        dplyr::select(GEOID, nvar_still_missing, nvar_imputed) |> sf::st_drop_geometry(), by = "GEOID"
    )

  # Quartiles (ndi::messer convention: cut at quantiles, not ntile)
  quartile_breaks <- stats::quantile(ndi_output_data$NDI,
                                     probs = c(0, 0.25, 0.5, 0.75, 1),
                                     na.rm = TRUE)
  if (anyNA(quartile_breaks) || any(duplicated(quartile_breaks))) {
    # Fallback when breaks are degenerate (e.g., all-NA NDI or heavy ties)
    ndi_output_data <- ndi_output_data |>
      dplyr::mutate(
        NDIQuartile = dplyr::ntile(NDI, 4),
        NDIQuartile = factor(NDIQuartile, levels = 1:4)
      )
  } else {
    ndi_output_data <- ndi_output_data |>
      dplyr::mutate(
        NDIQuartile = cut(NDI, breaks = quartile_breaks,
                          labels = 1:4, include.lowest = TRUE)
      )
  }

  ## Annotating GEOIDs
  ndi_output_data <- ndi_output_data |>
    dplyr::mutate(
      annotation = dplyr::case_when(
        tot_pop == 0 ~ 'No population',
        !is.na(NDI) & nvar_imputed == 0 & nvar_still_missing == 0 ~ "Complete data",
        !is.na(NDI) & nvar_imputed >= 0 & nvar_still_missing == 0 ~ "Imputation completed",
        is.na(NDI) & nvar_imputed >= 0 & nvar_still_missing > 0 ~ "Imputation incomplete",
        is.na(NDI) & nvar_still_missing == 0 ~ "PCA failed",
        TRUE ~ 'Other'
      )
    )

  # ====================================================================
  # === FINAL DATA FRAME EXTRACTIONS ===
  # ====================================================================

  # 1. df_ndi (Drop geometry)
  df_ndi <- ndi_output_data |>
    dplyr::select(GEOID, NDI, NDIQuartile, annotation)

  if (inherits(df_ndi, "sf")) {
    df_ndi <- sf::st_drop_geometry(df_ndi)
  }

  # 2. df_raw_values (Drop geometry to keep it tabular)
  df_raw_values <- raw_ndi_data_sub
  if (inherits(df_raw_values, "sf")) {
    df_raw_values <- sf::st_drop_geometry(df_raw_values)
  }

  # 3. df_geometry
  df_geometry <- raw_ndi_data_sub |>
    dplyr::select(GEOID, dplyr::any_of("geometry"))

  # 4. df_imputed (Drop geometry if it carried over)
  df_imputed <- ndi_input_data
  if (inherits(df_imputed, "sf")) {
    df_imputed <- sf::st_drop_geometry(df_imputed)
  }

  # 5. df_components (Extract columns without the calculated NDI score)
  df_components <- ndi_inp_data_pca |>
    dplyr::select(-dplyr::any_of("NDI"))

  if (inherits(df_components, "sf")) {
    df_components <- sf::st_drop_geometry(df_components)
  }

  return(list(
    df_ndi = df_ndi,
    df_raw_values = df_raw_values,
    df_geometry = df_geometry,
    df_imputed = df_imputed,
    df_components = df_components,
    pca_object = ndi_pca
  ))
}
