#' Process a single scope for the Powell-Wiley NDI
#'
#' @description
#' This is the primary internal workhorse function for `computeNDI` with
#' `type = "powell_wiley"`. It processes a single "scope" (e.g., all national
#' data, or data for a single state).
#'
#' It handles:
#' 1. Filtering geographies with no population.
#' 2. Optionally imputing missing data (spatial neighbors, queen contiguity).
#' 3. Standardizing the component variables within the scope and running the
#'    promax-rotated principal component analysis of `ndi::powell_wiley`
#'    (number of factors chosen by the Kaiser criterion; regression scoring on
#'    the first component using variables whose structure loading exceeds 0.4
#'    in absolute value), with robust error handling.
#' 4. Calculating the final NDI score.
#' 5. Annotating the final data based on data quality/imputation status.
#'
#' Two deviations from `ndi::powell_wiley`: (a) the population-weighted quintile
#' grouping is not computed (only the score is returned), and (b) the sign of
#' the component is aligned so that `PctFamBelowPov` loads positively - i.e.,
#' higher NDI always means more deprivation - which keeps the orientation
#' consistent when the PCA is fit separately per scope group.
#'
#' @param ndi_data_sub An `sf` data frame subset for a single scope (e.g., one state).
#' @param impute_sub Logical, whether to perform spatial imputation.
#' @param quiet_sub Logical, whether to suppress messages and warnings.
#' @param return_format_sub The desired return format (minimal or detailed).
#'   (Note: This is handled by the parent `computeNDI` function).
#' @param weight_var_sub A string. The name of the column to use for weighting
#'   the spatial imputation.
#'
#' @return
#' A list containing six elements:
#' * `df_ndi`: Data frame with GEOID, NDI, and annotation (no geometry).
#' * `df_raw_values`: The raw input data without geometry.
#' * `df_geometry`: Data frame containing only GEOID and geometry.
#' * `df_imputed`: The imputed data without geometry.
#' * `df_components`: The standardized component variables used for the PCA.
#' * `pca_object`: A list of PCA diagnostics (`principal`, `promax`,
#'   `structure`, `Phi`, `nfa`, `weights`), or `NULL` if the analysis failed.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter select any_of mutate across all_of left_join case_when
#' @importFrom glue glue
#' @importFrom psych principal
#' @importFrom stats promax loadings cor cov2cor complete.cases

processPowellWileyScope <- function(
    ndi_data_sub,
    impute_sub,
    quiet_sub,
    return_format_sub,
    weight_var_sub = NULL) {

  # 13 pre-standardization component columns, in the ndi::powell_wiley order.
  varlist <- c("logMedHHInc", "PctNoIDR", "PctPubAsst", "logMedHomeVal",
               "PctWorkClass", "PctFemHeadKids", "PctNotOwnerOcc", "PctNoPhone",
               "PctNComPlmb", "PctEducLTHS", "PctEducLTBch", "PctFamBelowPov",
               "PctUnempl")
  # The two log-income/value columns are used as-is; the other 11 are z-scored.
  log_vars <- c("logMedHHInc", "logMedHomeVal")

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

  # Keep only complete rows, then standardize the 11 non-log components within
  # this scope (unweighted z-score); the two log columns are used as-is.
  ndi_complete <- ndi_input_data |>
    dplyr::filter(nvar_still_missing == 0) |>
    dplyr::select(GEOID, dplyr::all_of(varlist)) |>
    sf::st_drop_geometry()

  z_vars <- setdiff(varlist, log_vars)
  ndi_std <- ndi_complete |>
    dplyr::mutate(dplyr::across(dplyr::all_of(z_vars), ~ scale(.)[, 1]))

  # ====================================================================
  # === Promax-rotated PCA with Error Handling (ndi::powell_wiley) ===
  # ====================================================================

  current_scope <- "Current Scope"
  if ("state" %in% colnames(raw_ndi_data_sub)) {
    current_scope <- unique(raw_ndi_data_sub$state)[1]
  }

  # Standardized component matrix (columns in the fixed varlist order)
  mat <- as.matrix(ndi_std[, varlist, drop = FALSE])

  n_obs  <- nrow(mat)
  n_vars <- ncol(mat)

  ndi_pca <- NULL

  # 1. Pre-check: skip if not enough observations
  if (n_obs < n_vars + 2) {
    if (!quiet_sub) {
      warning(glue::glue(
        "Skipping PCA for '{current_scope}': ",
        "Insufficient complete observations ({n_obs}) for {n_vars} variables."
      ))
    }
    ndi_scores <- rep(NA_real_, n_obs)
  } else {
    # 2. tryCatch: attempt the promax PCA + regression scoring
    set.seed(5757)
    pca_result <- tryCatch({
      eig <- eigen(stats::cor(mat))
      nfa <- max(sum(eig$values > 1), 1L)          # Kaiser criterion (>=1 guard)
      fit <- psych::principal(mat, nfactors = nfa, rotate = "none")
      fit_rotate <- stats::promax(stats::loadings(fit), m = 3)

      if (nfa > 1) {
        P_mat <- matrix(stats::loadings(fit_rotate), nrow = n_vars, ncol = nfa)
        ui  <- solve(fit_rotate$rotmat)
        Phi <- stats::cov2cor(ui %*% t(ui))
        S_mat <- P_mat %*% Phi
      } else {
        # stats::promax returns the loadings matrix directly when ncol < 2
        P_mat <- matrix(fit_rotate, nrow = n_vars, ncol = 1)
        Phi <- 1
        S_mat <- P_mat
      }

      R_mat <- as.matrix(stats::cor(mat))
      B_mat <- solve(R_mat, S_mat)

      # Score PC1 using only variables that load |structure| > 0.4
      keep <- abs(S_mat[, 1]) > 0.4
      scrs <- as.matrix(scale(mat[, keep, drop = FALSE])) %*% B_mat[keep, 1]

      # Sign-align so poverty loads positively (higher NDI = more deprivation)
      pov_idx <- match("PctFamBelowPov", varlist)
      if (!is.na(pov_idx) && is.finite(S_mat[pov_idx, 1]) && S_mat[pov_idx, 1] != 0) {
        factor_sign <- if (S_mat[pov_idx, 1] < 0) -1 else 1
      } else {
        # fallback: orient so most structure loadings are positive
        factor_sign <- if (sum(S_mat[, 1] < 0) > sum(S_mat[, 1] > 0)) -1 else 1
      }

      list(
        scores = as.numeric(scrs) * factor_sign,
        object = list(principal = fit, promax = fit_rotate,
                      structure = S_mat, Phi = Phi, nfa = nfa,
                      weights = B_mat[, 1])
      )
    }, error = function(e) {
      if (!quiet_sub) {
        warning(glue::glue(
          "PCA failed for '{current_scope}' ",
          "which had {n_obs} complete observations. ",
          "Error: {e$message}"
        ))
      }
      return(NULL)
    })

    if (is.null(pca_result)) {
      ndi_scores <- rep(NA_real_, n_obs)
    } else {
      ndi_scores <- pca_result$scores
      ndi_pca <- pca_result$object
    }
  }

  ndi_complete$NDI <- ndi_scores

  # Step 5: Preparing output
  ndi_output_data <- raw_ndi_data_sub |>
    dplyr::left_join(
      ndi_complete |> dplyr::select(GEOID, NDI), by = "GEOID"
    ) |>
    dplyr::left_join(
      ndi_input_data |>
        dplyr::select(GEOID, nvar_still_missing, nvar_imputed) |> sf::st_drop_geometry(),
      by = "GEOID"
    )

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

  # 1. df_ndi (score only - no quantile grouping)
  df_ndi <- ndi_output_data |>
    dplyr::select(GEOID, NDI, annotation)

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

  # 5. df_components (standardized PCA input, with GEOID)
  df_components <- ndi_std

  return(list(
    df_ndi = df_ndi,
    df_raw_values = df_raw_values,
    df_geometry = df_geometry,
    df_imputed = df_imputed,
    df_components = df_components,
    pca_object = ndi_pca
  ))
}
