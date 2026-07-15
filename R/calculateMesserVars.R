#' Calculate Messer NDI component variables
#'
#' @description
#' This internal helper function calculates the 8 component variables for the
#' Messer Neighborhood Deprivation Index from the raw ACS data, following
#' `ndi::messer`:
#' \itemize{
#'   \item `OCC`: proportion of males in management, business, science, and arts occupations
#'   \item `CWD`: proportion of crowded housing (> 1 occupant per room)
#'   \item `POV`: proportion of households below the poverty line
#'   \item `FHH`: proportion of female-headed households with dependent children
#'   \item `PUB`: proportion of households on public assistance or food stamps
#'   \item `U30`: proportion of households earning under $30,000 per year
#'   \item `EDU`: proportion of adults (25+) with less than a high school education
#'   \item `EMP`: proportion unemployed
#' }
#' All components are kept as proportions (0-1), exactly as in `ndi::messer`.
#'
#' Missing-value convention: infinite values (x/0) are replaced with 0
#' (faithful to `ndi::messer`), while NaN values (0/0) are converted to `NA`
#' so they are treated as missing by the imputation / complete-case steps.
#'
#' @param data The data frame from `cleanAcsNames`.
#' @param acs_vars The list of ACS variable names from the attribute.
#'
#' @return A data frame with the new component variables.
#'
#' @importFrom dplyr mutate across all_of where select

calculateMesserVars <- function(data, acs_vars) {
  data_calculated <- data |>
    dplyr::mutate(
      tot_pop = .data[[acs_vars$var_POPTOT]],
      OCC = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_OCC))) / .data[[acs_vars$var_OCC_den]],
      CWD = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_CWD))) / .data[[acs_vars$var_CWD_den]],
      POV = .data[[acs_vars$var_POV]] / .data[[acs_vars$var_POV_den]],
      FHH = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_FHH))) / .data[[acs_vars$var_FHH_den]],
      PUB = .data[[acs_vars$var_PUB]] / .data[[acs_vars$var_PUB_den]],
      U30 = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_U30))) / .data[[acs_vars$var_U30_den]],
      EDU = .data[[acs_vars$var_EDU]] / .data[[acs_vars$var_EDU_den]],
      EMP = .data[[acs_vars$var_EMP]] / .data[[acs_vars$var_EMP_den]]
    )

  data_calculated_out <- data_calculated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ ifelse(is.infinite(.), 0, ifelse(is.nan(.), NA, .))
      )
    )

  return(data_calculated_out)
}
