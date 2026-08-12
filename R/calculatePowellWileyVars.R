#' Calculate Powell-Wiley NDI component variables
#'
#' @description
#' This internal helper function calculates the component variables for the
#' Powell-Wiley Neighborhood Deprivation Index from the raw ACS data,
#' following `ndi::powell_wiley`. It produces the 13 pre-standardization
#' component columns that feed the principal component analysis:
#' \itemize{
#'   \item `logMedHHInc`: log of median household income
#'   \item `PctNoIDR`: percent of households without interest/dividend/rental income
#'   \item `PctPubAsst`: percent of households receiving public assistance
#'   \item `logMedHomeVal`: log of median home value
#'   \item `PctWorkClass`: percent working class (not management/business/science/arts)
#'   \item `PctFemHeadKids`: percent female-headed households with children
#'   \item `PctNotOwnerOcc`: percent of housing units not owner-occupied
#'   \item `PctNoPhone`: percent of housing units without telephone service
#'   \item `PctNComPlmb`: percent of housing units lacking complete plumbing
#'   \item `PctEducLTHS`: percent (25+) with less than a high school education
#'   \item `PctEducLTBch`: percent (25+) with less than a bachelor's degree
#'   \item `PctFamBelowPov`: percent of families below the poverty line
#'   \item `PctUnempl`: percent unemployed
#' }
#' The two income/value components are log-transformed; the other 11 are
#' standardized later, within each scope, in `processPowellWileyScope`.
#'
#' Missing-value convention: non-finite results (`NaN`, `Inf`, `-Inf`, e.g.
#' from a zero denominator or `log(0)`) are converted to `NA` so they are
#' treated as missing by the imputation / complete-case steps. This is a small
#' robustness deviation from `ndi::powell_wiley`, which does not guard against
#' non-finite values.
#'
#' @param data The data frame from `cleanAcsNames`.
#' @param acs_vars The list of ACS variable names from the attribute.
#'
#' @return A data frame with the new component variables.
#'
#' @importFrom dplyr mutate across all_of where select

calculatePowellWileyVars <- function(data, acs_vars) {
  data_calculated <- data |>
    dplyr::mutate(
      tot_pop    = .data[[acs_vars$var_POPTOT]],
      MedHHInc   = .data[[acs_vars$var_MHHINC]],
      MedHomeVal = .data[[acs_vars$var_MVALUE]],
      PctRecvIDR = .data[[acs_vars$var_IDR]] / .data[[acs_vars$var_IDR_den]] * 100,
      PctPubAsst = .data[[acs_vars$var_PUB]] / .data[[acs_vars$var_PUB_den]] * 100,
      PctMgmtBusScArti = .data[[acs_vars$var_MGMT]] / .data[[acs_vars$var_MGMT_den]] * 100,
      PctFemHeadKids = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_FHH))) /
        .data[[acs_vars$var_FHH_den]] * 100,
      PctOwnerOcc = .data[[acs_vars$var_OWNOCC]],
      PctNoPhone  = .data[[acs_vars$var_NOPHONE]],
      PctNComPlmb = .data[[acs_vars$var_NCPLMB]],
      PctEducHSPlus = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_EDU))) /
        .data[[acs_vars$var_EDU_den]] * 100,
      PctEducBchPlus = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_EDU_BCH))) /
        .data[[acs_vars$var_EDU_den]] * 100,
      PctFamBelowPov = .data[[acs_vars$var_FAMPOV]],
      PctUnempl      = .data[[acs_vars$var_UNEMP]]
    ) |>
    dplyr::mutate(
      logMedHHInc    = log(MedHHInc),
      logMedHomeVal  = log(MedHomeVal),
      PctNoIDR       = 100 - PctRecvIDR,
      PctWorkClass   = 100 - PctMgmtBusScArti,
      PctNotOwnerOcc = 100 - PctOwnerOcc,
      PctEducLTHS    = 100 - PctEducHSPlus,
      PctEducLTBch   = 100 - PctEducBchPlus
    )

  # Convert any non-finite values (NaN / Inf / -Inf) to NA so they are treated
  # as missing downstream.
  data_calculated_out <- data_calculated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ ifelse(is.finite(.), ., NA_real_)
      )
    )

  return(data_calculated_out)
}
