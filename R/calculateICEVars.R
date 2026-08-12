#' Calculate Krieger ICE measures
#'
#' @description
#' This internal helper function calculates the five Index of Concentration at
#' the Extremes (ICE) measures from the raw ACS data, following `ndi::krieger`.
#' Each measure is a per-geography ratio of the form
#' `(Advantaged - Deprived) / Total`:
#' \itemize{
#'   \item `ICE_inc`: household income (high-income minus low-income households)
#'   \item `ICE_edu`: educational attainment (bachelor's+ minus less-than-high-school, 25+)
#'   \item `ICE_rewb`: race/ethnicity (non-Hispanic White minus non-Hispanic Black)
#'   \item `ICE_wbinc`: racialized economic segregation (high-income non-Hispanic
#'     White minus low-income Black households)
#'   \item `ICE_wpcinc`: income concentration among non-Hispanic White households
#'     (high-income minus low-income White households)
#' }
#'
#' The income thresholds follow Krieger's inflation-adjusted cutpoints, which
#' `ndi::krieger` switches at 2016: for `year < 2016` "low income" is under
#' $25k and "high income" is over $100k; from 2016 onward "low income" is
#' under $30k and "high income" is over $125k.
#'
#' Missing-value convention: non-finite results (`NaN`, `Inf`, `-Inf`, e.g.
#' from a zero denominator) are converted to `NA`.
#'
#' @param data The data frame from `cleanAcsNames`.
#' @param acs_vars The list of ACS variable names from the attribute.
#' @param year The ACS5 year (determines the income-bracket cutpoints).
#'
#' @return A data frame with the building-block component sums and the five
#'   `ICE_*` measures.
#'
#' @importFrom dplyr mutate across all_of where select

calculateICEVars <- function(data, acs_vars, year) {

  # Year-dependent income bracket selections.
  # var_*_low columns are ordered _002.._006; var_*_high columns _014.._017.
  if (year < 2016) {
    inc_low_cols   <- acs_vars$var_INC_low[1:4]    # <$25k  (_002.._005)
    inc_high_cols  <- acs_vars$var_INC_high[1:4]   # >$100k (_014.._017)
    incw_low_cols  <- acs_vars$var_INCW_low[1:4]
    incw_high_cols <- acs_vars$var_INCW_high[1:4]
    incb_low_cols  <- acs_vars$var_INCB_low[1:4]
  } else {
    inc_low_cols   <- acs_vars$var_INC_low[1:5]    # <$30k  (_002.._006)
    inc_high_cols  <- acs_vars$var_INC_high[2:4]   # >$125k (_015.._017)
    incw_low_cols  <- acs_vars$var_INCW_low[1:5]
    incw_high_cols <- acs_vars$var_INCW_high[2:4]
    incb_low_cols  <- acs_vars$var_INCB_low[1:5]
  }

  data_calculated <- data |>
    dplyr::mutate(
      # Totals (denominators)
      TotalPop_inc = .data[[acs_vars$var_INC_tot]],
      TotalPop_edu = .data[[acs_vars$var_EDU_tot]],
      TotalPop_re  = .data[[acs_vars$var_RE_tot]],
      tot_pop      = .data[[acs_vars$var_RE_tot]],

      # Income concentration building blocks
      A_inc = rowSums(dplyr::across(dplyr::all_of(inc_high_cols))),
      P_inc = rowSums(dplyr::across(dplyr::all_of(inc_low_cols))),

      # Education concentration building blocks
      A_edu = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_EDU_BDup))),
      P_edu = rowSums(dplyr::across(dplyr::all_of(acs_vars$var_EDU_ltHS))),

      # Race/ethnicity concentration building blocks
      NHoLW = .data[[acs_vars$var_RE_nhw]],
      NHoLB = .data[[acs_vars$var_RE_nhb]],

      # Racialized economic segregation (White high income vs Black low income)
      A_wbinc = rowSums(dplyr::across(dplyr::all_of(incw_high_cols))),
      P_wbinc = rowSums(dplyr::across(dplyr::all_of(incb_low_cols))),

      # White income concentration (White high income vs White low income)
      A_wpcinc = rowSums(dplyr::across(dplyr::all_of(incw_high_cols))),
      P_wpcinc = rowSums(dplyr::across(dplyr::all_of(incw_low_cols)))
    ) |>
    dplyr::mutate(
      ICE_inc    = (A_inc - P_inc) / TotalPop_inc,
      ICE_edu    = (A_edu - P_edu) / TotalPop_edu,
      ICE_rewb   = (NHoLW - NHoLB) / TotalPop_re,
      ICE_wbinc  = (A_wbinc - P_wbinc) / TotalPop_inc,
      ICE_wpcinc = (A_wpcinc - P_wpcinc) / TotalPop_inc
    )

  # Convert any non-finite values (NaN / Inf / -Inf) to NA.
  data_calculated_out <- data_calculated |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ ifelse(is.finite(.), ., NA_real_)
      )
    )

  return(data_calculated_out)
}
