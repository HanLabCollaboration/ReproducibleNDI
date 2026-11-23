#' Compute the NDI exactly as the `ndi` package does
#'
#' @description
#' Reproduces the score of `ndi::messer()` (Messer et al. 2006) or
#' `ndi::powell_wiley()` (Andrews et al. 2020), with no options that change the
#' method. It fits one PCA over all the geographies of the `states` requested,
#' with no imputation and no scope groups.
#'
#' **Messer** (`type = "messer"`):
#' \enumerate{
#'   \item fetch the 8 ACS component proportions (same tables as
#'     `ndi::messer`);
#'   \item fit a one-component `psych::principal()` to the data **with its
#'     missing values**, so the loadings come from **pairwise-complete**
#'     correlations;
#'   \item score complete cases as `PC1 / eigenvalue^2`, with no sign
#'     alignment, and cut quartiles at the score quantiles.
#' }
#'
#' **Powell-Wiley** (`type = "powell_wiley"`):
#' \enumerate{
#'   \item fetch the 13 ACS component variables (same tables as
#'     `ndi::powell_wiley`);
#'   \item z-score the 11 percentage components (the two log components are
#'     used as-is);
#'   \item choose the number of factors with the Kaiser criterion
#'     (eigenvalues > 1) of the **complete-case** correlation matrix;
#'   \item fit `psych::principal()` to the data **with its missing values**, so
#'     the loadings come from **pairwise-complete** correlations, and rotate
#'     them with promax (`m = 3`);
#'   \item compute regression weights from the complete-case correlation
#'     matrix and score the factor in **column 1**, using the variables whose
#'     structure loading exceeds 0.4 in absolute value, for complete cases
#'     only, with no sign alignment.
#' }
#'
#' Differences from [computeNDI()], which are the reasons the two scores
#' differ:
#' \itemize{
#'   \item the loadings use pairwise-complete correlations, not complete
#'     cases (for Messer, the component scores are also standardized with
#'     the means and standard deviations of all available values);
#'   \item the sign is **not** aligned: as in `ndi`, a higher score may mean
#'     less deprivation, depending on the data;
#'   \item there is no scope (one PCA over the `states` requested) and no
#'     imputation.
#' }
#'
#' For Powell-Wiley, the scored factor is the one promax returns first,
#' whatever it measures. With ACS 5-year national tract data it is a
#' poverty-led factor in 2014 and an education/occupation-led factor from 2015
#' on (two factors account for almost the same variance); see
#' `dev/powell_wiley_factors/` in the source repository.
#'
#' Geographies without population are dropped, as in [computeNDI()]. `ndi`
#' keeps them, but their components are all missing (no tract without
#' population had any Powell-Wiley component in ACS5 2016 or 2022), so they
#' add nothing to the correlations and the score is the same. They are
#' returned with an `NA` score.
#'
#' Not reproduced: the Powell-Wiley population-weighted quintiles (`NDIQuint`,
#' which need `Hmisc`) and Cronbach's alpha. Component values that are `NaN`
#' (zero denominator) are set to `NA`, which `ndi` also treats as missing.
#'
#' @param type `"messer"` (default) or `"powell_wiley"`.
#' @param geo `"county"` or `"tract"` (default). Block groups are not
#'   available for either method (see [computeNDI()]).
#' @param year The ACS 5-year end year (>= 2011).
#' @param states A vector of state abbreviations, or `"all"` (default; the 50
#'   states and DC).
#' @param quiet Logical. If `TRUE`, suppresses messages.
#' @param return_format `"detailed"` (default) or `"minimal"`.
#'
#' @return
#' If `return_format = "minimal"`, a data frame with `GEOID` and `NDI` (and
#' `NDIQuart` for Messer), one row per geography, sorted by `GEOID`.
#' Otherwise a list:
#' \itemize{
#'   \item `df_ndi`: `GEOID`, `NAME`, `tot_pop`, `NDI` (and `NDIQuart` for
#'     Messer, with `ndi`'s labels).
#'   \item `df_components`: `GEOID` and the components (Powell-Wiley: before z-scoring),
#'     with their missing values, for the geographies with population.
#'   \item `pca`: for Messer, `principal` (the `psych::principal` fit),
#'     `loadings` and `eigenvalue`; for Powell-Wiley, `principal`, `promax`,
#'     `structure` (structure loadings), `Phi` (factor correlations), `nfa`
#'     (number of factors), `weights` (regression weights of factor 1) and
#'     `scored_vars` (variables with |structure loading| > 0.4). Both have
#'     `n_complete` (complete cases scored).
#' }
#'
#' @references
#' Messer, L.C., Laraia, B.A., Kaufman, J.S., et al. (2006). The development
#' of a standardized neighborhood deprivation index. Journal of Urban Health,
#' 83(6), 1041-1062.
#'
#' Andrews, M.R., Tamura, K., Claudel, S.E., et al. (2020). Geospatial
#' analysis of neighborhood deprivation index (NDI) for the United States by
#' county. Journal of Maps, 16(1), 101-112.
#'
#' Buller, I.D. ndi: Neighborhood Deprivation Indices. R package,
#' \url{https://CRAN.R-project.org/package=ndi}.
#'
#' @seealso [computeNDI()]
#'
#' @importFrom psych principal
#' @importFrom stats promax loadings cor cov2cor complete.cases quantile
#' @export
#'
#' @examples
#' \dontrun{
#'   # Requires a Census API key
#'   pw <- computeOriginalNDI(type = "powell_wiley", year = 2022, states = "CA")
#'
#'   # Same score as the ndi package
#'   ref <- ndi::powell_wiley(geo = "tract", year = 2022, state = "CA")
#'   m <- merge(pw$df_ndi, ref$ndi[, c("GEOID", "NDI")], by = "GEOID")
#'   max(abs(m$NDI.x - m$NDI.y), na.rm = TRUE)   # 0
#'
#'   ms <- computeOriginalNDI(type = "messer", year = 2022, states = "CA")
#' }
computeOriginalNDI <- function(
    type          = "messer",
    geo           = "tract",
    year          = 2022,
    states        = "all",
    quiet         = FALSE,
    return_format = "detailed"
) {
  if (!type %in% c("messer", "powell_wiley")) {
    stop("'type' must be \"messer\" or \"powell_wiley\".", call. = FALSE)
  }
  if (!geo %in% c("county", "tract")) {
    stop("computeOriginalNDI() supports geo = 'county' or 'tract' only ",
         "(neither method's ACS tables are published at block group).", call. = FALSE)
  }
  if (!is.numeric(year) || length(year) != 1 || year < 2011) {
    stop("'year' must be a single ACS 5-year end year, 2011 or later.", call. = FALSE)
  }
  if (!return_format %in% c("detailed", "minimal")) {
    stop("'return_format' must be \"detailed\" or \"minimal\".", call. = FALSE)
  }
  valid_states <- setdiff(unique(tidycensus::fips_codes$state),
                          c("PR", "AS", "GU", "MP", "VI", "UM"))
  if (identical(states, "all")) {
    states <- valid_states
  } else if (length(bad <- setdiff(states, valid_states)) > 0) {
    stop("Invalid state abbreviations: ", paste(bad, collapse = ", "),
         ". Use 2-letter codes for the 50 states or DC.", call. = FALSE)
  }

  # Component order of the ndi package (column positions matter in the PCA)
  varlist <- switch(type,
    messer       = c("OCC", "CWD", "POV", "FHH", "PUB", "U30", "EDU", "EMP"),
    powell_wiley = c("logMedHHInc", "PctNoIDR", "PctPubAsst", "logMedHomeVal",
                     "PctWorkClass", "PctFemHeadKids", "PctNotOwnerOcc",
                     "PctNoPhone", "PctNComPlmb", "PctEducLTHS", "PctEducLTBch",
                     "PctFamBelowPov", "PctUnempl"))
  fetch_fun     <- switch(type, messer = fetchMesserAcs, powell_wiley = fetchPowellWileyAcs)
  calculate_fun <- switch(type, messer = calculateMesserVars,
                          powell_wiley = calculatePowellWileyVars)

  if (!quiet) message("Fetching raw ACS data...")
  raw <- fetch_fun(geo, year, states, get_geometry = FALSE)
  acs_vars <- attr(raw, "acs_vars")
  dat <- calculate_fun(cleanAcsNames(dframe = raw, geo = geo), acs_vars = acs_vars)
  dat <- as.data.frame(dat)[order(dat$GEOID), c("GEOID", "NAME", "tot_pop", varlist)]

  # Geographies without population: NA score, not in the PCA
  has_pop <- !is.na(dat$tot_pop) & dat$tot_pop > 0
  comp <- dat[has_pop, varlist]
  rownames(comp) <- NULL

  if (!quiet) message("Fitting the PCA (pairwise-complete loadings)...")
  fit <- switch(type,
    messer       = fitOriginalMesser(comp),
    powell_wiley = fitOriginalPowellWiley(comp))

  df_ndi <- data.frame(GEOID = dat$GEOID, NAME = dat$NAME, tot_pop = dat$tot_pop,
                       NDI = NA_real_, stringsAsFactors = FALSE)
  df_ndi$NDI[has_pop] <- fit$scores

  if (type == "messer") {
    # ndi::messer quartiles: cut at the score quantiles
    q <- cut(df_ndi$NDI,
             breaks = stats::quantile(df_ndi$NDI, probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
             labels = c("1-Least deprivation", "2-BelowAvg deprivation",
                        "3-AboveAvg deprivation", "4-Most deprivation"),
             include.lowest = TRUE)
    df_ndi$NDIQuart <- factor(replace(as.character(q), is.na(q), "9-NDI not avail"),
                              c(levels(q), "9-NDI not avail"))
  }

  if (return_format == "minimal") {
    return(df_ndi[, intersect(c("GEOID", "NDI", "NDIQuart"), names(df_ndi))])
  }
  list(
    df_ndi        = df_ndi,
    df_components = data.frame(GEOID = dat$GEOID[has_pop], comp, stringsAsFactors = FALSE),
    pca           = fit$pca
  )
}

# Messer as in ndi::messer: one component from psych::principal() on the data
# with NAs (pairwise correlations); complete cases scored PC1 / eigenvalue^2.
fitOriginalMesser <- function(comp) {
  pca <- psych::principal(comp, nfactors = 1, n.obs = nrow(comp),
                          covar = FALSE, scores = TRUE, missing = FALSE)
  scores <- as.numeric(pca$scores[, 1]) / pca$values[1]^2
  list(scores = scores,
       pca = list(principal = pca, loadings = pca$loadings[, 1],
                  eigenvalue = pca$values[1],
                  n_complete = sum(is.finite(scores))))
}

# Powell-Wiley as in ndi::powell_wiley: Kaiser criterion on complete cases,
# promax-rotated loadings from pairwise correlations, regression scores of
# column 1 on complete cases.
fitOriginalPowellWiley <- function(comp) {
  varlist  <- names(comp)
  log_vars <- c("logMedHHInc", "logMedHomeVal")
  for (v in setdiff(varlist, log_vars)) comp[[v]] <- as.numeric(scale(comp[[v]]))

  n_vars <- length(varlist)
  cc     <- stats::complete.cases(comp)
  R_mat  <- stats::cor(comp[cc, ])

  nfa <- sum(eigen(R_mat)$values > 1)
  fit <- psych::principal(comp, nfactors = nfa, rotate = "none")
  fit_rotate <- stats::promax(stats::loadings(fit), m = 3)
  if (nfa > 1) {
    P_mat <- matrix(stats::loadings(fit_rotate), nrow = n_vars, ncol = nfa)
    ui    <- solve(fit_rotate$rotmat)
    Phi   <- stats::cov2cor(ui %*% t(ui))
    S_mat <- P_mat %*% Phi
  } else {
    P_mat <- matrix(fit_rotate, nrow = n_vars, ncol = 1)
    Phi   <- 1
    S_mat <- P_mat
  }
  dimnames(S_mat) <- list(varlist, paste0("F", seq_len(ncol(S_mat))))
  B_mat <- solve(R_mat, S_mat)

  keep   <- abs(S_mat[, 1]) > 0.4
  scores <- rep(NA_real_, nrow(comp))
  scores[cc] <- as.numeric(as.matrix(scale(comp[cc, keep, drop = FALSE])) %*% B_mat[keep, 1])

  list(scores = scores,
       pca = list(principal = fit, promax = fit_rotate, structure = S_mat,
                  Phi = Phi, nfa = nfa, weights = B_mat[, 1],
                  scored_vars = varlist[keep], n_complete = sum(cc)))
}
