#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data :=
## usethis namespace: end

utils::globalVariables(c(
  # shared
  "GEOID", "NAME", "NDI", "annotation", "county", "county_state",
  "nvar_imputed", "nvar_still_missing", "state", "tot_pop",
  # messer
  "NDIQuartile", "OCC", "CWD", "POV", "FHH", "PUB", "U30", "EDU", "EMP",
  # powell-wiley
  "MedHHInc", "MedHomeVal", "PctRecvIDR", "PctNoIDR", "PctPubAsst",
  "PctMgmtBusScArti", "PctWorkClass", "PctFemHeadKids", "PctOwnerOcc",
  "PctNotOwnerOcc", "PctNoPhone", "PctNComPlmb", "PctEducHSPlus",
  "PctEducBchPlus", "PctEducLTHS", "PctEducLTBch", "PctFamBelowPov",
  "PctUnempl", "logMedHHInc", "logMedHomeVal",
  # ice (krieger)
  "ICE_inc", "ICE_edu", "ICE_rewb", "ICE_wbinc", "ICE_wpcinc",
  "A_inc", "P_inc", "TotalPop_inc", "A_edu", "P_edu", "TotalPop_edu",
  "NHoLW", "NHoLB", "TotalPop_re", "A_wbinc", "P_wbinc", "A_wpcinc",
  "P_wpcinc", "val", "measure", "vars_imputed"
))
