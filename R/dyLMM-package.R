#' dyLMM: Dyadic Linear Mixed Models
#'
#' Fit dyadic linear mixed models (LMM/MLM/HLM) including ICC models and
#' actor-partner interdependence models (APIM) for indistinguishable and
#' distinguishable dyads.
#'
#' @section Data wrangling:
#' - [build_composites()] - Aggregate indicators to sum or mean composites
#' - [wide_to_long()] - Reshape wide dyad data to long format
#'
#' @section Model fitting:
#' - [fitICC()] - Intraclass correlation (one-way random effects)
#' - [fitAPIMindist()] - Indistinguishable actor-partner interdependence model
#' - [fitAPIMdist()] - Distinguishable actor-partner interdependence model
#'
#' @section Helpers:
#' - [getICC()] - Extract ICC from fitted ICC model
#' - [outputLMMTab()] - Reproducible parameter table (tibble or gt)
#'
#' @importFrom magrittr %>%
#' @keywords internal
"_PACKAGE"
