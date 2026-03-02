#' Extract Intraclass Correlation from an ICC Model
#'
#' Computes the ICC from a fitted one-way random effects model (e.g., from
#' [fitICC()]). Uses variance components from [lme4::VarCorr()] when
#' `performance` is not available; otherwise uses [performance::icc()] for
#' optional confidence intervals.
#'
#' @param fit A fitted \code{lmerMod} object from \code{\link{fitICC}} or equivalent
#'   (random intercept only: `y ~ 1 + (1 | dyad_id)`).
#' @param ci Numeric or `NULL`. If numeric (e.g., 0.95), compute a confidence
#'   interval using [performance::icc()]. Requires the `performance` package.
#'   If `NULL`, no CI is computed.
#'
#' @return A numeric vector with `ICC` and optionally `CI_low` and `CI_high`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dySEM)
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
#'   x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
#'   verbose = FALSE)
#' dat_comp <- build_composites(dat = commitmentQ, dvn = dvn)
#' dat_long <- wide_to_long(dat_comp)
#' fit <- fitICC(data = dat_long, y = "y")
#' getICC(fit)
#' getICC(fit, ci = 0.95)
#' }
getICC <- function(fit, ci = NULL) {
  if (!inherits(fit, "lmerMod")) {
    stop("`fit` must be a fitted lmerMod object (from lme4::lmer).")
  }

  vc <- lme4::VarCorr(fit)
  s2_dyad <- as.numeric(vc[[1]][1, 1])
  s2_resid <- as.numeric(attr(vc, "sc")^2)
  icc <- s2_dyad / (s2_dyad + s2_resid)

  if (is.null(ci)) {
    return(c(ICC = icc))
  }

  if (!requireNamespace("performance", quietly = TRUE)) {
    warning("Package 'performance' is required for confidence intervals. Returning ICC only.")
    return(c(ICC = icc))
  }

  icc_obj <- performance::icc(fit, ci = ci)
  coalesce2 <- function(...) {
    for (x in list(...)) if (!is.null(x)) return(x)
    NULL
  }
  or_null <- function(x, y) if (is.null(x)) y else x
  icc_val <- NULL
  ci_lo <- NULL
  ci_hi <- NULL

  if (inherits(icc_obj, "data.frame") && nrow(icc_obj) >= 1) {
    icc_val <- coalesce2(
      icc_obj[[1, "ICC_adjusted"]], icc_obj[[1, "ICC_Adjusted"]],
      icc_obj[["ICC_adjusted"]], icc_obj[["ICC_Adjusted"]],
      icc_obj[["ICC_conditional"]], icc_obj[["ICC_Conditional"]],
      icc_obj[["ICC"]], icc
    )
    if (nrow(icc_obj) >= 3) {
      ci_lo <- coalesce2(
        icc_obj[[2, "ICC_adjusted"]], icc_obj[[2, "ICC_Adjusted"]],
        icc_obj[2, "ICC_adjusted"]
      )
      ci_hi <- coalesce2(
        icc_obj[[3, "ICC_adjusted"]], icc_obj[[3, "ICC_Adjusted"]],
        icc_obj[3, "ICC_adjusted"]
      )
    }
  } else if (is.list(icc_obj)) {
    icc_val <- coalesce2(
      icc_obj[["ICC_adjusted"]], icc_obj[["ICC_Adjusted"]],
      icc_obj[["ICC_conditional"]], icc_obj[["ICC_Conditional"]],
      icc_obj[["ICC"]], icc
    )
    ci_lo <- or_null(icc_obj[["CI_low"]], icc_obj[["CI_2.5%"]])
    ci_hi <- or_null(icc_obj[["CI_high"]], icc_obj[["CI_97.5%"]])
  }

  icc_val <- or_null(icc_val, icc)
  if (is.null(ci_lo) || is.null(ci_hi)) {
    return(c(ICC = icc_val))
  }
  c(ICC = icc_val, CI_low = as.numeric(ci_lo), CI_high = as.numeric(ci_hi))
}
