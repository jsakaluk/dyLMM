#' Extract Intraclass Correlation from an ICC Model
#'
#' Computes the ICC from a fitted [fitICC()] model. For \code{gls(y ~ 1,
#' correlation = corCompSymm(...))}, the compound-symmetry correlation (rho)
#' equals the ICC.
#'
#' @param fit A fitted \code{gls} object from \code{\link{fitICC}}.
#' @param ci Numeric or `NULL`. If numeric (e.g., 0.95), compute a confidence
#'   interval using [nlme::intervals()]. If `NULL`, no CI is computed.
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
  if (!inherits(fit, "gls")) {
    stop("`fit` must be a fitted gls object (from nlme::gls).")
  }
  if (is.null(fit$modelStruct) || is.null(fit$modelStruct$corStruct)) {
    stop("`fit` must have a compound symmetry correlation structure (corCompSymm).")
  }

  icc <- as.numeric(coef(fit$modelStruct$corStruct, unconstrained = FALSE))

  if (is.null(ci)) {
    return(c(ICC = icc))
  }

  int <- tryCatch(
    nlme::intervals(fit, level = ci),
    error = function(e) NULL
  )
  if (is.null(int) || is.null(int$corStruct)) {
    warning("Could not compute confidence interval for ICC. Returning point estimate only.")
    return(c(ICC = icc))
  }

  cs <- int$corStruct
  if (length(cs) == 0) return(c(ICC = icc))

  cn <- colnames(cs)
  ci_lo <- if ("lower" %in% cn) cs[1, "lower"] else if (ncol(cs) >= 1) cs[1, 1] else NULL
  ci_hi <- if ("upper" %in% cn) cs[1, "upper"] else if (ncol(cs) >= 3) cs[1, 3] else NULL
  if (is.null(ci_lo) || is.null(ci_hi)) return(c(ICC = icc))

  c(ICC = icc, CI_low = as.numeric(ci_lo), CI_high = as.numeric(ci_hi))
}
