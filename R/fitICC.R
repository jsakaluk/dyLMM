#' Fit an Intraclass Correlation (ICC) Model
#'
#' Fits a model with compound symmetry to estimate the intraclass correlation
#' (ICC) for dyadic data. Uses [nlme::gls()]. Use [getICC()] or [getRho()] to
#' extract the ICC (rho) from the fitted model.
#'
#' @param data Data frame in long format (two rows per dyad).
#' @param y Character. Name of the outcome column.
#' @param dyad_id Character. Name of the dyad identifier column. Default `"dyad_id"`.
#' @param ... Further arguments passed to [nlme::gls()].
#'
#' @return A fitted \code{gls} object from \code{nlme::gls}.
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
#' }
fitICC <- function(data,
                   y,
                   dyad_id = "dyad_id",
                   ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!y %in% names(data)) {
    stop("Outcome column '", y, "' not found in `data`.")
  }
  if (!dyad_id %in% names(data)) {
    stop("Dyad ID column '", dyad_id, "' not found in `data`.")
  }

  f <- stats::as.formula(paste0(y, " ~ 1"))
  nlme::gls(
    model = f,
    data = data,
    correlation = nlme::corCompSymm(form = stats::as.formula(paste0("~ 1 | ", dyad_id))),
    na.action = na.omit,
    ...
  )
}
