#' Fit an Indistinguishable Actor-Partner Interdependence Model
#'
#' Fits an APIM with actor and partner effects constrained equal across dyad
#' members (exchangeable partners). Formula: `y ~ x + x_partner + (1 | dyad_id)`.
#'
#' @param data Data frame in long format (two rows per dyad).
#' @param y Character. Name of the outcome column.
#' @param x Character. Name of the actor's predictor column.
#' @param dyad_id Character. Name of the dyad identifier column. Default `"dyad_id"`.
#' @param x_partner Character. Name of the partner's predictor column. If `NULL`,
#'   inferred from `x` by appending `_partner` (e.g., `x` -> `x_partner`).
#' @param use_lmerTest Logical. If `TRUE` (default), use `lmerTest::lmer()` when
#'   the lmerTest package is available to obtain Satterthwaite p-values.
#' @param ... Further arguments passed to [lme4::lmer()].
#'
#' @return A fitted \code{lmerMod} object from \code{lme4::lmer}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dySEM)
#' dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
#'   x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
#'   y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
#'   verbose = FALSE)
#' dat_comp <- build_composites(dat = commitmentQ, dvn = dvn)
#' dat_long <- wide_to_long(dat_comp)
#' fit <- fitAPIMindist(data = dat_long, y = "y", x = "x")
#' }
fitAPIMindist <- function(data,
                          y,
                          x,
                          dyad_id = "dyad_id",
                          x_partner = NULL,
                          use_lmerTest = TRUE,
                          ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (is.null(x_partner)) {
    x_partner <- paste0(x, "_partner")
  }
  req <- c(y, x, dyad_id, x_partner)
  miss <- req[!req %in% names(data)]
  if (length(miss) > 0) {
    stop("Required columns not found in `data`: ", paste(miss, collapse = ", "))
  }

  f <- stats::as.formula(paste0(
    y, " ~ ", x, " + ", x_partner, " + (1 | ", dyad_id, ")"
  ))
  .lmer_fit(formula = f, data = data, use_lmerTest = use_lmerTest, ...)
}
