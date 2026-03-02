#' Fit a Distinguishable Actor-Partner Interdependence Model
#'
#' Fits an APIM with separate actor and partner effects by partner type
#' (e.g., gender). Formula: `y ~ (x + x_partner) * distinguish + (1 | dyad_id)`.
#' Supports both factor and numeric distinguish variables (e.g., effect-coded).
#'
#' @param data Data frame in long format (two rows per dyad).
#' @param y Character. Name of the outcome column.
#' @param x Character. Name of the actor's predictor column.
#' @param distinguish Character. Name of the column distinguishing partners
#'   (e.g., gender). Can be factor or numeric (effect-coded, dummy-coded).
#' @param dyad_id Character. Name of the dyad identifier column. Default `"dyad_id"`.
#' @param x_partner Character. Name of the partner's predictor column. If `NULL`,
#'   inferred from `x` by appending `_partner`.
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
#' dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
#'   x_delim1 = "", x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m",
#'   y_order = "sip", y_stem = "com", y_delim1 = "", y_delim2 = "_",
#'   verbose = FALSE)
#' dat_comp <- build_composites(dat = commitmentM, dvn = dvn)
#' dat_long <- wide_to_long(dat_comp, partner_labels = c("f", "m"))
#' fit <- fitAPIMdist(data = dat_long, y = "y", x = "x", distinguish = "distinguish")
#' }
fitAPIMdist <- function(data,
                        y,
                        x,
                        distinguish,
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
  req <- c(y, x, dyad_id, x_partner, distinguish)
  miss <- req[!req %in% names(data)]
  if (length(miss) > 0) {
    stop("Required columns not found in `data`: ", paste(miss, collapse = ", "))
  }

  f <- stats::as.formula(paste0(
    y, " ~ (", x, " + ", x_partner, ") * ", distinguish, " + (1 | ", dyad_id, ")"
  ))
  .lmer_fit(formula = f, data = data, use_lmerTest = use_lmerTest, ...)
}
