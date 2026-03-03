#' Fit an Indistinguishable Actor-Partner Interdependence Model
#'
#' Fits an APIM with actor and partner effects constrained equal across dyad
#' members (exchangeable partners). Uses [nlme::gls()] with compound symmetry;
#' equivalent to `y ~ x + x_partner` with correlated errors within dyads.
#'
#' @param data Data frame in long format (two rows per dyad).
#' @param y Character. Name of the outcome column.
#' @param x Character. Name of the actor's predictor column.
#' @param dyad_id Character. Name of the dyad identifier column. Default `"dyad_id"`.
#' @param x_partner Character. Name of the partner's predictor column. If `NULL`,
#'   inferred from `x` by appending `_partner` (e.g., `x` -> `x_partner`).
#' @param ... Further arguments passed to [nlme::gls()].
#'
#' @return A fitted \code{gls} object from \code{nlme::gls}.
#'
#' @seealso \url{https://randilgarcia.github.io/week-dyad-workshop/Indistinguishable.html}
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

  f <- stats::as.formula(paste0(y, " ~ ", x, " + ", x_partner))
  nlme::gls(
    model = f,
    data = data,
    correlation = nlme::corCompSymm(form = stats::as.formula(paste0("~ 1 | ", dyad_id))),
    na.action = na.omit,
    ...
  )
}
