#' Fit a Distinguishable Actor-Partner Interdependence Model
#'
#' Fits an APIM with separate actor and partner effects by partner type
#' (e.g., gender) using the two-intercept approach. Uses [nlme::gls()] with
#' compound symmetry and group-specific residual variances.
#'
#' @param data Data frame in long format (two rows per dyad).
#' @param y Character. Name of the outcome column.
#' @param x Character. Name of the actor's predictor column.
#' @param distinguish Character. Name of the column distinguishing partners
#'   (e.g., gender). Should be a factor for the two-intercept approach.
#' @param dyad_id Character. Name of the dyad identifier column. Default `"dyad_id"`.
#' @param x_partner Character. Name of the partner's predictor column. If `NULL`,
#'   inferred from `x` by appending `_partner`.
#' @param ... Further arguments passed to [nlme::gls()].
#'
#' @return A fitted \code{gls} object from \code{nlme::gls}.
#'
#' @seealso \url{https://randilgarcia.github.io/week-dyad-workshop/Distinguishable.html}
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

  # Ensure distinguish is a factor for varIdent and two-intercept parameterization
  if (!is.factor(data[[distinguish]])) {
    data <- as.data.frame(data)
    data[[distinguish]] <- factor(data[[distinguish]])
  }

  f <- stats::as.formula(paste0(
    y, " ~ ", distinguish, " + ", x, ":", distinguish, " + ", x_partner, ":",
    distinguish, " - 1"
  ))
  nlme::gls(
    model = f,
    data = data,
    correlation = nlme::corCompSymm(form = stats::as.formula(paste0("~ 1 | ", dyad_id))),
    weights = nlme::varIdent(form = stats::as.formula(paste0("~ 1 | ", distinguish))),
    na.action = na.omit,
    ...
  )
}
