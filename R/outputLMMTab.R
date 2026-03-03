#' Export Parameter Table from Fitted LMM
#'
#' Creates a table of fixed-effects parameter estimates and statistical tests
#' from a fitted nlme::gls model. Returns a tibble or a `gt` object for
#' publication. P-values are computed natively by gls (t-tests).
#'
#' @param fit A fitted \code{gls} object from \code{\link{fitAPIMindist}},
#'   \code{\link{fitAPIMdist}}, or \code{\link{fitICC}}.
#' @param gtTab Logical. If `TRUE`, return a [gt::gt()] object. If `FALSE`
#'   (default), return a [tibble::tibble()].
#' @param writeTo Optional. Directory path to save the table as `.rtf`. Requires
#'   `gtTab = TRUE`.
#' @param fileName Optional. Base filename for the `.rtf` (without extension).
#'
#' @return A tibble (if `gtTab = FALSE`) or gt object (if `gtTab = TRUE`) with
#'   columns: Parameter, Estimate, SE, Statistic (t), p-value.
#'
#' @details When `gtTab = TRUE` and `writeTo` is specified, the table is saved
#'   as an `.rtf` file. A confirmation message is printed on success.
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
#' outputLMMTab(fit)
#' outputLMMTab(fit, gtTab = TRUE)
#' }
outputLMMTab <- function(fit,
                        gtTab = FALSE,
                        writeTo = NULL,
                        fileName = NULL) {
  if (!inherits(fit, "gls")) {
    stop("`fit` must be a fitted gls object (from nlme::gls).")
  }
  if (!is.logical(gtTab) || length(gtTab) != 1) {
    stop("`gtTab` must be a single logical value.")
  }

  if (gtTab && !is.null(writeTo)) {
    if (!is.character(writeTo)) {
      stop("`writeTo` must be a character string.")
    }
    if (!dir.exists(writeTo)) {
      stop("Directory '", writeTo, "' does not exist.")
    }
    if (!is.null(fileName) && !is.character(fileName)) {
      stop("`fileName` must be a character string.")
    }
  }

  sm <- summary(fit)$tTable
  tab <- tibble::tibble(
    Parameter = rownames(sm),
    Estimate = sm[, "Value"],
    SE = sm[, "Std.Error"],
    Statistic = sm[, "t-value"],
    p = sm[, "p-value"]
  )

  if (!gtTab) {
    return(tibble::as_tibble(tab))
  }

  num_cols <- c("Estimate", "SE", "Statistic", "p")
  gt_tab <- tab %>%
    gt::gt()
  gt_tab <- gt_tab %>%
    gt::fmt_number(columns = num_cols, decimals = 3) %>%
    gt::sub_missing(columns = num_cols, missing_text = "")

  if (!is.null(writeTo)) {
    fname <- if (is.null(fileName)) "dyLMM_table" else fileName
    gt::gtsave(gt_tab, filename = paste0(fname, ".rtf"), path = writeTo)
    message(sprintf("Output stored in: %s/%s.rtf", writeTo, fname))
  }

  gt_tab
}
