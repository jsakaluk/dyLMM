#' Export Parameter Table from Fitted LMM
#'
#' Creates a table of fixed-effects parameter estimates and statistical tests
#' from a fitted lme4/lmerTest model. Returns a tibble or a `gt` object for
#' publication. P-values use Satterthwaite degrees of freedom when the model
#' was fit with `lmerTest::lmer()` (via \code{use_lmerTest = TRUE} in the
#' fitting functions); otherwise a z-approximation is used.
#'
#' @param fit A fitted \code{lmerMod} object from \code{lme4::lmer} or
#'   \code{lmerTest::lmer}.
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
  if (!inherits(fit, "lmerMod")) {
    stop("`fit` must be a fitted lmerMod object (from lme4::lmer or lmerTest::lmer).")
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

  if (requireNamespace("broom.mixed", quietly = TRUE)) {
    tab <- broom.mixed::tidy(fit, effects = "fixed")
    names(tab) <- gsub("^term$", "Parameter", names(tab))
    names(tab) <- gsub("^estimate$", "Estimate", names(tab))
    names(tab) <- gsub("^std.error$", "SE", names(tab))
    names(tab) <- gsub("^statistic$", "Statistic", names(tab))
    names(tab) <- gsub("^p.value$", "p", names(tab))
    if (!"p" %in% names(tab) && "Statistic" %in% names(tab)) {
      tab[["p"]] <- 2 * stats::pnorm(-abs(tab[["Statistic"]]))
    }
  } else {
    sm <- summary(fit)$coefficients
    has_p <- "Pr(>|t|)" %in% colnames(sm)
    tab <- tibble::tibble(
      Parameter = rownames(sm),
      Estimate = sm[, "Estimate"],
      SE = sm[, "Std. Error"],
      Statistic = sm[, "t value"],
      p = if (has_p) sm[, "Pr(>|t|)"] else 2 * stats::pnorm(-abs(sm[, "t value"]))
    )
  }
  tab <- tab[, intersect(
    c("Parameter", "Estimate", "SE", "Statistic", "p"),
    names(tab)
  ), drop = FALSE]

  if (!gtTab) {
    return(tibble::as_tibble(tab))
  }

  num_cols <- intersect(c("Estimate", "SE", "Statistic", "p"), names(tab))
  gt_tab <- tab %>%
    gt::gt()
  if (length(num_cols) > 0) {
    gt_tab <- gt_tab %>%
      gt::fmt_number(columns = num_cols, decimals = 3) %>%
      gt::sub_missing(columns = num_cols, missing_text = "")
  }

  if (!is.null(writeTo)) {
    fname <- if (is.null(fileName)) "dyLMM_table" else fileName
    gt::gtsave(gt_tab, filename = paste0(fname, ".rtf"), path = writeTo)
    message(sprintf("Output stored in: %s/%s.rtf", writeTo, fname))
  }

  gt_tab
}
