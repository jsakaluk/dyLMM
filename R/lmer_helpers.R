#' Internal: Fit LMM with Optional lmerTest
#'
#' Fits a linear mixed model via lme4 or lmerTest. When \code{use_lmerTest = TRUE}
#' and the lmerTest package is available, uses \code{lmerTest::lmer()} to obtain
#' Satterthwaite approximate degrees of freedom and p-values. Otherwise falls back
#' to \code{lme4::lmer()}.
#'
#' @param formula A model formula.
#' @param data A data frame.
#' @param use_lmerTest Logical. If \code{TRUE} (default), use lmerTest when
#'   available for p-values. If \code{FALSE}, use lme4 only.
#' @param ... Further arguments passed to \code{lmer()}.
#'
#' @return A fitted model object (\code{lmerMod} or \code{lmerModLmerTest}).
#' @noRd
.lmer_fit <- function(formula, data, use_lmerTest = TRUE, ...) {
  if (!is.logical(use_lmerTest) || length(use_lmerTest) != 1) {
    stop("`use_lmerTest` must be a single logical value.")
  }
  if (use_lmerTest && requireNamespace("lmerTest", quietly = TRUE)) {
    lmerTest::lmer(formula = formula, data = data, ...)
  } else {
    if (use_lmerTest) {
      message(
        "lmerTest not installed. P-values in output will use z-approximation. ",
        "Install lmerTest for Satterthwaite p-values: install.packages(\"lmerTest\")"
      )
    }
    lme4::lmer(formula = formula, data = data, ...)
  }
}
