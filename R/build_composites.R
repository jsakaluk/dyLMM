#' Build Composite Scores from Indicator Variables
#'
#' Calculate sum-scores or mean-scores for X and Y from indicator variables.
#' Accepts a `dvn` object from `dySEM::scrapeVarCross()` or explicit variable lists.
#'
#' @param dat Data frame with indicator variables (wide/dyad format).
#' @param dvn Optional. List from `dySEM::scrapeVarCross()` with `p1xvarnames`,
#'   `p2xvarnames`, and optionally `p1yvarnames`, `p2yvarnames`. When `dvn` has
#'   both X and Y (from scrapeVarCross with y_stem), both composites are built.
#' @param x_vars Optional. List with elements `p1` and `p2`, each a character
#'   vector of variable names. Use when not providing `dvn`.
#' @param y_vars Optional. List with elements `p1` and `p2`, each a character
#'   vector. Use when not providing `dvn` and Y composites are needed.
#' @param aggregate Either `"mean"` (default) or `"sum"` for computing composites.
#' @param x_name Character stem for X composite column names (default `"x"`).
#'   Produces `{x_name}_p1`, `{x_name}_p2`.
#' @param y_name Character stem for Y composite column names (default `"y"`).
#' @param dyad_id Character. Name of column in `dat` that identifies dyads. If
#'   `NULL`, a `dyad_id` column is created from row indices (1:nrow).
#'
#' @return Data frame with original columns plus `{x_name}_p1`, `{x_name}_p2`,
#'   and optionally `{y_name}_p1`, `{y_name}_p2`, and `dyad_id` (if not already present).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dySEM)
#' dvn <- scrapeVarCross(
#'   dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
#'   x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
#'   y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
#'   verbose = FALSE
#' )
#' dat_comp <- build_composites(dat = commitmentQ, dvn = dvn)
#' }
build_composites <- function(dat,
                             dvn = NULL,
                             x_vars = NULL,
                             y_vars = NULL,
                             aggregate = c("mean", "sum"),
                             x_name = "x",
                             y_name = "y",
                             dyad_id = NULL) {
  aggregate <- match.arg(aggregate)
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame.")
  }

  fun <- if (aggregate == "mean") base::rowMeans else base::rowSums

  out <- tibble::as_tibble(dat)

  if (is.null(dyad_id)) {
    out[["dyad_id"]] <- seq_len(nrow(dat))
  } else if (!dyad_id %in% names(dat)) {
    stop("`dyad_id` column '", dyad_id, "' not found in `dat`.")
  }

  if (!is.null(dvn)) {
    p1x <- dvn[["p1xvarnames"]]
    p2x <- dvn[["p2xvarnames"]]
    p1y <- dvn[["p1yvarnames"]]
    p2y <- dvn[["p2yvarnames"]]

    if (is.list(p1x) && !is.character(p1x)) {
      p1x <- unlist(p1x, use.names = FALSE)
      p2x <- unlist(p2x, use.names = FALSE)
      p1y <- if (!is.null(p1y)) unlist(p1y, use.names = FALSE) else NULL
      p2y <- if (!is.null(p2y)) unlist(p2y, use.names = FALSE) else NULL
    }

    if (is.null(p1x) || is.null(p2x)) {
      stop("`dvn` must contain `p1xvarnames` and `p2xvarnames`.")
    }
    p1x <- p1x[p1x %in% names(dat)]
    p2x <- p2x[p2x %in% names(dat)]
    if (length(p1x) == 0 || length(p2x) == 0) {
      stop("Indicator variables from `dvn` not found in `dat`.")
    }
    out[[paste0(x_name, "_p1")]] <- fun(as.data.frame(lapply(dat[, p1x, drop = FALSE], as.numeric)))
    out[[paste0(x_name, "_p2")]] <- fun(as.data.frame(lapply(dat[, p2x, drop = FALSE], as.numeric)))

    if (!is.null(p1y) && !is.null(p2y)) {
      p1y <- p1y[p1y %in% names(dat)]
      p2y <- p2y[p2y %in% names(dat)]
      if (length(p1y) > 0 && length(p2y) > 0) {
        out[[paste0(y_name, "_p1")]] <- fun(as.data.frame(lapply(dat[, p1y, drop = FALSE], as.numeric)))
        out[[paste0(y_name, "_p2")]] <- fun(as.data.frame(lapply(dat[, p2y, drop = FALSE], as.numeric)))
      }
    }
  } else if (!is.null(x_vars)) {
    if (!is.list(x_vars) || !all(c("p1", "p2") %in% names(x_vars))) {
      stop("`x_vars` must be a list with elements `p1` and `p2` (character vectors).")
    }
    p1x <- x_vars[["p1"]]
    p2x <- x_vars[["p2"]]
    p1x <- p1x[p1x %in% names(dat)]
    p2x <- p2x[p2x %in% names(dat)]
    if (length(p1x) == 0 || length(p2x) == 0) {
      stop("Indicator variables from `x_vars` not found in `dat`.")
    }
    out[[paste0(x_name, "_p1")]] <- fun(as.data.frame(lapply(dat[, p1x, drop = FALSE], as.numeric)))
    out[[paste0(x_name, "_p2")]] <- fun(as.data.frame(lapply(dat[, p2x, drop = FALSE], as.numeric)))

    if (!is.null(y_vars) && is.list(y_vars) && all(c("p1", "p2") %in% names(y_vars))) {
      p1y <- y_vars[["p1"]][y_vars[["p1"]] %in% names(dat)]
      p2y <- y_vars[["p2"]][y_vars[["p2"]] %in% names(dat)]
      if (length(p1y) > 0 && length(p2y) > 0) {
        out[[paste0(y_name, "_p1")]] <- fun(as.data.frame(lapply(dat[, p1y, drop = FALSE], as.numeric)))
        out[[paste0(y_name, "_p2")]] <- fun(as.data.frame(lapply(dat[, p2y, drop = FALSE], as.numeric)))
      }
    }
  } else {
    stop("Either `dvn` or `x_vars` must be provided.")
  }

  out
}
