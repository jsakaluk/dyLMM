#' Reshape Wide Dyad Data to Long Format
#'
#' Transform wide-format dyadic data (one row per dyad) to long format (two rows
#' per dyad) suitable for LMM fitting. Works with pre-computed composites or
#' with columns named `x_p1`, `x_p2`, `y_p1`, `y_p2`.
#'
#' @param dat Data frame in wide format. Must have columns for actor and partner
#'   values of X and Y, or be passed after `build_composites()`.
#' @param x_p1,x_p2 Column names for partner 1 and 2's X (predictor). Default
#'   `"x_p1"`, `"x_p2"`.
#' @param y_p1,y_p2 Column names for partner 1 and 2's Y (outcome). Default
#'   `"y_p1"`, `"y_p2"`.
#' @param dyad_id Column name identifying dyads. Default `"dyad_id"`.
#' @param partner_col Name for the new column indicating partner (1 vs 2).
#' @param x_col,x_partner_col Names for actor's X and partner's X in long format.
#' @param y_col Name for outcome in long format.
#' @param distinguish Optional. Column name(s) in `dat` that distinguish partners
#'   (e.g., gender). If provided, these are included in the long output.
#' @param partner_labels Optional. Length-2 character or numeric vector giving
#'   labels for partner 1 and 2 (e.g., `c("f", "m")` or `c("Wife", "Husband")`).
#'   When provided, a new column `distinguish` is created from these labels.
#'
#' @return A tibble in long format with columns: `dyad_id`, `partner`
#'   (1/2), `x` (actor's X), `x_partner` (partner's X), `y` (outcome), and
#'   optionally `distinguish` if provided.
#'
#' @importFrom rlang .data
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
#' dat_long <- wide_to_long(dat_comp)
#' }
wide_to_long <- function(dat,
                        x_p1 = "x_p1",
                        x_p2 = "x_p2",
                        y_p1 = "y_p1",
                        y_p2 = "y_p2",
                        dyad_id = "dyad_id",
                        partner_col = "partner",
                        x_col = "x",
                        x_partner_col = "x_partner",
                        y_col = "y",
                        distinguish = NULL,
                        partner_labels = NULL) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame.")
  }

  req <- c(x_p1, x_p2, y_p1, y_p2, dyad_id)
  miss <- req[!req %in% names(dat)]
  if (length(miss) > 0) {
    stop("Required columns not found in `dat`: ", paste(miss, collapse = ", "))
  }

  if (!is.null(distinguish)) {
    dis <- distinguish[distinguish %in% names(dat)]
    if (length(dis) == 0 && length(distinguish) > 0) {
      stop("`distinguish` column(s) not found in `dat`.")
    }
    distinguish <- dis
  }

  id_col <- dyad_id
  if (is.null(distinguish) || length(distinguish) == 0) {
    keep_cols <- id_col
  } else {
    keep_cols <- c(id_col, distinguish)
  }
  value_cols <- c(x_p1, x_p2, y_p1, y_p2)
  select_cols <- c(keep_cols, value_cols)

  if (!is.null(partner_labels)) {
    if (length(partner_labels) != 2) {
      stop("`partner_labels` must be a length-2 vector.")
    }
  }

  sel1 <- dat %>%
    dplyr::select(dplyr::all_of(select_cols))
  args1 <- stats::setNames(
    list(1L, sel1[[x_p1]], sel1[[x_p2]], sel1[[y_p1]]),
    c(partner_col, x_col, x_partner_col, y_col)
  )
  d1 <- sel1 %>%
    dplyr::mutate(!!!args1) %>%
    dplyr::select(-dplyr::any_of(value_cols))

  sel2 <- dat %>%
    dplyr::select(dplyr::all_of(select_cols))
  args2 <- stats::setNames(
    list(2L, sel2[[x_p2]], sel2[[x_p1]], sel2[[y_p2]]),
    c(partner_col, x_col, x_partner_col, y_col)
  )
  d2 <- sel2 %>%
    dplyr::mutate(!!!args2) %>%
    dplyr::select(-dplyr::any_of(value_cols))

  if (!is.null(partner_labels)) {
    d1[["distinguish"]] <- partner_labels[1]
    d2[["distinguish"]] <- partner_labels[2]
  }

  out <- dplyr::bind_rows(d1, d2) %>%
    dplyr::arrange(!!rlang::sym(id_col), !!rlang::sym(partner_col)) %>%
    tibble::as_tibble()

  out
}
