test_that("fitAPIMdist produces lmerMod with factor distinguish", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentM, x_order = "sip", x_stem = "sat.g",
    x_delim1 = "", x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m",
    y_order = "sip", y_stem = "com", y_delim1 = "", y_delim2 = "_",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentM, dvn = dvn)
  dat_long <- wide_to_long(dat_comp, partner_labels = c("f", "m"))
  fit <- fitAPIMdist(
    data = dat_long, y = "y", x = "x", distinguish = "distinguish"
  )
  expect_s4_class(fit, "lmerMod")
})
