test_that("getICC returns numeric ICC", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentQ, dvn = dvn)
  dat_long <- wide_to_long(dat_comp)
  fit <- fitICC(data = dat_long, y = "y")
  icc <- getICC(fit)
  expect_named(icc, "ICC")
  expect_true(icc["ICC"] >= 0 && icc["ICC"] <= 1)
})

test_that("getICC rejects non-gls", {
  expect_error(getICC(list(a = 1)), "gls")
})
