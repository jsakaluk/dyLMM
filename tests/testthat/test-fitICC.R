test_that("fitICC produces lmerMod", {
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
  expect_s4_class(fit, "lmerMod")
  expect_true(lme4::isLMM(fit))
})

test_that("fitICC accepts use_lmerTest = FALSE", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentQ, dvn = dvn)
  dat_long <- wide_to_long(dat_comp)
  fit <- fitICC(data = dat_long, y = "y", use_lmerTest = FALSE)
  expect_s4_class(fit, "lmerMod")
})

test_that("fitICC rejects invalid inputs", {
  dat <- data.frame(dyad_id = 1:4, y = rnorm(4))
  expect_error(fitICC(data = dat, y = "missing"), "not found")
})
