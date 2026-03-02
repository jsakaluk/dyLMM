test_that("outputLMMTab returns tibble when gtTab = FALSE", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentQ, dvn = dvn)
  dat_long <- wide_to_long(dat_comp)
  fit <- fitAPIMindist(data = dat_long, y = "y", x = "x")
  tab <- outputLMMTab(fit, gtTab = FALSE)
  expect_s3_class(tab, "tbl_df")
  expect_true("Parameter" %in% names(tab))
  expect_true("Estimate" %in% names(tab))
  expect_true("p" %in% names(tab))
  expect_true(all(tab[["p"]] >= 0 & tab[["p"]] <= 1))
})

test_that("outputLMMTab returns gt when gtTab = TRUE", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentQ, dvn = dvn)
  dat_long <- wide_to_long(dat_comp)
  fit <- fitAPIMindist(data = dat_long, y = "y", x = "x")
  tab <- outputLMMTab(fit, gtTab = TRUE)
  expect_s3_class(tab, "gt_tbl")
})
