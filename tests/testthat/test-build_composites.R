test_that("build_composites works with dvn from scrapeVarCross", {
  skip_if_not_installed("dySEM")
  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    verbose = FALSE
  )
  out <- build_composites(dat = dySEM::commitmentQ, dvn = dvn)
  expect_equal(nrow(out), nrow(dySEM::commitmentQ))
  expect_true("x_p1" %in% names(out))
  expect_true("x_p2" %in% names(out))
  expect_true("y_p1" %in% names(out))
  expect_true("y_p2" %in% names(out))
  expect_true("dyad_id" %in% names(out))
  expect_equal(out$x_p1[1], mean(as.numeric(dySEM::commitmentQ[1, dvn$p1xvarnames])))
})

test_that("build_composites works with x_vars list", {
  dat <- data.frame(
    id = 1:10,
    x1_A = rnorm(10), x2_A = rnorm(10), x3_A = rnorm(10),
    x1_B = rnorm(10), x2_B = rnorm(10), x3_B = rnorm(10)
  )
  x_vars <- list(p1 = c("x1_A", "x2_A", "x3_A"), p2 = c("x1_B", "x2_B", "x3_B"))
  out <- build_composites(dat = dat, x_vars = x_vars, dyad_id = "id")
  expect_equal(nrow(out), 10)
  expect_true("x_p1" %in% names(out))
  expect_equal(out$x_p1[1], mean(as.numeric(unlist(dat[1, c("x1_A", "x2_A", "x3_A")]))), tolerance = 1e-10)
})

test_that("build_composites rejects invalid inputs", {
  expect_error(build_composites(dat = dySEM::commitmentQ), "Either")
  expect_error(build_composites(dat = "not a df", dvn = list()), "data frame")
})
