test_that("getRho returns numeric rho for gls with corCompSymm", {
  skip_if_not_installed("nlme")
  # Minimal dyad data: 2 rows per dyad
  set.seed(1)
  n <- 20
  dat <- data.frame(
    dyad_id = rep(1:n, each = 2),
    y = rnorm(2 * n)
  )
  fit <- nlme::gls(
    y ~ 1,
    data = dat,
    correlation = nlme::corCompSymm(form = ~1 | dyad_id),
    na.action = na.omit
  )
  rho <- getRho(fit)
  expect_named(rho, "rho")
  expect_true(rho["rho"] >= -1 && rho["rho"] <= 1)
})

test_that("getRho rejects non-gls", {
  expect_error(getRho(list(a = 1)), "gls")
})

test_that("getRho returns CI when ci is specified", {
  skip_if_not_installed("nlme")
  set.seed(2)
  n <- 25
  dat <- data.frame(
    dyad_id = rep(1:n, each = 2),
    y = rnorm(2 * n)
  )
  fit <- nlme::gls(
    y ~ 1,
    data = dat,
    correlation = nlme::corCompSymm(form = ~1 | dyad_id),
    na.action = na.omit
  )
  rho <- getRho(fit, ci = 0.95)
  expect_true("rho" %in% names(rho))
  expect_true("CI_low" %in% names(rho))
  expect_true("CI_high" %in% names(rho))
  expect_true(rho["CI_low"] <= rho["rho"])
  expect_true(rho["CI_high"] >= rho["rho"])
})

test_that("getRho rejects gls without corStruct", {
  skip_if_not_installed("nlme")
  dat <- data.frame(x = 1:10, y = rnorm(10))
  fit <- nlme::gls(y ~ x, data = dat)
  expect_error(getRho(fit), "corCompSymm")
})
