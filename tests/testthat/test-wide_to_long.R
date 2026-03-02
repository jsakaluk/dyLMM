test_that("wide_to_long produces correct structure", {
  dat <- data.frame(
    dyad_id = 1:5,
    x_p1 = rnorm(5), x_p2 = rnorm(5),
    y_p1 = rnorm(5), y_p2 = rnorm(5)
  )
  out <- wide_to_long(dat)
  expect_equal(nrow(out), 10L)
  expect_true(all(c("dyad_id", "partner", "x", "x_partner", "y") %in% names(out)))
  expect_equal(out$y[1], dat$y_p1[1])
  expect_equal(out$y[2], dat$y_p2[1])
  expect_equal(out$x[1], dat$x_p1[1])
  expect_equal(out$x_partner[1], dat$x_p2[1])
})

test_that("wide_to_long works with partner_labels", {
  dat <- data.frame(
    dyad_id = 1:3,
    x_p1 = 1:3, x_p2 = 4:6, y_p1 = 7:9, y_p2 = 10:12
  )
  out <- wide_to_long(dat, partner_labels = c("f", "m"))
  expect_true("distinguish" %in% names(out))
  expect_equal(out$distinguish[out$partner == 1], rep("f", 3))
  expect_equal(out$distinguish[out$partner == 2], rep("m", 3))
})

test_that("wide_to_long rejects missing columns", {
  dat <- data.frame(dyad_id = 1:2, x_p1 = 1:2)
  expect_error(wide_to_long(dat), "Required columns")
})
