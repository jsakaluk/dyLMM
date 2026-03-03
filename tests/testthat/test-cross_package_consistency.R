test_that("fitAPIMindist actor and partner effects match lavaan indistinguishable APIM", {
  skip_if_not_installed("dySEM")
  skip_if_not_installed("lavaan")

  dvn <- dySEM::scrapeVarCross(
    dat = dySEM::commitmentM,
    x_order = "sip", x_stem = "sat.g", x_delim1 = "", x_delim2 = "_",
    y_order = "sip", y_stem = "com", y_delim1 = "", y_delim2 = "_",
    distinguish_1 = "f", distinguish_2 = "m",
    verbose = FALSE
  )
  dat_comp <- build_composites(dat = dySEM::commitmentM, dvn = dvn)
  dat_long <- wide_to_long(dat_comp, partner_labels = c("f", "m"))

  fit_gls <- fitAPIMindist(data = dat_long, y = "y", x = "x")
  coef_gls <- coef(fit_gls)
  actor_gls <- coef_gls["x"]
  partner_gls <- coef_gls["x_partner"]

  obsAPIMScriptIndist <- "
#Actor and Partner Effects
y_p1 ~ a*x_p1 + p*x_p2
y_p2 ~ a*x_p2 + p*x_p1

#ICC and Residual ICC
x_p1 ~~ x_p2
y_p1 ~~ y_p2

#Variances and Residual Variances
x_p1 ~~ vx*x_p1
x_p2 ~~ vx*x_p2

y_p1 ~~ vy*y_p1
y_p2 ~~ vy*y_p2
"
  fit_lavaan <- lavaan::cfa(obsAPIMScriptIndist, data = dat_comp, missing = "ml")
  pe <- lavaan::parameterEstimates(fit_lavaan)
  actor_lavaan <- pe[pe$label == "a", "est"][1]
  partner_lavaan <- pe[pe$label == "p", "est"][1]

  expect_equal(as.numeric(actor_gls), as.numeric(actor_lavaan), tolerance = 0.02)
  expect_equal(as.numeric(partner_gls), as.numeric(partner_lavaan), tolerance = 0.02)
})
