trade_fit_data <- function() {
  list(
    prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
    quantities = c(.066, .172, .253, .187, .099, .223) * 100,
    margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
    owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG"),
    tariff = c(0, 0, 0, 0, .1, .1),
    quota = c(Inf, Inf, Inf, Inf, .8, .8)
  )
}

expect_trade_result_parity <- function(actual, expected) {
  expect_equal(class(actual), class(expected))
  expect_equal(actual@pricePre, expected@pricePre, tolerance = 1e-7)
  expect_equal(actual@pricePost, expected@pricePost, tolerance = 1e-7)
  expect_equal(actual@mcPre, expected@mcPre, tolerance = 1e-7)
  expect_equal(actual@mcPost, expected@mcPost, tolerance = 1e-7)
  expect_equal(unname(calcShares(actual, TRUE)), unname(calcShares(expected, TRUE)), tolerance = 1e-7)
  expect_equal(unname(calcShares(actual, FALSE)), unname(calcShares(expected, FALSE)), tolerance = 1e-7)
  expect_equal(unname(calcMargins(actual, TRUE)), unname(calcMargins(expected, TRUE)), tolerance = 1e-7)
  expect_equal(unname(calcMargins(actual, FALSE)), unname(calcMargins(expected, FALSE)), tolerance = 1e-7)
}

test_that("calibrate and simulate separate a Bertrand Logit tariff", {
  x <- trade_fit_data()
  fit <- calibrate(
    demand = "Logit",
    conduct = "Nash-Bertrand",
    prices = x$prices,
    quantities = x$quantities,
    margins = x$margins,
    owner = x$owner
  )
  old <- suppressWarnings(
    bertrand_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )
  new <- simulate(fit, tariffPost = x$tariff)

  expect_s4_class(fit, "TradeFit")
  expect_equal(fit@spec$id, "logit::bertrand")
  expect_equal(fit@diagnostics$route, "calibrate")
  expect_equal(fit@model@tariffPost, rep(0, length(x$prices)))
  expect_trade_result_parity(new, old)
  expect_equal(new@tariffPost, x$tariff)
})

test_that("one calibrated fit supports repeated tariff simulations", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)

  first <- simulate(fit, tariffPost = x$tariff)
  second <- simulate(fit, tariffPost = c(rep(0, 5), .2))
  unchanged <- simulate(fit, tariffPost = rep(0, 6))

  expect_true(second@pricePost[6] > first@pricePost[6])
  expect_equal(unchanged@pricePost, fit@model@pricePre, tolerance = 1e-7)
  expect_equal(fit@model@tariffPost, rep(0, 6))
})

test_that("specify and simulate preserve the supplied-parameter sim path", {
  x <- trade_fit_data()
  parameters <- list(
    alpha = -48.0457,
    meanval = c(0, .4149233, 1.1899885, .8252482, .1460183, 1.4865730)
  )
  fit <- specify(
    model_spec("logit", "bertrand"),
    prices = x$prices,
    parameters = parameters,
    owner = x$owner
  )
  old <- suppressWarnings(
    sim(
      prices = x$prices,
      demand = "logit",
      supply = "bertrand",
      demand.param = parameters,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )

  expect_equal(fit@diagnostics$route, "specify")
  expect_trade_result_parity(simulate(fit, tariffPost = x$tariff), old)
})

test_that("CES uses its complete legacy calibration and simulation path", {
  x <- trade_fit_data()
  fit <- calibrate("ces", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)
  old <- suppressWarnings(
    bertrand_tariff("ces", x$prices, x$quantities, x$margins,
      owner = x$owner, tariffPost = x$tariff)
  )

  expect_trade_result_parity(simulate(fit, tariffPost = x$tariff), old)
})

test_that("quota policy is a separate complete model path", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", policy = "quota",
    prices = x$prices, quantities = x$quantities, margins = x$margins,
    owner = x$owner)
  old <- suppressWarnings(
    bertrand_quota("logit", x$prices, x$quantities, x$margins,
      owner = x$owner, quotaPost = x$quota)
  )

  new <- simulate(fit, quotaPost = x$quota)
  expect_equal(fit@spec$id, "logit::bertrand::quota")
  expect_trade_result_parity(new, old)
})

test_that("post-policy inputs are kept at the simulation boundary", {
  x <- trade_fit_data()
  expect_error(
    calibrate("logit", "bertrand", prices = x$prices,
      quantities = x$quantities, margins = x$margins, owner = x$owner,
      tariffPost = x$tariff),
    "belong in simulate"
  )
  expect_error(
    specify("logit", "bertrand", prices = x$prices,
      parameters = list(alpha = -48, meanval = rep(0, 6)),
      owner = x$owner, tariffPost = x$tariff),
    "belong in simulate"
  )
})
