test_that("TradeFit inherits the shared structural fit contract", {
  expected_slots <- c("spec", "model", "parameters", "observed",
                      "diagnostics")

  expect_true(methods::isClass("TradeFit"))
  expect_true(methods::extends("TradeFit", "StructuralFit"))
  expect_equal(methods::slotNames("TradeFit"), expected_slots)
  expect_equal(methods::slotNames("StructuralFit"), expected_slots)
})


test_that("a saved fitted trade model retains its tariff economics", {
  fit <- suppressWarnings(calibrate(
    "logit", "bertrand", prices = c(2, 2.2, 2.5),
    quantities = c(35, 25, 20), margins = c(.4, .35, .3),
    owner = c("A", "A", "B")
  ))
  restored <- unserialize(serialize(fit, NULL))
  tariff <- c(0, .1, .2)
  actual <- suppressWarnings(simulate(restored, tariffPost = tariff))
  expected <- suppressWarnings(simulate(fit, tariffPost = tariff))

  expect_s4_class(restored, "TradeFit")
  expect_equal(actual@pricePost, expected@pricePost, tolerance = 1e-10)
  expect_equal(actual@mcPost, expected@mcPost, tolerance = 1e-10)
})


test_that("antitrust::simulate() reaches trade's own TradeFit method via shared dispatch", {
  ## simulate() is a single S4 generic owned by antitrust; trade supplies a
  ## method for its own "TradeFit" signature. Calling antitrust::simulate()
  ## and trade::simulate() on a TradeFit therefore dispatch to the exact same
  ## registered method -- there is no rejection to test for here anymore.
  prices <- c(.0441, .0328, .0409, .0396, .0387, .0497)
  quantities <- c(.066, .172, .253, .187, .099, .223) * 100
  margins <- c(.3830, .5515, .5421, .5557, .4453, .3769)
  owner <- c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  tariff <- c(0, 0, 0, 0, .1, .1)

  fit <- suppressWarnings(calibrate(
    demand = "logit", conduct = "bertrand", prices = prices,
    quantities = quantities, margins = margins, owner = owner
  ))

  via_antitrust <- suppressWarnings(antitrust::simulate(fit, tariffPost = tariff))
  via_trade <- suppressWarnings(trade::simulate(fit, tariffPost = tariff))

  expect_s4_class(via_antitrust, "TariffLogit")
  expect_equal(via_antitrust, via_trade)
})


test_that("TradeFit exposes trade-specific counterfactual validation", {
  prices <- c(.0441, .0328, .0409, .0396, .0387, .0497)
  quantities <- c(.066, .172, .253, .187, .099, .223) * 100
  margins <- c(.3830, .5515, .5421, .5557, .4453, .3769)
  owner <- c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  fit <- suppressWarnings(calibrate(
    demand = "logit", conduct = "bertrand", prices = prices,
    quantities = quantities, margins = margins, owner = owner
  ))
  tariff_cf <- counterfactual(tariff = c(0, 0, 0, 0, .1, .1))

  expect_true(methods::existsMethod("validate_counterfactual", "TradeFit"))
  expect_identical(
    antitrust::validate_counterfactual(fit, tariff_cf),
    tariff_cf
  )
  expect_error(
    antitrust::validate_counterfactual(
      fit, counterfactual(quota = rep(Inf, length(prices)))
    ),
    "does not support counterfactual field\\(s\\): quota"
  )
})
