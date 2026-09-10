test_that("TradeFit inherits the shared structural fit contract", {
  expected_slots <- c("spec", "model", "parameters", "observed",
                      "diagnostics")

  expect_true(methods::isClass("TradeFit"))
  expect_true(methods::extends("TradeFit", "StructuralFit"))
  expect_equal(methods::slotNames("TradeFit"), expected_slots)
  expect_equal(methods::slotNames("StructuralFit"), expected_slots)
})


test_that("TradeFit construction retains its visible slots", {
  fit <- methods::new(
    "TradeFit",
    spec = list(id = "test::trade"),
    model = list(state = TRUE),
    parameters = list(alpha = -1.2),
    observed = list(prices = c(1, 2)),
    diagnostics = list(status = "constructed")
  )

  expect_s4_class(fit, "TradeFit")
  expect_s4_class(fit, "StructuralFit")
  expect_equal(fit@spec$id, "test::trade")
  expect_true(isTRUE(fit@model$state))
  expect_equal(fit@parameters$alpha, -1.2)
})


test_that("named trade simulate calls retain trade dispatch", {
  prices <- c(.0441, .0328, .0409, .0396, .0387, .0497)
  quantities <- c(.066, .172, .253, .187, .099, .223) * 100
  margins <- c(.3830, .5515, .5421, .5557, .4453, .3769)
  owner <- c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  tariff <- c(0, 0, 0, 0, .1, .1)

  fit <- suppressWarnings(calibrate(
    demand = "logit", conduct = "bertrand", prices = prices,
    quantities = quantities, margins = margins, owner = owner
  ))
  result <- suppressWarnings(trade::simulate(fit, tariffPost = tariff))

  expect_s4_class(fit, "StructuralFit")
  expect_s4_class(result, "TariffLogit")
  expect_equal(result@tariffPost, tariff)
})


test_that("TradeFit survives an RDS round trip", {
  fit <- methods::new(
    "TradeFit",
    spec = list(id = "test::rds"),
    model = list(state = TRUE),
    parameters = list(alpha = -1.2),
    observed = list(prices = c(1, 2)),
    diagnostics = list(status = "constructed")
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  saveRDS(fit, path)
  restored <- readRDS(path)

  expect_s4_class(restored, "TradeFit")
  expect_s4_class(restored, "StructuralFit")
  expect_equal(restored, fit)
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


test_that("a StructuralFit subclass that is neither TradeFit nor AntitrustFit still hits the shared fallback", {
  ## The shared StructuralFit fallback methods, owned by antitrust, must keep
  ## producing an explicit "no method defined" error for any StructuralFit
  ## subclass -- including a hypothetical third sibling family -- that
  ## supplies neither a TradeFit nor an AntitrustFit method. Trade must not
  ## touch or duplicate that fallback.
  setClass("DummyTradeSiblingFit", contains = "StructuralFit")
  on.exit(removeClass("DummyTradeSiblingFit"), add = TRUE)
  dummy <- methods::new("DummyTradeSiblingFit")

  expect_true(is(dummy, "StructuralFit"))
  expect_false(is(dummy, "TradeFit"))
  expect_false(is(dummy, "AntitrustFit"))

  expect_error(
    antitrust::simulate(dummy, ownerPost = c("A", "B")),
    "no simulate\\(\\) method is defined"
  )
  expect_error(
    antitrust::respecify(dummy, conduct = "cournot"),
    "no respecify\\(\\) method is defined"
  )
})
