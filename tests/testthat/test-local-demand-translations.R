trade_translation_data <- function() {
  list(
    prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
    quantities = c(.066, .172, .253, .187, .099, .223) * 100,
    margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
    owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  )
}

expect_trade_translation <- function(source, target, target_demand) {
  source_quantities <- calcQuantities(source@model, preMerger = TRUE)
  use_revenue <- identical(target_demand, "ces")
  source_shares <- calcShares(source@model, preMerger = TRUE,
                              revenue = use_revenue)
  target_shares <- calcShares(target@model, preMerger = TRUE,
                              revenue = use_revenue)
  expect_equal(unname(target@model@pricePre),
               unname(source@model@pricePre), tolerance = 1e-7)
  expect_equal(unname(target_shares), unname(source_shares), tolerance = 1e-7)
  expect_equal(unname(calcQuantities(target@model, preMerger = TRUE)),
               unname(source_quantities), tolerance = 1e-6)
  expect_lte(target@diagnostics$translation$baseline_share_discrepancy,
             1e-8)
  expect_lte(target@diagnostics$translation$baseline_quantity_discrepancy,
             1e-6)
  expect_true(is.finite(target@diagnostics$translation$elasticity_rmse))
  expect_equal(target@diagnostics$translation$transition_kind,
               "algebraic-translation")
  invisible(target)
}

test_that("trade flat Logit and CES translation preserves quantities", {
  x <- trade_translation_data()
  source <- calibrate("logit", "bertrand", prices = x$prices,
                      quantities = x$quantities, margins = x$margins,
                      owner = x$owner)
  target <- respecify(source, demand = "ces", gamma = 1.2)
  expect_trade_translation(source, target, "ces")
  expect_s4_class(target@model, "TariffCES")
  expect_false(isTRUE(all.equal(source@model@mcPre, target@model@mcPre)))
  expect_equal(target@diagnostics$translation$delegated_to,
               "antitrust::respecify")
  expect_equal(target@diagnostics$transition$policy$retained, "tariffPre")

  reverse <- respecify(target, demand = "logit", alpha = -25)
  expect_trade_translation(target, reverse, "logit")
  expect_s4_class(reverse@model, "TariffLogit")
})

test_that("trade translation works for monopolistic competition", {
  x <- trade_translation_data()
  source <- calibrate("logit", "moncom", prices = x$prices,
                      quantities = x$quantities, margins = x$margins)
  target <- respecify(source, demand = "ces", gamma = 1.2)
  expect_trade_translation(source, target, "ces")
  expect_equal(target@spec$conduct, "moncom")
  expect_s4_class(target@model, "TariffMonComCES")
})

test_that("trade demand translation retains the calibrated tariff state", {
  x <- trade_translation_data()
  tariff <- c(0, 0, 0, 0, .1, .1)
  source <- calibrate("logit", "bertrand", prices = x$prices,
                      quantities = x$quantities, margins = x$margins,
                      owner = x$owner, tariffPre = tariff)
  target <- respecify(source, demand = "ces", gamma = 1.2)

  expect_equal(target@model@tariffPre, tariff, tolerance = 0)
  expect_equal(target@model@tariffPost, tariff, tolerance = 0)
  expect_equal(target@diagnostics$transition$policy$retained, "tariffPre")
  expect_equal(target@diagnostics$transition$policy$discarded, "tariffPost")
  expect_lte(target@diagnostics$translation$baseline_price_discrepancy,
             1e-8)
  expect_lte(target@diagnostics$translation$baseline_quantity_discrepancy,
             1e-6)
})

test_that("trade translations require target curvature", {
  x <- trade_translation_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
                   quantities = x$quantities, margins = x$margins,
                   owner = x$owner)
  expect_error(respecify(fit, demand = "ces"),
               "requires explicit target primitive.*gamma")

  ces <- respecify(fit, demand = "ces", gamma = 1.2)
  expect_error(respecify(ces, demand = "logit"),
               "requires explicit target primitive.*alpha")
  expect_error(respecify(fit, demand = "ces", gamma = 0),
               "trade CES 'gamma'.*greater than 1")
  expect_error(respecify(fit, demand = "ces", gamma = 1),
               "trade CES 'gamma'.*greater than 1")
  expect_error(respecify(ces, demand = "logit", alpha = 0),
               "trade Logit 'alpha'.*negative")
})

test_that("trade bargaining transitions require bargaining power", {
  x <- trade_translation_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
                   quantities = x$quantities, margins = x$margins,
                   owner = x$owner)

  expect_error(respecify(fit, conduct = "bargaining"),
               "requires explicit target primitive.*bargpowerPre")

  target <- respecify(fit, conduct = "bargaining",
                      bargpowerPre = rep(.6, length(x$prices)))
  expect_equal(target@model@bargpowerPre, rep(.6, length(x$prices)))
  expect_equal(target@diagnostics$transition$required_arguments,
               "bargpowerPre")
})

test_that("trade keeps unregistered demand transitions unavailable", {
  x <- trade_translation_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
                   quantities = x$quantities, margins = x$margins,
                   owner = x$owner)
  expect_error(respecify(fit, demand = "logit_nests"),
               "unsupported demand")
  expect_error(respecify(fit, demand = "aids"),
               "not supported")
})
