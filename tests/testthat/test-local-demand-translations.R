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
  expect_lte(target@diagnostics$local_translation$baseline_share_discrepancy,
             1e-8)
  expect_lte(target@diagnostics$local_translation$baseline_quantity_discrepancy,
             1e-6)
  expect_true(is.finite(target@diagnostics$local_translation$elasticity_rmse))
  expect_equal(target@diagnostics$local_translation$transition_type,
               "local-demand-translation")
  invisible(target)
}

test_that("trade flat Logit and CES translation preserves quantities", {
  x <- trade_translation_data()
  source <- calibrate("logit", "bertrand", prices = x$prices,
                      quantities = x$quantities, margins = x$margins,
                      owner = x$owner)
  target <- respecify(source, demand = "ces")
  expect_trade_translation(source, target, "ces")
  expect_s4_class(target@model, "TariffCES")
  expect_false(isTRUE(all.equal(source@model@mcPre, target@model@mcPre)))

  reverse <- respecify(target, demand = "logit")
  expect_trade_translation(target, reverse, "logit")
  expect_s4_class(reverse@model, "TariffLogit")
})

test_that("trade translation works for monopolistic competition", {
  x <- trade_translation_data()
  source <- calibrate("logit", "moncom", prices = x$prices,
                      quantities = x$quantities, margins = x$margins)
  target <- respecify(source, demand = "ces")
  expect_trade_translation(source, target, "ces")
  expect_equal(target@spec$conduct, "moncom")
  expect_s4_class(target@model, "TariffMonComCES")
})

test_that("trade keeps nested demand transitions unavailable until registered", {
  x <- trade_translation_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
                   quantities = x$quantities, margins = x$margins,
                   owner = x$owner)
  expect_error(respecify(fit, demand = "logit_nests"),
               "unsupported demand")
})
