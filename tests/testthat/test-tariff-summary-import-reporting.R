test_that("tariff summaries classify imports by policy and weight the groups", {
  ## Migration parity: A (permanent).
  ## This catches a reporting failure where cost changes are mistaken for
  ## import status, even though tariffs are the policy that defines incidence.
  prices <- c(2, 2.3, 2.6)
  quantities <- c(50, 30, 20)
  tariff_pre <- c(0, 0, 0)
  tariff_post <- c(0, .2, 0)
  result <- suppressWarnings(trade::bertrand_tariff(
    demand = "logit", prices = prices, quantities = quantities,
    margins = c(.3, .3, .3), owner = c("A", "B", "C"),
    tariffPre = tariff_pre, tariffPost = tariff_post,
    labels = c("domestic_A", "import_B", "domestic_C")
  ))

  ## Deliberately make marginal-cost changes disagree with tariff incidence;
  ## changing this bookkeeping slot does not change the solved equilibrium.
  result@mcDelta <- c(.15, 0, -.1)
  detail <- summary(result)
  expect_identical(as.character(detail$isForeign), c(" ", "*", " "))

  market <- summary(result, market = TRUE)
  imported <- tariff_pre > 0 | tariff_post > 0
  expected_domestic <- stats::weighted.mean(
    detail$priceDelta[!imported], detail$sharesPost[!imported]
  )
  expected_import <- stats::weighted.mean(
    detail$priceDelta[imported], detail$sharesPost[imported]
  )
  expect_equal(market[["Domestic Firm Price Change (%)"]],
               expected_domestic, tolerance = 1e-12)
  expect_equal(market[["Foreign Firm Price Change (%)"]],
               expected_import, tolerance = 1e-12)
  expect_gt(abs(expected_domestic - mean(detail$priceDelta[!imported])), 1e-5)
})
