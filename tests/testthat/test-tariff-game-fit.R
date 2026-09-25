skip_if_not_installed("coordination")

.tariff_game_fixture <- function(tau = c(.1, .1, .2)) {
  coordination::stackelberg(
    prices = c(2, 2.2, 2.4),
    shares = c(.2, .15, .1),
    margins = rep(.3, 3),
    ownerPre = c("A", "A", "B"),
    leadersPre = "A",
    alpha = 1.5,
    insideSize = 100,
    control.equ = list(implicitCheck = FALSE)
  )
}

test_that("native promotion copies baseline state without a solve", {
  source <- .tariff_game_fixture()
  source_price <- source@pricePre
  fit <- trade::as_trade_fit(
    source, tariffPre = c(.1, .1, .2),
    cost_basis = "effective", margin_basis = "net_revenue"
  )
  expect_s4_class(fit, "TariffGameFit")
  expect_equal(fit@model@pricePre, source_price)
  expect_equal(fit@model@pricePost, source_price)
  expect_identical(fit@diagnostics$tariff_reuse$promotion_solves, 0L)
  expect_equal(unname(source@mcPre), unname(fit@tariff_state$kappaPre))
  expect_identical(names(fit@tariff_state$kappaPre), fit@tariff_state$labels)
  expect_equal(fit@tariff_state$cPre, source@mcPre *
                 (1 - fit@tariff_state$tariffPre))
  expect_equal(source@pricePost, source_price)
})

test_that("the scalar tariff contract also covers a single product", {
  source <- coordination::stackelberg(
    prices = 2, shares = .2, margins = .3, ownerPre = "A",
    leadersPre = "A", alpha = 1.5, insideSize = 100,
    control.equ = list(implicitCheck = FALSE)
  )
  fit <- trade::as_trade_fit(source, tariffPre = 0)
  expect_equal(length(fit@tariff_state$kappaPre), 1L)
  expect_equal(fit@tariff_state$cPre, fit@tariff_state$kappaPre)
})

test_that("uniform tariffs can differ across firms and recover physical costs", {
  fit <- trade::as_trade_fit(.tariff_game_fixture(),
                             tariffPre = c(.1, .1, .2),
                             cost_basis = "effective",
                             margin_basis = "net_revenue")
  expect_equal(fit@tariff_state$cPre,
               fit@tariff_state$kappaPre * (1 - fit@tariff_state$tariffPre))
  expect_error(
    trade::as_trade_fit(.tariff_game_fixture(), tariffPre = c(.1, .2, .2),
                        cost_basis = "effective", margin_basis = "net_revenue"),
    class = "trade_tariff_incompatible_baseline"
  )
})

test_that("tariff, physical cost, merger, and combined scenarios use original baseline", {
  fit <- trade::as_trade_fit(.tariff_game_fixture(),
                             tariffPre = c(.1, .1, .2),
                             cost_basis = "effective",
                             margin_basis = "net_revenue")
  result <- trade::simulate(
    fit, tariffPost = c(.2, .2, .2),
    ownerPost = c("A", "A", "A"), leadersPost = "A",
    mcDelta = c(.1, 0, 0)
  )
  expected_c <- fit@tariff_state$cPre * c(1.1, 1, 1)
  expect_equal(result@tariff_state$cPost, expected_c)
  expect_equal(result@tariff_state$kappaPost,
               expected_c / (1 - result@tariff_state$tariffPost))
  expect_equal(result@tariff_state$kappaPost,
               fit@tariff_state$cPre * c(1.1, 1, 1) /
                 (1 - c(.2, .2, .2)))
  expect_equal(unname(fit@tariff_state$tariffPost), c(.1, .1, .2))
  expect_identical(names(fit@tariff_state$tariffPost), fit@tariff_state$labels)
  expect_equal(unname(fit@model@mcPre), unname(fit@tariff_state$kappaPre))
  expect_equal(fit@model@pricePost, fit@model@pricePre)
})

test_that("zero and unchanged scenarios are stable and basis rules are explicit", {
  source <- .tariff_game_fixture()
  zero <- trade::as_trade_fit(source, tariffPre = 0)
  expect_identical(zero@tariff_state$cost_basis, "effective")
  expect_identical(zero@tariff_state$margin_basis, "net_revenue")
  unchanged <- trade::simulate(zero, tariffPost = 0)
  expect_equal(unchanged@model@pricePost, source@pricePre)
  expect_equal(trade::tariff_welfare(unchanged)$physical_plus_government_delta, 0)
  expect_error(trade::as_trade_fit(source, tariffPre = .1),
               class = "trade_tariff_unsupported_cost_basis")
  expect_error(trade::as_trade_fit(source, tariffPre = .1,
                                   cost_basis = "physical",
                                   margin_basis = "net_revenue"),
               class = "trade_tariff_unsupported_cost_basis")
  expect_error(trade::as_trade_fit(source, tariffPre = .1,
                                   cost_basis = "effective"),
               class = "trade_tariff_missing_margin_basis")
})

test_that("accounts expose net revenue, physical profit, and government revenue", {
  fit <- trade::as_trade_fit(.tariff_game_fixture(), tariffPre = .1,
                             cost_basis = "effective",
                             margin_basis = "net_revenue")
  accounts <- trade::tariff_accounts(fit, preMerger = TRUE)
  expect_equal(accounts$net_revenue,
               (1 - accounts$tau) * accounts$gross_revenue)
  expect_equal(accounts$producer_profit,
               accounts$net_revenue - accounts$physical_cost * accounts$q)
  expect_equal(accounts$government_revenue,
               accounts$tau * accounts$gross_revenue)
  expect_equal(unname(trade::calcMC(fit)), accounts$physical_cost)
  expect_identical(names(trade::calcMC(fit)), accounts$product)
})

test_that("tariff wrappers reject legacy lifecycle paths and summarize physical transfers", {
  fit <- trade::as_trade_fit(.tariff_game_fixture(), tariffPre = .1,
                             cost_basis = "effective",
                             margin_basis = "net_revenue")
  expect_error(update(fit), class = "trade_tariff_unsupported_lifecycle")
  expect_error(respecify(fit, demand = "ces"),
               class = "trade_tariff_unsupported_lifecycle")
  path <- add_step(counterfactual(tariff = .2), tariff = .3)
  expect_error(simulate(fit, path), class = "trade_tariff_multiple_steps")
  market <- summary(fit, market = TRUE)
  expect_true(all(c("physicalProducerSurplusDelta",
                    "importingGovernmentRevenueDelta",
                    "physicalPlusGovernmentDelta") %in% names(market)))
})

test_that("fractional or non-block ordinary ownership is rejected", {
  source <- try(antitrust::specify(
    demand = "logit", conduct = "bertrand",
    prices = c(1.5, 1.8, 2.1), shares = c(.3, .25, .15),
    parameters = list(alpha = -1.5, meanval = c(.5, .2, -.1)),
    ownerPre = matrix(c(1, .5, .5, .5, 1, 0, .5, 0, 1), 3, 3,
                      byrow = TRUE), margins = rep(.3, 3), insideSize = 100,
    labels = paste0("P", 1:3)
  ), silent = TRUE)
  skip_if(inherits(source, "try-error"), "antitrust rejected the fractional ownership fixture")
  expect_error(
    trade::as_trade_fit(source, tariffPre = 0,
                        cost_basis = "effective",
                        margin_basis = "net_revenue"),
    class = "trade_tariff_unsupported_ownership"
  )
})

test_that("recorded gross margins are compatible only at a zero baseline tariff", {
  source <- try(antitrust::specify(
    demand = "logit", conduct = "bertrand",
    prices = c(1.5, 1.8, 2.1), shares = c(.3, .25, .15),
    parameters = list(alpha = -1.5, meanval = c(.5, .2, -.1)),
    ownerPre = c("A", "A", "B"), margins = rep(.3, 3),
    insideSize = 100, labels = paste0("P", 1:3)
  ), silent = TRUE)
  skip_if(inherits(source, "try-error"), "antitrust could not build the ordinary fixture")
  source@diagnostics$margin_basis <- "gross_revenue"
  zero <- trade::as_trade_fit(source, tariffPre = 0,
                              cost_basis = "effective",
                              margin_basis = "net_revenue")
  expect_s4_class(zero, "TariffGameFit")
  expect_error(
    trade::as_trade_fit(source, tariffPre = .1,
                        cost_basis = "effective",
                        margin_basis = "net_revenue"),
    class = "trade_tariff_incompatible_basis"
  )
})
