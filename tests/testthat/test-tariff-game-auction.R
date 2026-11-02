skip_if_not_installed("antitrust")

.auction_expect_numeric <- function(actual, expected, ...) {
  expect_true(is.numeric(actual) && is.numeric(expected))
  # Public legacy methods differ in optional names. Product identity/order is
  # checked separately, while this assertion tests the economic values.
  expect_equal(as.numeric(actual), as.numeric(expected), ...)
}

.auction_policy_source <- function() {
  labels <- paste0("P", seq_len(5L))
  shares <- c(.08, .12, .10, .09, .06)
  outside <- 1 - sum(shares)
  alpha <- -1.4
  prices <- c(2, 2.2, 2.4, 2.6, 2.8)
  markup <- -log1p(-shares) / (-alpha * shares)
  antitrust::specify(
    demand = "logit", conduct = "auction2nd", prices = prices,
    margins = markup,
    parameters = list(alpha = alpha, meanval = log(shares / outside)),
    ownerPre = paste0("F", seq_len(5L)), insideSize = 100,
    priceOutside = 0, labels = labels
  )
}

.auction_policy_owner <- function(pair = NULL) {
  owner <- paste0("F", seq_len(5L))
  if (!is.null(pair)) owner[pair] <- paste0("M", paste(pair, collapse = ""))
  owner
}

.auction_policy_fit <- function() {
  trade::as_trade_fit(
    .auction_policy_source(), tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue"
  )
}

test_that("standard output Logit auction promotion preserves a positive outside share", {
  source <- .auction_policy_source()
  fit <- .auction_policy_fit()
  shares <- antitrust::calcShares(source@model, preMerger = TRUE)
  expect_true(sum(shares) < 1)
  expect_s4_class(fit, "TariffGameFit")
  .auction_expect_numeric(antitrust::calcShares(fit@model, preMerger = TRUE), shares,
               tolerance = 1e-10)
  .auction_expect_numeric(fit@tariff_state$kappaPre,
               antitrust::calcMC(source@model, preMerger = TRUE),
               tolerance = 1e-10)
  expect_identical(fit@tariff_state$source_model@slopes$meanval,
                   source@model@slopes$meanval)
})

test_that("only standard output Logit auction enters the explicit route", {
  source <- .auction_policy_source()
  ces <- try(antitrust::specify(
    demand = "ces", conduct = "auction2nd", prices = source@model@pricePre,
    shares = c(.08, .12, .10, .09, .06), margins = rep(.3, 5),
    parameters = list(gamma = 2, meanval = rep(1, 5)),
    ownerPre = .auction_policy_owner(), insideSize = 100,
    priceOutside = 1, labels = source@model@labels
  ), silent = TRUE)
  skip_if(inherits(ces, "try-error"),
          "the installed antitrust does not construct standard CES auction fits")
  expect_error(
    trade::as_trade_fit(
      ces, tariffPre = 0, cost_basis = "effective",
      margin_basis = "net_revenue"
    ),
    class = "trade_tariff_unsupported_model"
  )
})

test_that("auction zero tariff is a no-op and retains immutable baseline state", {
  fit <- .auction_policy_fit()
  source_meanval <- fit@tariff_state$source_model@slopes$meanval
  source_mc <- fit@tariff_state$source_model@mcPre
  source_state <- fit@tariff_state[c("kappaPre", "cPre", "ownerPre")]
  result <- trade::simulate(fit, tariffPost = 0)
  .auction_expect_numeric(as.numeric(antitrust::calcPrices(result@model, preMerger = FALSE)),
               as.numeric(antitrust::calcPrices(fit@model, preMerger = TRUE)),
               tolerance = 1e-10)
  .auction_expect_numeric(as.numeric(antitrust::calcShares(result@model, preMerger = FALSE)),
               as.numeric(antitrust::calcShares(fit@model, preMerger = TRUE)),
               tolerance = 1e-10)
  expect_equal(fit@tariff_state[c("kappaPre", "cPre", "ownerPre")],
               source_state)
  expect_identical(fit@tariff_state$source_model@slopes$meanval, source_meanval)
  expect_identical(fit@tariff_state$source_model@mcPre, source_mc)
})

test_that("auction 25 and 50 percent statutory tariffs use consumer fractions and additive kappa shocks", {
  fit <- .auction_policy_fit()
  kpre <- fit@tariff_state$kappaPre
  cpre <- fit@tariff_state$cPre

  result25 <- trade::simulate(fit, tariffPost = .2)
  result50 <- trade::simulate(fit, tariffPost = 1 / 3)

  .auction_expect_numeric(result25@tariff_state$kappaPost, kpre / .8,
               tolerance = 1e-10)
  .auction_expect_numeric(result50@tariff_state$kappaPost, kpre / (2 / 3),
               tolerance = 1e-10)
  .auction_expect_numeric(result25@tariff_state$cPost, cpre, tolerance = 1e-10)
  .auction_expect_numeric(result50@tariff_state$cPost, cpre, tolerance = 1e-10)
  .auction_expect_numeric(antitrust::calcMC(result25@model, preMerger = FALSE),
               result25@tariff_state$kappaPost, tolerance = 1e-10)
  .auction_expect_numeric(antitrust::calcMC(result50@model, preMerger = FALSE),
               result50@tariff_state$kappaPost, tolerance = 1e-10)
  .auction_expect_numeric(result25@model@pricePost - result25@tariff_state$kappaPost,
               antitrust::calcMargins(result25@model, preMerger = FALSE,
                                      level = TRUE), tolerance = 1e-8)
  .auction_expect_numeric(result50@model@pricePost - result50@tariff_state$kappaPost,
               antitrust::calcMargins(result50@model, preMerger = FALSE,
                                      level = TRUE), tolerance = 1e-8)
})

test_that("legacy TradeFit auction recalc stores the converted level cost before pricing", {
  source <- .auction_policy_source()
  legacy <- trade::specify(
    demand = "logit", conduct = "auction2nd", prices = source@model@pricePre,
    parameters = source@model@slopes, owner = .auction_policy_owner(),
    tariffPre = rep(0, 5L), insideSize = 100, priceOutside = 0,
    labels = source@model@labels
  )
  result <- trade::simulate(legacy, tariffPost = rep(.2, 5L))
  model <- if (methods::is(result, "TradeFit")) result@model else result
  .auction_expect_numeric(as.numeric(model@mcPost),
               as.numeric(model@mcPre / .8), tolerance = 1e-10)
  expect_identical(names(model@mcPost), names(model@mcPre))
  actual_margin <- model@pricePost - model@mcPost
  expected_margin <- antitrust::calcMargins(model, preMerger = FALSE,
                                            level = TRUE)
  .auction_expect_numeric(as.numeric(actual_margin), as.numeric(expected_margin),
               tolerance = 1e-8)
  expect_identical(as.character(model@labels), as.character(source@model@labels))
})

test_that("auction merger, cost, and combined counterfactuals use original physical costs", {
  fit <- .auction_policy_fit()
  cpre <- fit@tariff_state$cPre
  pair <- .auction_policy_owner(c(2L, 3L))
  merger <- trade::simulate(fit, tariffPost = .2, ownerPost = pair)
  cost <- trade::simulate(fit, tariffPost = 0, mcDelta = .1)
  combined <- trade::simulate(fit, tariffPost = .2, ownerPost = pair,
                              mcDelta = .1)

  expect_identical(merger@tariff_state$ownerPost, pair)
  .auction_expect_numeric(merger@tariff_state$cPost, cpre, tolerance = 1e-10)
  .auction_expect_numeric(cost@tariff_state$cPost, cpre * 1.1, tolerance = 1e-10)
  .auction_expect_numeric(combined@tariff_state$cPost, cpre * 1.1, tolerance = 1e-10)
  .auction_expect_numeric(combined@tariff_state$kappaPost, cpre * 1.1 / .8,
               tolerance = 1e-10)
  actual_quantity <- trade::calcQuantities(combined, preMerger = FALSE)
  expected_quantity <- antitrust::calcQuantities(combined@model, preMerger = FALSE)
  .auction_expect_numeric(as.numeric(actual_quantity), as.numeric(expected_quantity),
               tolerance = 1e-8)
  expect_identical(names(expected_quantity), as.character(combined@model@labels))
  expect_identical(trade::tariff_accounts(combined, FALSE)$product, as.character(combined@model@labels))
  actual_margin <- trade::calcMargins(combined, preMerger = FALSE, level = FALSE)
  expected_margin <- trade::tariff_accounts(combined, FALSE)$net_margin
  .auction_expect_numeric(as.numeric(actual_margin), as.numeric(expected_margin),
               tolerance = 1e-10)
  expect_identical(names(actual_margin), as.character(combined@model@labels))
})

test_that("auction tariff uniformity is enforced after merger ownership changes", {
  fit <- .auction_policy_fit()
  expect_error(
    trade::simulate(
      fit, tariffPost = c(0, .2, .25, .2, .2),
      ownerPost = .auction_policy_owner(c(2L, 3L))
    ),
    class = "trade_tariff_heterogeneous"
  )
  for (pair in list(c(2L, 3L), c(3L, 4L), c(4L, 5L))) {
    result <- trade::simulate(
      fit, tariffPost = c(0, .2, .2, .2, .2),
      ownerPost = .auction_policy_owner(pair)
    )
    expect_identical(result@tariff_state$ownerPost,
                     .auction_policy_owner(pair))
  }
})

test_that("auction CV equals the established native oracle", {
  fit <- .auction_policy_fit()
  result <- trade::simulate(fit, tariffPost = .2)
  model <- result@model
  alpha <- model@slopes$alpha
  a <- -alpha
  eta_pre <- model@slopes$meanval
  eta_post <- eta_pre + alpha *
    (model@mcPost - model@mcPre - model@priceOutside)
  v_pre <- 1 + sum(exp(eta_pre))
  v_post <- 1 + sum(exp(eta_post))
  markup_pre <- antitrust::calcMargins(model, preMerger = TRUE,
                                       exAnte = TRUE, level = TRUE)
  markup_post <- antitrust::calcMargins(model, preMerger = FALSE,
                                        exAnte = TRUE, level = TRUE)
  oracle <- (sum(markup_post) - sum(markup_pre) -
             log(v_post / v_pre) / a) * model@mktSize
  .auction_expect_numeric(as.numeric(trade::CV(result)), as.numeric(oracle),
               tolerance = 1e-8)
})

test_that("explicit auction adapter rejects ALM and input specifications", {
  source <- .auction_policy_source()
  alm <- source
  alm@spec <- antitrust::model_spec("logit", "auction2nd", variant = "alm")
  expect_error(trade::as_trade_fit(alm, tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue"),
    class = "trade_tariff_unsupported_model")
  input <- source
  input@model@output <- FALSE
  expect_error(trade::as_trade_fit(input, tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue"),
    class = "trade_tariff_unsupported_market_side")
})
