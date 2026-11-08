# A permanent: each test names an economic/API failure missed by one-step
# comparisons. The broader model/variant grid is explicitly extended tier.

.path_source <- function(demand = "logit", conduct = "bertrand", variant = "standard") {
  args <- list(demand = demand, conduct = conduct, variant = variant,
    prices = c(2, 2.2, 2.4, 2.6), ownerPre = c("A", "A", "B", "C"),
    insideSize = 100, labels = c("a", "b", "c", "d"))
  args$parameters <- if (demand == "logit")
    list(alpha = -1.5, meanval = c(3, 2.5, 2, 1.5)) else
    list(gamma = 5, meanval = c(.9, .8, .6, .4), shareInside = .7)
  suppressWarnings(do.call(antitrust::specify, args))
}

.path_fit <- function(demand = "logit", conduct = "bertrand", entrant_tariff = .17) {
  trade::as_trade_fit(.path_source(demand, conduct), entrant_tariff = entrant_tariff)
}

.path_definition <- function() {
  cf <- counterfactual(entry = antitrust::entrant("e", 1, .9, 2, tariff = .23),
    exit = "d", costs = c(a = .1, e = .05))
  cf <- add_step(cf, ownership = c("A", "A", "A", "C", "e"))
  cf <- add_step(cf, ownership = c("A", "A", "A", "C", "A"),
    costs = c(a = -.05, e = .2))
  cf <- add_step(cf, tariff = c(a = 0, b = .12, c = .25, d = .1, e = .18))
  add_step(cf, exit = "b")
}

# Demand is calculated directly from primitives, independently of package
# shares/margin/elasticity functions and the solver's own FOC residuals.
.path_demand <- function(model, price, active) {
  if (methods::is(model, "CES")) {
    g <- model@slopes$gamma
    a <- model@slopes$meanval[active]
    outside <- if (is.na(model@normIndex)) model@priceOutside^(1 - g) else 0
    model@mktSize * a * price^(-g) / (outside + sum(a * price^(1 - g)))
  } else {
    w <- exp(model@slopes$meanval[active] +
      model@slopes$alpha * (price - model@priceOutside))
    model@mktSize * w / ((if (is.na(model@normIndex)) 1 else 0) + sum(w))
  }
}

.path_expect_foc <- function(result, conduct = "bertrand", tolerance = 2e-6) {
  state <- result@tariff_state
  active <- which(state$subsetPost)
  p <- as.numeric(result@model@pricePost[active])
  expect_true(all(is.finite(p) & p > 0))
  c <- as.numeric(state$cPost[active])
  r <- as.numeric(state$retentionPost[active])
  owner <- state$ownerPost[active]
  demand <- function(price) .path_demand(result@model, price, active)
  q <- demand(p)
  expect_true(all(is.finite(q) & q > 0))
  if (conduct == "cournot") {
    if (methods::is(result@model, "CES")) {
      # Legacy CES Cournot holds its calibrated outside-revenue alpha fixed.
      # Its inverse demand derivative therefore differs from inverting the
      # contemporaneous fixed-outside-price Bertrand demand Jacobian.
      g <- result@model@slopes$gamma
      a <- result@model@slopes$alpha
      if (is.null(a)) a <- 0
      b <- (g - 1) * (1 + a) / (g * (1 + g * a))
      inverse <- -diag(p / (g * q)) - b * outer(p, p) / result@model@mktSize
    } else {
      inverse <- solve(numDeriv::jacobian(demand, p))
    }
    residual <- vapply(seq_along(p), function(j) {
      r[j] * p[j] - c[j] + sum((owner == owner[j]) * r * q * inverse[, j])
    }, numeric(1))
  } else {
    residual <- vapply(seq_along(p), function(j) {
      numDeriv::grad(function(x) {
        changed <- p; changed[j] <- x
        sum((owner == owner[j]) * (r * changed - c) * demand(changed))
      }, p[j])
    }, numeric(1))
  }
  expect_lt(max(abs(residual)), tolerance)
  accounts <- trade::tariff_accounts(result)
  expect_equal(accounts$q[active], as.numeric(q), tolerance = 1e-8)
  expect_equal(as.numeric(antitrust::calcMC(result@model, FALSE))[active] * r,
               c, tolerance = 1e-10)
  expect_equal(accounts$producer_profit + accounts$government_revenue,
               accounts$gross_revenue - accounts$physical_cost * accounts$q,
               tolerance = 1e-10)
  expect_true(all(accounts$q[-active] == 0))
  expect_true(all(accounts$government_revenue[-active] == 0))
}

test_that("A: entry, two mergers, heterogeneous tariff and exit retain multiproduct physical-profit FOCs", {
  fit <- .path_fit()
  original <- serialize(fit, NULL)
  path <- suppressWarnings(simulate(fit, .path_definition()))
  for (result in path@results) .path_expect_foc(result)
  expect_identical(final_result(path)@tariff_state$labels, c("a", "b", "c", "d", "e"))
  expect_identical(final_result(path)@tariff_state$subsetPost,
                   c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(as.numeric(final_result(path)@tariff_state$cPost[c("a", "e")]),
    c(as.numeric(fit@tariff_state$cPre[1]) * 1.1 * .95, .9 * 1.05 * 1.2), tolerance = 1e-10)
  expect_identical(serialize(fit, NULL), original)
  expect_identical(final_result(path)@source_fit, fit@source_fit)
})

test_that("A: CES price and Cournot quantity games retain their distinct inverse-demand incentives", {
  for (conduct in c("bertrand", "cournot")) {
    path <- suppressWarnings(simulate(.path_fit("ces", conduct), .path_definition()))
    for (result in path@results) .path_expect_foc(result, conduct)
  }
  path <- suppressWarnings(simulate(.path_fit("logit", "cournot"), .path_definition()))
  .path_expect_foc(final_result(path), "cournot")
})

test_that("A: resume across entry and exit preserves current cost stocks and policy assignments", {
  fit <- .path_fit(entrant_tariff = .19)
  cf <- add_step(counterfactual(exit = "d"), costs = c(a = .1))
  prefix <- suppressWarnings(simulate(fit, cf))
  tail <- counterfactual(entry = antitrust::entrant("e", 1, .9, 2))
  tail <- add_step(tail, costs = c(a = -.05, e = .1), exit = "b")
  resumed <- suppressWarnings(simulate(prefix, tail))
  full_cf <- cf
  full_cf@steps <- c(cf@steps, tail@steps)
  full <- suppressWarnings(simulate(fit, full_cf))
  expect_equal(trade::tariff_accounts(final_result(resumed)),
               trade::tariff_accounts(final_result(full)), tolerance = 1e-8)
  expect_equal(final_result(resumed)@tariff_state$cPost[1],
               fit@tariff_state$cPre[1] * 1.1 * .95, tolerance = 1e-10)
  expect_equal(as.numeric(final_result(resumed)@tariff_state$tariffPost["e"]), .19)
  .path_expect_foc(final_result(resumed))
})

test_that("A: entry policy precedence and pre-entry accounts cannot invent prior entrant output", {
  fit <- .path_fit(entrant_tariff = .31)
  default <- simulate(fit, counterfactual(entry = antitrust::entrant("e", 1, .9, 2)))
  explicit <- simulate(fit, counterfactual(entry = antitrust::entrant("e", 1, .9, 2, tariff = .2)))
  simultaneous <- simulate(fit, counterfactual(
    entry = antitrust::entrant("e", 1, .9, 2, tariff = .2), tariff = .12))
  expect_true(methods::is(default, "TariffGameFit"))
  for (i in seq_along(list(default, explicit, simultaneous))) {
    result <- list(default, explicit, simultaneous)[[i]]
    expect_equal(as.numeric(result@tariff_state$tariffPost["e"]), c(.31, .2, .12)[i])
    expect_equal(trade::tariff_accounts(result, TRUE)$q[5], 0)
    expect_equal(as.numeric(antitrust::calcShares(result, TRUE))[5], 0)
    expect_equal(trade::tariff_accounts(result, TRUE)$q[1:4],
                 trade::tariff_accounts(fit, FALSE)$q, tolerance = 1e-12)
    expect_equal(as.numeric(antitrust::calcMC(result@model, FALSE))[5],
                 .9 / (1 - c(.31, .2, .12)[i]), tolerance = 1e-10)
    .path_expect_foc(result)
  }
  expect_equal(.path_fit(entrant_tariff = 0)@tariff_state$entrant_tariff, 0)
  for (bad in list(1, Inf, NA_real_, c(.1, .2), "0")) {
    expect_error(trade::as_trade_fit(.path_source(), entrant_tariff = bad),
                 class = "trade_tariff_invalid_policy")
  }
})

test_that("A: direct alternatives remain baseline-relative while sequential masks never resurrect exits", {
  fit <- .path_fit()
  first <- suppressWarnings(simulate(fit, subset = c(FALSE, TRUE, TRUE, TRUE), mcDelta = .1))
  alternative <- suppressWarnings(simulate(first, subset = c(TRUE, FALSE, TRUE, TRUE), mcDelta = .1))
  reference <- suppressWarnings(simulate(fit, subset = c(TRUE, FALSE, TRUE, TRUE), mcDelta = .1))
  expect_equal(alternative@model@pricePost, reference@model@pricePost, tolerance = 1e-8)
  expect_equal(alternative@tariff_state$cPost, first@tariff_state$cPost)
  cf <- add_step(counterfactual(exit = "a"), exit = c(TRUE, FALSE, TRUE, TRUE))
  path <- simulate(fit, cf)
  expect_identical(final_result(path)@tariff_state$subsetPost, c(FALSE, FALSE, TRUE, TRUE))
  .path_expect_foc(final_result(path))
})

test_that("A: native core roles can change through the public path API and resume", {
  source <- coordination::core_fringe(demand = "logit", conduct = "bertrand",
    prices = c(2, 2.2, 2.4, 2.6), shares = c(.15, .12, .1, .08),
    margins = rep(NA_real_, 4), ownerPre = c("A", "A", "B", "C"),
    corePre = c("A", "B"), alpha = 1.5, insideSize = 100)
  fit <- trade::as_trade_fit(source, entrant_tariff = .2)
  cf <- add_step(counterfactual(ownership = c("A", "A", "A", "C"), core = "A"),
                 tariff = .12)
  prefix <- suppressWarnings(simulate(fit, cf))
  resumed <- suppressWarnings(simulate(prefix, counterfactual(tariff = .2)))
  full <- suppressWarnings(simulate(fit, add_step(cf, tariff = .2)))
  expect_equal(trade::tariff_accounts(final_result(resumed)),
               trade::tariff_accounts(final_result(full)), tolerance = 1e-8)
  expect_identical(final_result(resumed)@tariff_state$corePost, "A")
  expect_equal(final_result(resumed)@tariff_state$cPost, fit@tariff_state$cPre)
  expect_error(simulate(fit, counterfactual(entry = antitrust::entrant("e", 1, .9, 2))),
               class = "trade_tariff_unsupported_counterfactual")
  expect_error(antitrust::simulate(.path_source(), antitrust::counterfactual(core = "A")),
               "does not support.*core")
})

test_that("A: literal exit to two mergers to tariff to exit retains the prior market in welfare reports", {
  source <- antitrust::specify("logit", "bertrand", prices = c(2, 2.2, 2.4, 2.6, 2.8),
    ownerPre = c("A", "A", "B", "C", "D"), insideSize = 100,
    parameters = list(alpha = -1.5, meanval = c(3, 2.5, 2, 1.5, 1)),
    labels = c("a", "b", "c", "d", "e"))
  fit <- trade::as_trade_fit(source, entrant_tariff = 0)
  cf <- counterfactual(exit = "e")
  cf <- add_step(cf, ownership = c("A", "A", "A", "C", "D"))
  cf <- add_step(cf, ownership = c("A", "A", "A", "A", "D"))
  cf <- add_step(cf, tariff = c(0, .12, .25, .1, 0))
  prefix <- simulate(fit, cf)
  path <- simulate(prefix, counterfactual(exit = "b"))
  result <- final_result(path)
  .path_expect_foc(result)
  for (pre in c(TRUE, FALSE)) {
    accounts <- trade::tariff_accounts(result, pre)
    active <- if (pre) result@tariff_state$subsetPre else result@tariff_state$subsetPost
    expect_true(all(is.finite(antitrust::calcMargins(result, pre)[active])))
    expect_equal(as.numeric(antitrust::calcProducerSurplus(result, pre)),
                 accounts$producer_profit, tolerance = 1e-10)
  }
  # Independent logsum money metric must exclude the first exit from both
  # markets and the second exit only from post, rather than summing NA prices.
  model <- result@model
  mass <- function(p, active) 1 + sum(exp(model@slopes$meanval[active] +
    model@slopes$alpha * (p[active] - model@priceOutside)))
  state <- result@tariff_state
  expected_cv <- model@mktSize / model@slopes$alpha * log(
    mass(model@pricePost, state$subsetPost) / mass(model@pricePre, state$subsetPre))
  expect_equal(antitrust::CV(result), as.numeric(expected_cv), tolerance = 1e-10)
  capture.output(report <- summary(result, market = TRUE))
  expect_true(all(is.finite(as.matrix(report))))
  expect_error(simulate(path, counterfactual(costs = c(b = -.1))), "inactive")
})

test_that("A: active-only CES monopoly without an outside good fails instead of reporting a negative-price root", {
  source <- suppressWarnings(antitrust::specify("ces", "bertrand",
    prices = c(2, 2.2, 2.4), ownerPre = c("A", "B", "C"), insideSize = 100,
    parameters = list(gamma = 5, meanval = c(1, .8, .6)), labels = c("a", "b", "c")))
  fit <- trade::as_trade_fit(source, entrant_tariff = 0)
  cf <- add_step(counterfactual(exit = "c"), ownership = c("A", "A", "C"))
  expect_error(simulate(fit, cf), "without an outside good.*no finite")
})

test_that("A: auction paths carry cumulative cost-relative win probabilities and conditional bids", {
  shares <- c(.12, .1, .08, .07, .06)
  outside <- 1 - sum(shares)
  alpha <- -1.4
  source <- antitrust::specify("logit", "auction2nd", prices = c(2, 2.2, 2.4, 2.6, 2.8),
    margins = -log1p(-shares) / (-alpha * shares),
    parameters = list(alpha = alpha, meanval = log(shares / outside)),
    ownerPre = paste0("F", 1:5), insideSize = 100,
    priceOutside = 0, labels = paste0("P", 1:5))
  fit <- trade::as_trade_fit(source, entrant_tariff = .1)
  cf <- counterfactual(entry = antitrust::entrant("E", log(.05 / outside), .9, 2),
    tariff = .1, costs = c(P1 = .1))
  cf <- add_step(cf, ownership = c("A", "A", "B", "C", "D", "E"), costs = c(P1 = .05))
  cf <- add_step(cf, tariff = .2)
  cf <- add_step(cf, exit = "P5")
  result <- final_result(suppressWarnings(simulate(fit, cf)))
  baseline_cost <- c(as.numeric(fit@tariff_state$cPre), .9)
  physical <- baseline_cost * c(1.1 * 1.05, rep(1, 5))
  effective <- physical / .8
  score <- c(shares / outside, .05 / outside) * exp(alpha * (effective - baseline_cost))
  score[5] <- 0
  win <- score / (1 + sum(score))
  actual_win <- as.numeric(antitrust::calcShares(result, FALSE))
  actual_win[5] <- 0
  expect_equal(actual_win, win, tolerance = 1e-10)
  owners <- result@tariff_state$ownerPost
  firm_win <- vapply(owners, function(f) sum(win[owners == f]), numeric(1))
  predicted_bid <- effective + log1p(-firm_win) / (alpha * firm_win)
  active <- which(result@tariff_state$subsetPost)
  expect_equal(as.numeric(result@model@pricePost[active]), as.numeric(predicted_bid[active]), tolerance = 1e-8)
  expect_equal(as.numeric(result@tariff_state$cPost), physical, tolerance = 1e-10)
  expect_true(is.finite(antitrust::CV(result)))
  mixed <- add_step(cf, tariff = c(.1, .2, .2, .2, .2, .2))
  expect_error(suppressWarnings(simulate(fit, mixed)), class = "trade_tariff_heterogeneous")
})

test_that("A: extended native and ordinary game paths preserve role and policy state across promotion", {
  trade_skip_unless_tier("extended")
  for (demand in c("logit", "ces")) {
    fits <- list(B = .path_fit(demand, "bertrand"),
                 C = .path_fit(demand, "cournot"),
                 MC = .path_fit(demand, "moncom"))
    for (game in c("stackelberg", "core_fringe")) for (conduct in c("bertrand", "cournot")) {
      args <- list(prices = c(2, 2.2, 2.4, 2.6), shares = c(.15, .12, .1, .08),
        margins = rep(NA_real_, 4), ownerPre = c("A", "A", "B", "C"),
        demand = demand, conduct = conduct, insideSize = 100)
      if (demand == "logit") args$alpha <- 1.5 else args$gamma <- 5
      if (game == "stackelberg") args$leadersPre <- "B" else args$corePre <- "B"
      native <- do.call(getExportedValue("coordination", game), args)
      fits[[paste(game, conduct)]] <- trade::as_trade_fit(native, entrant_tariff = .1)
    }
    for (name in names(fits)) {
      fit <- fits[[name]]
      cf <- add_step(counterfactual(tariff = .1), costs = c(-.05, 0, 0, 0))
      cf <- add_step(cf, tariff = .2, exit = 4)
      path <- suppressWarnings(simulate(fit, cf))
      direct <- suppressWarnings(simulate(fit, tariffPost = .2,
        mcDelta = c(-.05, 0, 0, 0), subset = c(TRUE, TRUE, TRUE, FALSE)))
      expect_equal(trade::tariff_accounts(final_result(path)),
        trade::tariff_accounts(direct), tolerance = 1e-6, info = paste(demand, name))
      expect_equal(as.numeric(final_result(path)@tariff_state$cPost),
        as.numeric(fit@tariff_state$cPre) * c(.95, 1, 1, 1), tolerance = 1e-10)
    }
  }
})
