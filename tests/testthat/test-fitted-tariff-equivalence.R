# Independent integration matrix: production-cost accounting is checked
# directly and equilibria are compared to the public underlying-game API.
.tariff_matrix_source <- function(demand, family, roles = c("A", "B")) {
  prices <- c(2, 2.2, 2.4, 2.6)
  shares <- c(.15, .10, .10, .08)
  owners <- c("A", "A", "B", "C")
  labels <- paste0("product_", seq_along(prices))
  if (family %in% c("B", "C", "MC")) {
    conduct <- c(B = "bertrand", C = "cournot", MC = "moncom")[[family]]
    parameters <- if (demand == "logit")
      list(alpha = -1.5, meanval = c(1, .8, .6, .4)) else
      list(gamma = 5, meanval = c(1, .8, .6, .4))
    return(suppressWarnings(antitrust::specify(
      demand, conduct, prices = prices, shares = shares,
      ownerPre = owners, parameters = parameters, labels = labels,
      insideSize = 100)))
  }
  args <- list(prices = prices, shares = shares, ownerPre = owners,
               margins = rep(NA_real_, length(prices)), demand = demand,
               conduct = if (family %in% c("BF", "BS")) "bertrand" else "cournot",
               insideSize = 100, labels = labels)
  if (demand == "logit") args$alpha <- 1.5 else args$gamma <- 5
  if (family %in% c("BF", "CF")) {
    args$corePre <- roles
    do.call(coordination::core_fringe, args)
  } else {
    args$leadersPre <- roles
    do.call(coordination::stackelberg, args)
  }
}

test_that("all fourteen output games agree under physical costs and tariff wedges", {
  trade_skip_unless_tier("extended")
  for (demand in c("logit", "ces")) for (family in c("B", "C", "MC", "BF", "CF", "BS", "CS")) {
    source <- .tariff_matrix_source(demand, family)
    original <- serialize(source, NULL)
    pre <- c(.10, .10, .20, .05)
    promoted <- trade::as_trade_fit(source, tariffPre = pre,
      cost_basis = "effective", margin_basis = "net_revenue")
    expect_s4_class(promoted, "TariffGameFit")
    expect_equal(unname(trade::calcMC(promoted)),
                 unname(promoted@tariff_state$kappaPre * (1 - pre)), tolerance = 1e-12)
    expect_equal(promoted@tariff_state$promotion_solves, 0L)
    no_change <- trade::simulate(promoted)
    expect_equal(unname(no_change@model@pricePost), unname(no_change@model@pricePre), tolerance = 1e-12)
    expect_equal(trade::tariff_welfare(no_change)$physical_plus_government_delta, 0, tolerance = 1e-12)
    native <- !methods::is(source, "AntitrustFit")
    stack <- family %in% c("BS", "CS")
    for (kind in c("merger", "tariff", "cost", "combined")) {
      merger <- kind %in% c("merger", "combined")
      owners <- promoted@tariff_state$ownerPre
      if (merger) {
        firms <- unique(owners)
        owners[owners == firms[[2L]]] <- firms[[1L]]
      }
      post <- if (kind == "cost") pre else rep(.15, 4L)
      physical_delta <- if (kind %in% c("cost", "combined")) rep(.1, 4L) else rep(0, 4L)
      effective_delta <- (1 + physical_delta) * (1 - pre) / (1 - post) - 1
      role_args <- if (!native) list() else if (stack)
        list(leadersPost = if (merger) "A" else c("A", "B")) else
        list(corePost = if (merger) "A" else c("A", "B"))
      result <- do.call(trade::simulate, c(list(promoted, tariffPost = post,
        ownerPost = owners, mcDelta = physical_delta), role_args))
      expected <- if (!native) antitrust::simulate(source, owners, mcDelta = effective_delta) else if (stack)
        coordination::stackelberg_simulate(source, ownerPost = owners,
          leadersPost = role_args$leadersPost, mcDelta = effective_delta) else
        coordination::core_fringe_simulate(source, ownerPost = owners,
          corePost = role_args$corePost, mcDelta = effective_delta)
      expected_model <- if (methods::is(expected, "AntitrustFit")) expected@model else expected
      expect_equal(unname(result@model@pricePost), unname(expected_model@pricePost), tolerance = 1e-8)
      expect_equal(antitrust::calcShares(result, FALSE, revenue = FALSE),
        antitrust::calcShares(expected_model, FALSE, revenue = FALSE), tolerance = 1e-8, ignore_attr = TRUE)
      accounts <- trade::tariff_accounts(result)
      expect_equal(accounts$physical_cost, unname(promoted@tariff_state$cPre * (1 + physical_delta)), tolerance = 1e-12)
      expect_equal(accounts$producer_profit,
        ((1 - post) * accounts$price - accounts$physical_cost) * accounts$q, tolerance = 1e-10)
      expect_equal(accounts$government_revenue, post * accounts$price * accounts$q, tolerance = 1e-10)
      expect_equal(accounts$producer_profit + accounts$government_revenue,
        (accounts$price - accounts$physical_cost) * accounts$q, tolerance = 1e-10)
      expect_equal(unname(trade::calcProducerSurplus(result, FALSE)), accounts$producer_profit, tolerance = 1e-10)
    }
    expect_identical(serialize(source, NULL), original)
  }
})

test_that("multiproduct followers obey the same tariff cost transformation", {
  for (demand in c("logit", "ces")) for (family in c("BS", "CS")) {
    source <- .tariff_matrix_source(demand, family, roles = "B")
    original <- serialize(source, NULL)
    pre <- c(.1, .1, .2, .05)
    promoted <- trade::as_trade_fit(source, tariffPre = pre,
      cost_basis = "effective", margin_basis = "net_revenue")
    result <- trade::simulate(promoted, tariffPost = .2, mcDelta = .1)
    expected <- coordination::stackelberg_simulate(source,
      mcDelta = 1.1 * (1 - pre) / .8 - 1)
    expect_equal(result@model@pricePost, expected@pricePost, tolerance = 1e-8)
    expect_identical(result@model@slopes, source@slopes)
    expect_identical(result@tariff_state$leadersPost, "B")
    expect_identical(serialize(source, NULL), original)
  }
})

test_that("untaxed products respond across all fourteen fitted output games", {
  trade_skip_unless_tier("extended")
  for (demand in c("logit", "ces")) for (family in c("B", "C", "MC", "BF", "CF", "BS", "CS")) {
    source <- .tariff_matrix_source(demand, family)
    fit <- suppressWarnings(trade::as_trade_fit(source, tariffPre = 0,
      cost_basis = "effective", margin_basis = "net_revenue"))
    before <- trade::tariff_accounts(fit, TRUE)
    result <- suppressWarnings(trade::simulate(fit, tariffPost = c(0, .1, .3, .4)))
    after <- trade::tariff_accounts(result, FALSE)
    info <- paste(demand, family)
    expect_equal(after$tau[1], 0, info = info)
    expect_true(abs(after$q[1] - before$q[1]) > 1e-7, info = info)
    expect_equal(after$physical_cost, before$physical_cost, tolerance = 1e-10, info = info)
    expect_equal(as.numeric(antitrust::calcMC(result@model, FALSE)),
                 after$effective_cost, tolerance = 1e-8, info = info)
  }
})

test_that("promotion-only ALM variants retain untaxed domestic responses", {
  for (demand in c("logit", "ces")) for (conduct in c("bertrand", "cournot")) {
    if (demand == "logit" && conduct == "cournot") next # Direct adapter tested separately.
    source <- suppressWarnings(antitrust::calibrate(demand, conduct, variant = "alm",
      prices = c(2, 2.2, 2.4, 2.6), shares = c(.4, .3, .2, .1),
      margins = rep(.3, 4), ownerPre = c("A", "A", "B", "C"),
      mktElast = -2, insideSize = 100))
    fit <- suppressWarnings(trade::as_trade_fit(source,
      cost_basis = "effective", margin_basis = "net_revenue"))
    result <- suppressWarnings(trade::simulate(fit, tariffPost = c(0, 0, .2, .3)))
    before <- trade::tariff_accounts(fit, TRUE)
    after <- trade::tariff_accounts(result, FALSE)
    expect_true(abs(after$q[1] - before$q[1]) > 1e-7,
                info = paste(demand, conduct, "ALM"))
    expect_equal(after$physical_cost, before$physical_cost, tolerance = 1e-10)
    expect_equal(as.numeric(antitrust::getRetention(result@model, FALSE)), c(1, 1, .8, .7))
  }
})

test_that("repeated policies and scenario changes never compound tariff-adjusted costs", {
  source <- .tariff_matrix_source("logit", "BS")
  promoted <- trade::as_trade_fit(source, tariffPre = .1,
    cost_basis = "effective", margin_basis = "net_revenue")
  first <- trade::simulate(promoted, tariffPost = .2, mcDelta = .1)
  retained <- trade::simulate(first)
  repeated <- trade::simulate(first, tariffPost = .2, mcDelta = .1)
  reset <- trade::simulate(first, tariffPost = .2, mcDelta = 0)
  independent <- trade::simulate(promoted, tariffPost = .2, mcDelta = 0)
  expect_equal(repeated@tariff_state$cPost, first@tariff_state$cPost, tolerance = 1e-12)
  expect_identical(retained@tariff_state$cPost, first@tariff_state$cPost)
  expect_identical(retained@model@pricePost, first@model@pricePost)
  expect_equal(repeated@model@pricePost, first@model@pricePost, tolerance = 1e-8)
  expect_equal(reset@tariff_state$cPost, promoted@tariff_state$cPre, tolerance = 1e-12)
  expect_equal(reset@model@pricePost, independent@model@pricePost, tolerance = 1e-8)
})

test_that("tariff declarations cannot override contradictory source cost provenance", {
  for (family in c("B", "BS")) {
    source <- .tariff_matrix_source("logit", family)
    source@diagnostics$cost_basis <- "physical"
    zero <- trade::as_trade_fit(source, tariffPre = 0,
      cost_basis = "effective", margin_basis = "net_revenue")
    expect_s4_class(zero, "TariffGameFit")
    expect_error(trade::as_trade_fit(source, tariffPre = .1,
      cost_basis = "effective", margin_basis = "net_revenue"),
      class = "trade_tariff_incompatible_basis")
    source@diagnostics$cost_basis <- NULL
    source@diagnostics$cost_basis_notes <- "not a declaration of cost units"
    compatible <- trade::as_trade_fit(source, tariffPre = .1,
      cost_basis = "effective", margin_basis = "net_revenue")
    expect_s4_class(compatible, "TariffGameFit")
  }
  source <- .tariff_matrix_source("logit", "B")
  source@diagnostics$margin_basis <- "net_revenue"
  source@model <- .tariff_matrix_source("logit", "BS")
  expect_error(trade::as_trade_fit(source, tariffPre = .1,
    cost_basis = "effective", margin_basis = "net_revenue"),
    class = "trade_tariff_unsupported_model")
})

test_that("heterogeneous merged-firm tariffs are solved and draw attributes persist", {
  source <- .tariff_matrix_source("ces", "CS")
  attr(source, "bayes_policy_draw") <- list(draw_index = 2L, k = 2L,
    quality = c(.1, -.1, 0), plan_fingerprint = "fixture")
  promoted <- trade::as_trade_fit(source, tariffPre = c(.1, .1, .2, .05),
    cost_basis = "effective", margin_basis = "net_revenue")
  expect_identical(attr(promoted, "bayes_policy_draw"), attr(source, "bayes_policy_draw"))
  mixed <- trade::simulate(promoted, ownerPost = c("A", "A", "A", "C"),
    leadersPost = "A")
  expect_true(all(is.finite(mixed@model@pricePost)))
  expect_equal(as.numeric(antitrust::getRetention(mixed@model, FALSE)),
               1 - c(.1, .1, .2, .05))
  result <- trade::simulate(promoted, tariffPost = .15,
    ownerPost = c("A", "A", "A", "C"), leadersPost = "A")
  expect_identical(attr(result, "bayes_policy_draw"), attr(source, "bayes_policy_draw"))
  expect_identical(attr(result@model, "bayes_policy_draw"), attr(source, "bayes_policy_draw"))
})


test_that("promotion and unchanged policy invoke no construction or equilibrium API", {
  sources <- list(.tariff_matrix_source("logit", "B"),
                  .tariff_matrix_source("ces", "BS"),
                  .tariff_matrix_source("logit", "CF"))
  forbidden <- function(...) stop("unexpected construction/equilibrium call")
  testthat::local_mocked_bindings(specify = forbidden, calibrate = forbidden,
    simulate = forbidden, calcPrices = forbidden, calcSlopes = forbidden,
    .package = "antitrust")
  testthat::local_mocked_bindings(stackelberg_simulate = forbidden,
    core_fringe_simulate = forbidden, .package = "coordination")
  for (source in sources) {
    promoted <- trade::as_trade_fit(source, tariffPre = .1,
      cost_basis = "effective", margin_basis = "net_revenue")
    unchanged <- trade::simulate(promoted)
    expect_equal(unname(unchanged@model@pricePost), unname(promoted@model@pricePre))
    expect_equal(unchanged@tariff_state$promotion_solves, 0L)
  }
})


test_that("net margin and physical unit-markup reports use explicit cost bases", {
  for (demand in c("logit", "ces")) {
    promoted <- trade::as_trade_fit(.tariff_matrix_source(demand, "B"),
      tariffPre = .1, cost_basis = "effective", margin_basis = "net_revenue")
    result <- trade::simulate(promoted, tariffPost = .2)
    accounts <- trade::tariff_accounts(result)
    expect_equal(unname(trade::calcMargins(result, FALSE, level = FALSE)), accounts$net_margin, tolerance = 1e-10)
    expect_equal(unname(trade::calcMargins(result, FALSE, level = TRUE)), accounts$physical_unit_profit, tolerance = 1e-10)
    expect_equal(unname(trade::calcRevenues(result, FALSE)), accounts$gross_revenue, tolerance = 1e-10)
    expect_equal(accounts$effective_unit_markup, accounts$price - accounts$effective_cost, tolerance = 1e-10)
  }
})

test_that("inactive CES products have zero monetary accounts and native missing prices", {
  source <- .tariff_matrix_source("ces", "CS")
  promoted <- trade::as_trade_fit(source, tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue")
  # Product2 exits, and its different tariff must not constrain activefirmA.
  result <- trade::simulate(promoted, tariffPost = c(.15, .3, .15, .15),
    subset = c(TRUE, FALSE, TRUE, TRUE))
  accounts <- trade::tariff_accounts(result)
  expect_true(is.na(accounts$price[[2L]]))
  expect_equal(accounts$q[[2L]], 0)
  expect_equal(accounts$gross_revenue[[2L]], 0)
  expect_equal(accounts$producer_profit[[2L]], 0)
  expect_equal(accounts$government_revenue[[2L]], 0)
  expect_true(all(is.finite(trade::calcProducerSurplus(result, FALSE))))
  expect_true(is.finite(trade::tariff_welfare(result)$physical_plus_government_delta))
})

test_that("ordinary games reject irrelevant roles and fractional ownership", {
  source <- .tariff_matrix_source("logit", "B")
  promoted <- trade::as_trade_fit(source, tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue")
  expect_error(trade::simulate(promoted, leadersPost = "A"), class = "trade_tariff_invalid_roles")
  expect_error(trade::simulate(promoted, corePost = character()), class = "trade_tariff_invalid_roles")
  source@model@ownerPre[1L, 3L] <- .25
  expect_error(trade::as_trade_fit(source, tariffPre = 0,
    cost_basis = "effective", margin_basis = "net_revenue"), class = "trade_tariff_unsupported_ownership")
})


test_that("policy contracts reject unnamed or duplicate declarations", {
  source <- .tariff_matrix_source("logit", "BS")
  expect_error(trade::as_trade_fit(source, policy = "tariff", .1), class = "trade_tariff_invalid_contract")
  expect_error(trade::as_trade_fit(source, cost_basis = "effective", cost_basis = "physical"),
               class = "trade_tariff_invalid_contract")
})

test_that("named one-product cost shocks and named tariffs preserve product alignment", {
  source <- .tariff_matrix_source("logit", "BS")
  tau <- c(product_4 = .05, product_3 = .2, product_2 = .1, product_1 = .1)
  promoted <- trade::as_trade_fit(source, tariffPre = tau,
    cost_basis = "effective", margin_basis = "net_revenue")
  expect_equal(unname(promoted@tariff_state$tariffPre), c(.1, .1, .2, .05))
  result <- trade::simulate(promoted, mcDelta = c(product_1 = .1))
  expect_equal(unname(result@tariff_state$cPost / promoted@tariff_state$cPre), c(1.1, 1, 1, 1))
})

test_that("uniformity is judged on retention even near a full tariff", {
  source <- .tariff_matrix_source("logit", "BS")
  expect_error(trade::as_trade_fit(source,
    tariffPre = c(1 - 1e-12, 1 - 1e-11, .1, .1),
    cost_basis = "effective", margin_basis = "net_revenue"),
    class = "trade_tariff_incompatible_baseline")
})
