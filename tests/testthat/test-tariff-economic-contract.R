.retained_test_fit <- function(demand = "logit", conduct = "bertrand",
                               retention = NULL) {
  args <- list(demand = demand, conduct = conduct,
    prices = c(2, 2.3, 2.6), ownerPre = c("A", "A", "B"),
    insideSize = 100, labels = c("domestic", "import_A", "import_B"),
    revenueRetentionPre = retention)
  if (demand == "blp") {
    args$shares <- c(.25, .2, .15)
    args$parameters <- list(alphaMean = -1.5, sigma = .3)
    args$draws <- c(-1.5, -.5, .5, 1.5)
    args$integrationWeights <- c(.1, .2, .3, .4)
  } else {
    args$parameters <- list(alpha = -1.5, meanval = c(2.5, 2.3, 2.1))
  }
  if (conduct == "bargaining") args$bargpowerPre <- rep(.4, 3)
  suppressWarnings(do.call(antitrust::specify, args))
}

test_that("mixed-tariff Bertrand and Cournot satisfy independent physical-profit FOCs", {
  for (demand in c("logit", "blp")) for (conduct in c("bertrand", "cournot")) {
    source <- .retained_test_fit(demand, conduct)
    fit <- suppressWarnings(trade::as_trade_fit(source, tariffPre = 0))
    tau <- c(0, .3, .5)
    model <- suppressWarnings(trade::simulate(fit, tariffPost = tau))
    if (methods::is(model, "TradeFit")) model <- model@model
    p <- model@pricePost; retention <- 1 - tau
    physical_cost <- source@model@mcPre
    own <- source@model@ownerPre
    shares <- function(price) {
      obj <- model; obj@pricePost <- price
      as.numeric(antitrust::calcShares(obj, FALSE, revenue = FALSE))
    }
    q <- shares(p)
    if (conduct == "bertrand") {
      residual <- vapply(seq_along(p), function(i) {
        profit <- function(x) {
          pp <- p; pp[i] <- x
          sum(own[i, ] * (retention * pp - physical_cost) * shares(pp))
        }
        numDeriv::grad(profit, p[i])
      }, numeric(1))
    } else {
      inverse_jacobian <- solve(numDeriv::jacobian(shares, p))
      residual <- retention * p - physical_cost +
        rowSums(own * t(inverse_jacobian) *
                  matrix(retention * q, nrow = length(p), ncol = length(p), byrow = TRUE))
    }
    info <- paste(demand, conduct)
    expect_true(max(abs(residual)) < 1e-6, info = info)
    expect_equal(unname(model@ownerPost), unname(own), info = info)
    expect_equal(as.numeric(antitrust::getRetention(model, FALSE)), retention)
    expect_equal(as.numeric(model@mcPost) * retention, as.numeric(physical_cost),
                 tolerance = 1e-8)
    expect_gt(abs(q[1] - antitrust::calcShares(source@model, TRUE)[1]), 1e-6)
  }
})

test_that("heterogeneous baseline promotion preserves only a compatible fitted game", {
  tau <- c(0, .3, .5)
  source <- .retained_test_fit()
  expect_error(trade::as_trade_fit(source, tariffPre = tau),
               class = "trade_tariff_incompatible_baseline")
  source <- .retained_test_fit(retention = 1 - tau)
  before <- serialize(source, NULL)
  fit <- suppressWarnings(trade::as_trade_fit(source, tariffPre = tau))
  expect_equal(fit@model@pricePre, source@model@pricePre)
  result <- suppressWarnings(trade::simulate(fit, tariffPost = tau))
  expect_equal(as.numeric(result@pricePost), as.numeric(source@model@pricePre),
               tolerance = 1e-7)
  expect_identical(serialize(source, NULL), before)
})

test_that("foreign tariffs approach the domestic-only Bertrand equilibrium", {
  source <- suppressWarnings(antitrust::specify("logit", "bertrand",
    prices = c(2, 2), ownerPre = c("domestic", "foreign"), insideSize = 100,
    parameters = list(alpha = -1.5, meanval = c(5, 5))))
  fit <- suppressWarnings(trade::as_trade_fit(source))
  moderate <- suppressWarnings(trade::simulate(fit, tariffPost = c(0, .5)))
  high <- suppressWarnings(trade::simulate(fit, tariffPost = c(0, .95)))
  domestic_only <- suppressWarnings(antitrust::simulate(source,
    ownerPost = c("domestic", "foreign"), subset = c(TRUE, FALSE)))
  expect_gt(moderate@pricePost[1], source@model@pricePre[1])
  expect_gt(high@pricePost[1], moderate@pricePost[1])
  expect_equal(as.numeric(high@pricePost[1]), as.numeric(domestic_only@pricePost[1]),
               tolerance = 1e-5)
  expect_lt(antitrust::calcShares(high, FALSE)[2], 1e-8)
})

test_that("tariff validation rejects undefined retentions and permits subsidies", {
  for (value in list(1, 1.2, Inf, -Inf, NaN, "0.2")) {
    expect_error(trade:::.normalize_tariff(rep(value, 3), 3, "tariffPost"))
  }
  expect_equal(trade:::.normalize_tariff(c(NA, -.2, .3), 3, "tariffPost"),
               c(0, -.2, .3))
  expect_error(trade:::.normalize_tariff_matrix(matrix(1, 2, 1), c(2, 1), "tariffPost"))
  model <- .retained_test_fit()@model
  model <- antitrust::setRetention(model, retentionPre = rep(1e-308, 3))
  expect_silent(trade:::.trade_check_baseline_retention(model, rep(1e308, 3)))
})

test_that("auction promotion rejects mixed portfolios even with matching retention metadata", {
  for (demand in c("logit", "blp")) {
    source <- .retained_test_fit(demand, "auction2nd")
    source <- antitrust::setRetention(source, retentionPre = c(.8, .7, .6),
                                     retentionPost = c(.8, .7, .6))
    expect_error(trade::as_trade_fit(source, tariffPre = c(.2, .3, .4)),
                 class = "trade_tariff_heterogeneous")
    if (demand == "logit") {
      expect_error(trade::as_trade_fit(source, tariffPre = c(.2, .3, .4),
        cost_basis = "effective", margin_basis = "net_revenue"),
        class = "trade_tariff_heterogeneous")
    }
  }
})

test_that("native auction wrappers reject mixed near-prohibitive tariffs", {
  for (demand in c("logit", "blp")) {
    fit <- suppressWarnings(trade::as_trade_fit(.retained_test_fit(demand, "auction2nd")))
    expect_error(trade::simulate(fit, tariffPost = c(1 - 1e-12, 1 - 1e-11, .2)),
                 "mixed revenue retention")
    exited <- suppressWarnings(trade::simulate(fit, tariffPost = c(.1, .2, .3),
      subset = c(TRUE, FALSE, TRUE)))
    inactive_share <- as.numeric(antitrust::calcShares(exited, FALSE)[2])
    expect_true(is.na(inactive_share) || inactive_share == 0)
    expect_true(all(is.finite(exited@pricePost[c(1, 3)])))
  }
})
