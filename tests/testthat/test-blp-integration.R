## Trade must pass the antitrust BLP integration controls through unchanged.
## The fixture is generated from the antitrust price-random-coefficient model,
## so the comparison tests the wrapper rather than a second trade estimator.

.trade_blp_fixture <- function(nodes, weights) {
  prices <- c(1.5, 2, 2.6)
  owner <- c("A", "A", "B")
  alpha <- -5
  sigma <- .8
  base_delta <- c(.7, .35, .05)
  target_s0 <- .2
  aggregate_share <- function(delta) {
    antitrust:::.blp_stable_shares(
      delta, prices, alpha + sigma * nodes,
      nodes, weights, outside = TRUE
    )$aggregate
  }
  shift <- uniroot(
    function(value) sum(aggregate_share(base_delta + value)) -
      (1 - target_s0),
    c(-10, 10), tol = 1e-12
  )$root
  delta <- base_delta + shift
  shares <- aggregate_share(delta)
  model <- suppressWarnings(antitrust:::.blp_model(
    conduct = "bertrand", prices = prices, shares = shares,
    margins = rep(.2, length(prices)), ownerPre = owner,
    alphaMean = alpha, sigma = sigma, meanval = delta,
    draws = nodes, drawWeights = weights, s0 = 1 - sum(shares),
    output = TRUE
  ))
  draw_shares <- antitrust::calcShares(model, aggregate = FALSE)
  alphas <- model@slopes$alphas
  derivative <- matrix(0, nrow = 3, ncol = 3)
  for (r in seq_along(alphas)) {
    s <- draw_shares[, r]
    derivative <- derivative + weights[r] * alphas[r] *
      (diag(s) - tcrossprod(s))
  }
  elasticity <- derivative * outer(1 / shares, prices)
  revenue <- prices * shares / sum(prices * shares)
  margins <- -as.vector(
    solve(t(elasticity) * model@ownerPre) %*%
      (revenue * diag(model@ownerPre))
  ) / revenue
  list(
    prices = prices, shares = shares, margins = margins,
    owner = owner, s0 = 1 - sum(shares), nodes = nodes,
    weights = weights, alpha = alpha, sigma = sigma, delta = delta
  )
}

test_that("trade BLP preserves provided integration points and weights", {
  fixture <- .trade_blp_fixture(
    nodes = c(-1.5, -.5, .5, 1.5),
    weights = c(.10, .20, .30, .40)
  )
  trade_fit <- calibrate(
    demand = "blp", conduct = "bertrand",
    prices = fixture$prices, shares = fixture$shares,
    margins = fixture$margins, owner = fixture$owner, s0 = fixture$s0,
    output = TRUE, integration = "provided", draws = fixture$nodes,
    integrationWeights = fixture$weights,
    optimizer_control = list(maxit = 150, factr = 1e3, pgtol = 1e-8)
  )
  antitrust_fit <- antitrust::calibrate(
    demand = "blp", conduct = "bertrand",
    prices = fixture$prices, shares = fixture$shares,
    margins = fixture$margins, ownerPre = fixture$owner, s0 = fixture$s0,
    output = TRUE, integration = "provided", draws = fixture$nodes,
    integrationWeights = fixture$weights,
    optimizer_control = list(maxit = 150, factr = 1e3, pgtol = 1e-8)
  )

  expect_equal(trade_fit@parameters$alphaMean,
               antitrust_fit@parameters$alphaMean, tolerance = 1e-10)
  expect_equal(trade_fit@parameters$sigma,
               antitrust_fit@parameters$sigma, tolerance = 1e-10)
  expect_equal(unname(calcShares(trade_fit@model, TRUE)), fixture$shares,
               tolerance = 2e-10)
  expect_equal(trade_fit@diagnostics$integration$weights,
               fixture$weights / sum(fixture$weights), tolerance = 0)
  expect_equal(trade_fit@diagnostics$integration$rule, "provided")
})

test_that("trade BLP no-demographics calibration works under GH and Monte Carlo", {
  for (rule in c("gauss-hermite", "monte-carlo")) {
    if (identical(rule, "gauss-hermite")) {
      quadrature <- antitrust:::.blp_normal_nodes(15L)
      fixture <- .trade_blp_fixture(quadrature$nodes, quadrature$weights)
      integration_args <- list(integration = rule, nNodes = 15L)
    } else {
      set.seed(20260906)
      nodes <- rnorm(15L)
      fixture <- .trade_blp_fixture(nodes, rep(1 / 15, 15))
      set.seed(20260906)
      integration_args <- list(integration = rule, nDraws = 15L)
    }
    fit <- do.call(calibrate, c(list(
      demand = "blp", conduct = "bertrand",
      prices = fixture$prices, shares = fixture$shares,
      margins = fixture$margins, owner = fixture$owner, s0 = fixture$s0,
      output = TRUE,
      optimizer_control = list(maxit = 150, factr = 1e3, pgtol = 1e-8)
    ), integration_args))

    expect_equal(fit@parameters$alphaMean, fixture$alpha,
                 tolerance = 2e-3, info = rule)
    expect_equal(fit@parameters$sigma, fixture$sigma,
                 tolerance = 2e-3, info = rule)
    expect_identical(fit@diagnostics$integration$rule, rule)
    expect_length(fit@diagnostics$integration$nodes, 15L)
    expect_equal(unname(calcShares(fit@model, TRUE)), fixture$shares,
                 tolerance = 2e-10, info = rule)
    expect_lt(fit@diagnostics$maxAbsResidual, 2e-5)

    result <- simulate(fit, tariffPost = rep(.1, length(fixture$prices)))
    expect_s4_class(result, "TariffLogitBLP")
    expect_true(any(abs(result@pricePost - fit@model@pricePre) > 1e-8))
  }
})

test_that("trade Monte Carlo BLP defaults to 5000 draws", {
  fixture <- .trade_blp_fixture(
    nodes = c(-1.5, -.5, .5, 1.5),
    weights = c(.10, .20, .30, .40)
  )
  set.seed(20260906)
  fit <- specify(
    demand = "blp", conduct = "bertrand", prices = fixture$prices,
    parameters = list(
      alphaMean = fixture$alpha, sigma = fixture$sigma,
      meanval = fixture$delta
    ), shares = fixture$shares, owner = fixture$owner, s0 = fixture$s0,
    output = TRUE, integration = "monte-carlo"
  )
  expect_identical(fit@diagnostics$integration$rule, "monte-carlo")
  expect_length(fit@diagnostics$integration$nodes, 5000L)
})
