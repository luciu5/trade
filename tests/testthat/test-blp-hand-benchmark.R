test_that("trade BLP supplied integration matches hand-computed demand and FOCs", {
  prices <- c(2, 2.4, 2.8)
  mean_utility <- c(.5, .7, .3)
  alpha_mean <- -1.4
  sigma <- .4
  nodes <- c(-1.5, .25, 1.2)
  weights <- c(.2, .3, .5)
  owner <- c("A", "A", "B")

  # The independent finite mixture has an outside option with utility zero.
  draw_share <- vapply(nodes, function(node) {
    utility <- mean_utility + (alpha_mean + sigma * node) * prices
    exp_utility <- exp(utility)
    exp_utility / (1 + sum(exp_utility))
  }, numeric(length(prices)))
  expected_share <- drop(draw_share %*% weights)
  expected_jacobian <- matrix(0, length(prices), length(prices))
  for (draw in seq_along(nodes)) {
    share <- draw_share[, draw]
    expected_jacobian <- expected_jacobian + weights[draw] *
      (alpha_mean + sigma * nodes[draw]) *
      (diag(share) - tcrossprod(share))
  }
  expected_elasticity <- expected_jacobian *
    outer(1 / expected_share, prices)

  fit <- suppressWarnings(specify(
    demand = "blp", conduct = "bertrand", prices = prices,
    parameters = list(alphaMean = alpha_mean, sigma = sigma,
                      meanval = mean_utility),
    shares = expected_share, s0 = 1 - sum(expected_share),
    owner = owner, integration = "provided", draws = nodes,
    integrationWeights = weights, output = TRUE
  ))

  expect_equal(unname(calcShares(fit@model, TRUE)), expected_share,
               tolerance = 1e-10)
  expect_equal(unname(elast(fit@model, TRUE)), expected_elasticity,
               tolerance = 1e-10)
  expect_equal(fit@diagnostics$integration$weights, weights,
               tolerance = 0)

  # Bertrand FOCs use each owner's full product portfolio. The fixture
  # includes a two-product firm so a diagonal-ownership error is detectable.
  cost <- unname(calcMC(fit@model, TRUE))
  ownership <- outer(owner, owner, `==`)
  residual <- expected_share +
    as.vector(t(expected_jacobian * ownership) %*% (prices - cost))
  expect_lt(max(abs(residual)), 1e-9)
})
