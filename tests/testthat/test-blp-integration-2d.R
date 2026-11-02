## Focused trade-wrapper tests for antitrust's two-dimensional BLP state.

trade_blp_2d_points <- matrix(c(
  -1, -1,  0, -1,  1, -1,
  -1,  0,  0,  0,  1,  0,
  -1,  1,  0,  1,  1,  1
), ncol = 2L, byrow = TRUE)
trade_blp_2d_weights <- c(1, 2, 1, 2, 4, 2, 1, 2, 1)


test_that("trade forwards supplied two-dimensional BLP integration state", {
  points <- trade_blp_2d_points
  weights <- trade_blp_2d_weights
  fit <- suppressMessages(suppressWarnings(specify(
    demand = "blp", conduct = "bertrand",
    prices = c(1.5, 1.8, 2.1), shares = c(.30, .25, .15),
    owner = c("A", "B", "C"), insideSize = 100,
    parameters = list(
      alpha = -1.5, sigma = .2, meanval = c(.5, .2, -.1),
      piDemog = .3, demogMean = .2,
      demogCov = matrix(.64, 1L, 1L)
    ), integrationPoints = points, integrationWeights = weights
  )))

  model <- fit@model
  expect_identical(model@slopes$integration, "provided")
  expect_identical(model@slopes$integrationPoints, points)
  expect_equal(model@slopes$drawWeights, weights / sum(weights), tolerance = 0)
  expect_identical(model@slopes$factorOrder, c("price", "demog1"))
  expect_identical(model@slopes$nodesPerAxis, NULL)
  expect_equal(model@nDraws, nrow(points), tolerance = 0)
})


test_that("trade BLP calibration rejects multidimensional integration", {
  expect_error(
    calibrate(
      demand = "blp", conduct = "bertrand",
      prices = c(2, 2.3, 2.7), shares = c(.3, .25, .15),
      margins = c(.4, .35, .3), owner = 1:3, s0 = .3,
      integrationPoints = trade_blp_2d_points
    ),
    "estimates only price random-coefficient heterogeneity"
  )
})


test_that("trade BLP counterfactuals retain exact supplied integration state", {
  fit <- suppressMessages(suppressWarnings(specify(
    demand = "blp", conduct = "bertrand",
    prices = c(1.5, 1.8, 2.1), shares = c(.30, .25, .15),
    owner = c("A", "B", "C"), insideSize = 100,
    parameters = list(
      alpha = -1.5, sigma = .2, meanval = c(.5, .2, -.1),
      piDemog = .3, demogMean = .2,
      demogCov = matrix(.64, 1L, 1L)
    ), integrationPoints = trade_blp_2d_points,
    integrationWeights = trade_blp_2d_weights
  )))
  points <- fit@model@slopes$integrationPoints
  weights <- fit@model@slopes$drawWeights

  set.seed(20260910)
  before <- .Random.seed
  result <- suppressWarnings(simulate(
    fit, antitrust::counterfactual(tariff = rep(0, 3L))
  ))
  after <- .Random.seed

  expect_identical(after, before)
  expect_identical(result@slopes$integrationPoints, points)
  expect_identical(result@slopes$drawWeights, weights)
})
