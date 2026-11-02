linear_cournot_solution <- function(intercept, slope, marginal_cost_slope,
                                    retention) {
  weight <- retention / (marginal_cost_slope - retention * slope)
  total_quantity <- intercept * sum(weight) / (1 - slope * sum(weight))
  price <- intercept + slope * total_quantity
  quantity <- weight * price
  list(quantity = quantity, total = total_quantity, price = price)
}

linear_mc <- function(slope) {
  force(slope)
  function(q) slope * sum(q, na.rm = TRUE)
}

linear_vc <- function(slope) {
  force(slope)
  function(q) slope * sum(q, na.rm = TRUE)^2 / 2
}

constant_mc <- function(cost) {
  force(cost)
  function(q) cost
}

constant_vc <- function(cost) {
  force(cost)
  function(q) cost * sum(q, na.rm = TRUE)
}

test_that("linear Cournot tariffs use retention and solve the analytic equilibrium", {
  intercept <- 10
  slope <- -1
  cost_slope <- c(1, 2)
  retention_pre <- c(.8, .8)
  retention_post <- c(.2, .8)
  baseline <- linear_cournot_solution(
    intercept, slope, cost_slope, retention_pre
  )
  baseline_mc <- cost_slope * baseline$quantity
  margins <- 1 - baseline_mc / (retention_pre * baseline$price)

  result <- suppressWarnings(cournot_tariff(
    prices = baseline$price,
    quantities = matrix(baseline$quantity, ncol = 1,
                        dimnames = list(c("foreign", "domestic"), "good")),
    margins = matrix(margins, ncol = 1),
    demand = "linear",
    cost = rep("linear", 2),
    owner = diag(2),
    tariffPre = matrix(1 - retention_pre, ncol = 1),
    tariffPost = matrix(1 - retention_post, ncol = 1),
    mcfunPre = lapply(cost_slope, linear_mc),
    mcfunPost = lapply(cost_slope, linear_mc),
    vcfunPre = lapply(cost_slope, linear_vc),
    vcfunPost = lapply(cost_slope, linear_vc)
  ))

  expected_post <- linear_cournot_solution(
    intercept, slope, cost_slope, retention_post
  )
  expect_equal(unname(result@quantityPre[, 1]), baseline$quantity,
               tolerance = 1e-6)
  expect_equal(unname(result@quantityPost[, 1]), expected_post$quantity,
               tolerance = 1e-6)
  expect_equal(unname(result@pricePre), baseline$price, tolerance = 1e-6)
  expect_equal(unname(result@pricePost), expected_post$price, tolerance = 1e-6)
  expect_gt(result@quantityPost["domestic", 1],
            result@quantityPre["domestic", 1])
  expect_lt(result@quantityPost["foreign", 1],
            result@quantityPre["foreign", 1])

  post_gradient <- retention_post * result@pricePost +
    slope * retention_post * result@quantityPost[, 1] -
    cost_slope * result@quantityPost[, 1]
  expect_equal(unname(post_gradient), c(0, 0), tolerance = 1e-6)
})

test_that("log-linear Cournot tariffs satisfy the plant-level FOCs", {
  beta <- -.5
  baseline_quantity <- c(40, 60)
  baseline_total <- sum(baseline_quantity)
  baseline_price <- 5
  retention_pre <- c(.8, .8)
  retention_post <- c(.7, .8)
  cost <- retention_pre * baseline_price *
    (1 + beta * baseline_quantity / baseline_total)
  margins <- 1 - cost / (retention_pre * baseline_price)

  result <- suppressWarnings(cournot_tariff(
    prices = baseline_price,
    quantities = matrix(baseline_quantity, ncol = 1,
                        dimnames = list(c("foreign", "domestic"), "good")),
    margins = matrix(margins, ncol = 1),
    demand = "log",
    cost = rep("linear", 2),
    owner = diag(2),
    tariffPre = matrix(1 - retention_pre, ncol = 1),
    tariffPost = matrix(1 - retention_post, ncol = 1),
    mcfunPre = lapply(cost, constant_mc),
    mcfunPost = lapply(cost, constant_mc),
    vcfunPre = lapply(cost, constant_vc),
    vcfunPost = lapply(cost, constant_vc)
  ))

  quantity <- result@quantityPost[, 1]
  total <- sum(quantity)
  price <- exp(result@intercepts) * total^result@slopes
  partial <- exp(result@intercepts) * result@slopes *
    total^(result@slopes - 1)
  gradient <- retention_post * price +
    partial * retention_post * quantity - cost

  expect_true(all(quantity >= 0))
  expect_true(all(quantity > 0))
  expect_equal(unname(gradient), c(0, 0), tolerance = 1e-6)
})

test_that("finite Cournot plant capacities enter the KKT conditions", {
  intercept <- 10
  slope <- -1
  cost_slope <- c(1, 2)
  baseline <- linear_cournot_solution(
    intercept, slope, cost_slope, c(1, 1)
  )
  margins <- 1 - cost_slope * baseline$quantity / baseline$price

  result <- suppressWarnings(cournot_tariff(
    prices = baseline$price,
    quantities = matrix(baseline$quantity, ncol = 1,
                        dimnames = list(c("plant1", "plant2"), "good")),
    margins = matrix(margins, ncol = 1),
    demand = "linear",
    cost = rep("linear", 2),
    owner = diag(2),
    capacitiesPre = c(Inf, Inf),
    capacitiesPost = c(2, Inf),
    mcfunPre = lapply(cost_slope, linear_mc),
    mcfunPost = lapply(cost_slope, linear_mc),
    vcfunPre = lapply(cost_slope, linear_vc),
    vcfunPost = lapply(cost_slope, linear_vc)
  ))

  quantity <- result@quantityPost[, 1]
  gradient <- result@pricePost - quantity - cost_slope * quantity
  expect_equal(unname(quantity), c(2, 2), tolerance = 1e-6)
  expect_equal(unname(result@pricePost), 6, tolerance = 1e-6)
  expect_equal(unname(sum(quantity)), 4, tolerance = 1e-6)
  expect_gt(gradient["plant1"], 0)
  expect_equal(unname(gradient["plant2"]), 0, tolerance = 1e-6)
})

test_that("a high tariff permits a zero-output Cournot corner", {
  baseline_quantity <- c(4, 1)
  baseline_price <- 5
  retention_pre <- c(.8, .8)
  retention_post <- c(.8, .1)
  cost <- c(.8, 3.2)
  margins <- 1 - cost / (retention_pre * baseline_price)

  result <- suppressWarnings(cournot_tariff(
    prices = baseline_price,
    quantities = matrix(baseline_quantity, ncol = 1,
                        dimnames = list(c("foreign", "domestic"), "good")),
    margins = matrix(margins, ncol = 1),
    demand = "linear",
    cost = rep("linear", 2),
    owner = diag(2),
    tariffPre = matrix(1 - retention_pre, ncol = 1),
    tariffPost = matrix(1 - retention_post, ncol = 1),
    mcfunPre = lapply(cost, constant_mc),
    mcfunPost = lapply(cost, constant_mc),
    vcfunPre = lapply(cost, constant_vc),
    vcfunPost = lapply(cost, constant_vc)
  ))

  quantity <- result@quantityPost[, 1]
  price <- result@pricePost
  expect_true(all(quantity >= 0))
  expect_equal(unname(quantity["domestic"]), 0, tolerance = 1e-8)
  expect_gt(quantity["foreign"], result@quantityPre["foreign", 1])

  gradient <- retention_post * price - cost -
    retention_post * quantity
  expect_equal(unname(gradient["foreign"]), 0, tolerance = 1e-6)
  expect_lte(gradient["domestic"], 1e-7)
  expect_equal(as.numeric(calcMC(result, FALSE)), cost, tolerance = 1e-12)
})

test_that("multiproduct Cournot respects demand vectors and physical cost shocks", {
  intercept <- c(10, 12)
  cost <- c(1, 2)
  retention_pre <- matrix(c(.8, .9), 2, 2)
  solve_market <- function(retention, costs) {
    kappa <- costs / retention
    rbind((intercept - 2 * kappa[1, ] + kappa[2, ]) / 3,
          (intercept - 2 * kappa[2, ] + kappa[1, ]) / 3)
  }
  qpre <- solve_market(retention_pre, cost)
  ppre <- intercept - colSums(qpre)
  margins <- 1 - cost / (retention_pre * rep(ppre, each = 2))
  retention_post <- matrix(c(.8, .7, .5, .9), 2, 2)
  result <- suppressWarnings(cournot_tariff(
    prices = ppre, quantities = qpre, margins = margins,
    owner = diag(2), demand = rep("linear", 2),
    tariffPre = 1 - retention_pre, tariffPost = 1 - retention_post,
    mcfunPre = lapply(cost, constant_mc), vcfunPre = lapply(cost, constant_vc)))
  expect_equal(unname(result@quantityPost), solve_market(retention_post, cost),
               tolerance = 2e-5)
  result@mcDelta <- c(.1, .2)
  result@quantityPost <- calcQuantities(result, FALSE)
  expect_equal(unname(result@quantityPost),
               solve_market(retention_post, cost * c(1.1, 1.2)), tolerance = 2e-5)
  expect_equal(as.numeric(calcMC(result, FALSE)), cost * c(1.1, 1.2))
})
