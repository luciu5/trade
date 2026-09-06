test_that("zero-tariff flat demand agrees with antitrust", {
  prices <- c(.04, .03, .04, .04, .04, .05)
  owner <- c("A", "B", "C", "C", "D", "E")
  cases <- list(
    logit = list(alpha = -25, meanval = c(0, .4, 1.1, .8, .2, 1.4)),
    ces = list(gamma = 1.2, alpha = .2,
               meanval = c(1, 1.2, .8, 1.1, .9, 1.3))
  )

  for (demand in names(cases)) {
    trade_fit <- specify(
      demand = demand, conduct = "bertrand", prices = prices,
      parameters = cases[[demand]], owner = owner,
      tariffPre = rep(0, length(prices))
    )
    antitrust_fit <- antitrust::specify(
      demand = demand, conduct = "bertrand", prices = prices,
      parameters = cases[[demand]], ownerPre = owner
    )

    expect_equal(unname(trade_fit@model@pricePre),
                 unname(antitrust_fit@model@pricePre), tolerance = 1e-12,
                 info = demand)
    expect_equal(unname(calcShares(trade_fit@model, TRUE)),
                 unname(antitrust::calcShares(antitrust_fit@model, TRUE)),
                 tolerance = 1e-12, info = demand)
    expect_equal(trade_fit@model@mcPre,
                 antitrust::calcMC(antitrust_fit@model, TRUE),
                 tolerance = 1e-12, info = demand)
    expect_equal(unname(elast(trade_fit@model, TRUE)),
                 unname(antitrust::elast(antitrust_fit@model, TRUE)),
                 tolerance = 1e-12, info = demand)
  }
})

test_that("respecification keeps nonzero tariff policy state", {
  x <- list(
    prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
    quantities = c(.066, .172, .253, .187, .099, .223) * 100,
    margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
    owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  )
  tariff <- c(0, 0, 0, 0, .1, .1)
  fit <- calibrate(
    demand = "logit", conduct = "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner,
    tariffPre = tariff
  )
  target <- respecify(fit, demand = "ces", gamma = 1.2)

  expect_equal(target@model@tariffPre, tariff, tolerance = 0)
  expect_equal(target@model@tariffPost, tariff, tolerance = 0)
  expect_equal(target@diagnostics$transition$policy$retained, "tariffPre")
  expect_equal(target@diagnostics$transition$policy$recomputed,
               c("tariff-adjusted ownership", "marginal costs",
                 "equilibrium state"))
})
