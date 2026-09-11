trade_fit_data <- function() {
  list(
    prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
    quantities = c(.066, .172, .253, .187, .099, .223) * 100,
    margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
    owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG"),
    tariff = c(0, 0, 0, 0, .1, .1),
    quota = c(Inf, Inf, Inf, Inf, .8, .8)
  )
}

expect_trade_result_parity <- function(actual, expected) {
  expect_equal(class(actual), class(expected))
  expect_equal(actual@pricePre, expected@pricePre, tolerance = 1e-7)
  expect_equal(actual@pricePost, expected@pricePost, tolerance = 1e-7)
  expect_equal(actual@mcPre, expected@mcPre, tolerance = 1e-7)
  expect_equal(actual@mcPost, expected@mcPost, tolerance = 1e-7)
  expect_equal(unname(calcShares(actual, TRUE)), unname(calcShares(expected, TRUE)), tolerance = 1e-7)
  expect_equal(unname(calcShares(actual, FALSE)), unname(calcShares(expected, FALSE)), tolerance = 1e-7)
  expect_equal(unname(calcMargins(actual, TRUE)), unname(calcMargins(expected, TRUE)), tolerance = 1e-7)
  expect_equal(unname(calcMargins(actual, FALSE)), unname(calcMargins(expected, FALSE)), tolerance = 1e-7)
}

test_that("calibrate and simulate separate a Bertrand Logit tariff", {
  x <- trade_fit_data()
  fit <- calibrate(
    demand = "Logit",
    conduct = "Nash-Bertrand",
    prices = x$prices,
    quantities = x$quantities,
    margins = x$margins,
    owner = x$owner
  )
  old <- suppressWarnings(
    bertrand_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )
  new <- simulate(fit, tariffPost = x$tariff)

  expect_s4_class(fit, "TradeFit")
  expect_equal(fit@spec$id, "logit::bertrand")
  expect_equal(fit@diagnostics$route, "calibrate")
  expect_equal(fit@model@tariffPost, rep(0, length(x$prices)))
  expect_trade_result_parity(new, old)
  expect_equal(new@tariffPost, x$tariff)
})

test_that("TariffAIDS lifecycle simulation matches the legacy economic oracle", {
  prices <- rep(10, 4)
  quantities <- c(40, 30, 20, 10)
  margins <- c(.25, .25, NA, NA)
  owner <- c("Firm1", "Firm2", "Firm3", "Firm3")
  tariff_pre <- c(.05, .05, 0, 0)
  tariff_post <- c(.25, .25, 0, 0)

  fit <- calibrate(
    "aids", "bertrand",
    prices = prices,
    quantities = quantities,
    margins = margins,
    owner = owner,
    tariffPre = tariff_pre,
    mktElast = -1
  )
  lifecycle <- simulate(fit, tariffPost = tariff_post)
  legacy <- suppressWarnings(
    bertrand_tariff(
      "aids",
      prices = prices,
      quantities = quantities,
      margins = margins,
      owner = owner,
      tariffPre = tariff_pre,
      tariffPost = tariff_post,
      mktElast = -1
    )
  )

  expect_trade_result_parity(lifecycle, legacy)
  expect_equal(
    unname(lifecycle@priceDelta),
    c(.215410962672, .218026293531, .086440483652, .086440478422),
    tolerance = 1e-7
  )
  expect_equal(
    unname(lifecycle@pricePost),
    c(12.1541096267, 12.1802629353, 10.8644048365, 10.8644047842),
    tolerance = 1e-7
  )
  expect_gt(max(abs(lifecycle@pricePost - lifecycle@pricePre)), .1)

  unchanged <- simulate(fit, tariffPost = tariff_pre)
  expect_equal(unname(unchanged@priceDelta), rep(0, 4), tolerance = 1e-7)
  expect_equal(unchanged@pricePost, unchanged@pricePre, tolerance = 1e-7)

  custom_start <- simulate(
    fit, tariffPost = c(.08, .08, 0, 0), priceStart = rep(0, 4)
  )
  expect_equal(
    unname(custom_start@priceDelta),
    c(.026990687992297, .027282600700729, .011620729322997, .011620729323014),
    tolerance = 1e-7
  )
})

test_that("TariffAIDS repeated and sequential simulations re-solve each target", {
  prices <- rep(10, 4)
  quantities <- c(40, 30, 20, 10)
  margins <- c(.25, .25, NA, NA)
  owner <- c("Firm1", "Firm2", "Firm3", "Firm3")
  fit <- calibrate(
    "aids", "bertrand",
    prices = prices,
    quantities = quantities,
    margins = margins,
    owner = owner,
    tariffPre = c(.05, .05, 0, 0),
    mktElast = -1
  )
  intermediate <- c(.20, .20, 0, 0)
  target <- c(.25, .25, 0, 0)

  first <- simulate(fit, tariffPost = intermediate)
  target_once <- simulate(fit, tariffPost = target)
  first_again <- simulate(fit, tariffPost = intermediate)
  cf <- add_step(counterfactual(tariff = intermediate), tariff = target)
  target_sequential <- result_at(simulate(fit, cf), 2)

  expect_equal(first@priceDelta, first_again@priceDelta, tolerance = 1e-10)
  expect_equal(first@pricePost, first_again@pricePost, tolerance = 1e-10)
  expect_equal(target_sequential@priceDelta, target_once@priceDelta, tolerance = 1e-7)
  expect_equal(target_sequential@pricePost, target_once@pricePost, tolerance = 1e-7)
})

test_that("one calibrated fit supports repeated tariff simulations", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)

  first <- simulate(fit, tariffPost = x$tariff)
  second <- simulate(fit, tariffPost = c(rep(0, 5), .2))
  unchanged <- simulate(fit, tariffPost = rep(0, 6))

  expect_true(second@pricePost[6] > first@pricePost[6])
  expect_equal(unchanged@pricePost, fit@model@pricePre, tolerance = 1e-7)
  expect_equal(fit@model@tariffPost, rep(0, 6))
})

test_that("specify and simulate preserve the supplied-parameter sim path", {
  x <- trade_fit_data()
  parameters <- list(
    alpha = -48.0457,
    meanval = c(0, .4149233, 1.1899885, .8252482, .1460183, 1.4865730)
  )
  fit <- specify(
    model_spec("logit", "bertrand"),
    prices = x$prices,
    parameters = parameters,
    owner = x$owner
  )
  old <- suppressWarnings(
    .sim_legacy(
      prices = x$prices,
      demand = "logit",
      supply = "bertrand",
      demand.param = parameters,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )

  expect_equal(fit@diagnostics$route, "specify")
  expect_trade_result_parity(simulate(fit, tariffPost = x$tariff), old)
})

test_that("migrated sim paths remain numerically compatible", {
  x <- trade_fit_data()
  parameters <- list(
    logit = list(alpha = -48.0457,
      meanval = c(0, .4149233, 1.1899885, .8252482, .1460183, 1.4865730)),
    ces = list(gamma = 2.2, alpha = .2,
      meanval = c(.8, .9, 1.1, 1.2, 1.3, 1.4))
  )

  cases <- list(
    c("logit", "bertrand"),
    c("ces", "bertrand"),
    c("logit", "moncom"),
    c("ces", "moncom"),
    c("logit", "auction2nd"),
    c("logit", "bargaining")
  )

  for (case in cases) {
    demand <- case[[1L]]
    supply <- case[[2L]]
    old <- suppressWarnings(.sim_legacy(
      prices = x$prices,
      supply = supply,
      demand = demand,
      demand.param = parameters[[demand]],
      owner = x$owner,
      tariffPost = x$tariff
    ))
    new <- suppressWarnings(sim(
      prices = x$prices,
      supply = supply,
      demand = demand,
      demand.param = parameters[[demand]],
      owner = x$owner,
      tariffPost = x$tariff
    ))
    expect_trade_result_parity(new, old)
  }
})

test_that("CES uses its complete legacy calibration and simulation path", {
  x <- trade_fit_data()
  fit <- calibrate("ces", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)
  old <- suppressWarnings(
    bertrand_tariff("ces", x$prices, x$quantities, x$margins,
      owner = x$owner, tariffPost = x$tariff)
  )

  expect_trade_result_parity(simulate(fit, tariffPost = x$tariff), old)
})

test_that("trade MonCom CES matches core direct primitives and responds to tariffs", {
  prices <- c(2, 2.5, 3)
  revenue_shares <- c(.30, .25, .25)
  revenue_total <- 100
  quantities <- revenue_total * revenue_shares / prices
  owner <- c("A", "A", "B")
  gamma <- 2
  parameters <- list(
    alpha = 1 / sum(revenue_shares) - 1,
    gamma = gamma,
    meanval = revenue_shares * prices /
      ((1 - sum(revenue_shares)) * 1)
  )

  core <- antitrust::specify(
    "ces", "moncom", prices = prices, parameters = parameters,
    ownerPre = owner, shares = revenue_shares, insideSize = revenue_total,
    priceOutside = 1
  )
  fit <- specify(
    "ces", "moncom", prices = prices, parameters = parameters,
    owner = owner, insideSize = revenue_total, priceOutside = 1
  )

  expect_s4_class(fit@model, "TariffMonComCES")
  expect_equal(unname(fit@parameters$slopes$gamma), gamma, tolerance = 0)
  expect_equal(unname(fit@model@pricePre), unname(core@model@pricePre),
               tolerance = 1e-10)
  expect_equal(unname(calcShares(fit@model, TRUE, revenue = TRUE)),
               unname(calcShares(core@model, TRUE, revenue = TRUE)),
               tolerance = 1e-10)
  expect_equal(unname(calcQuantities(fit@model, TRUE)),
               unname(calcQuantities(core@model, TRUE)), tolerance = 1e-10)
  expect_equal(unname(fit@model@mcPre), unname(core@model@mcPre),
               tolerance = 1e-10)

  ## Independent atomistic CES FOC: q + (p-mc)(-gamma q/p) = 0.
  q_pre <- as.numeric(calcQuantities(fit@model, TRUE))
  p_pre <- as.numeric(fit@model@pricePre)
  mc_pre <- as.numeric(fit@model@mcPre)
  foc_pre <- q_pre + (p_pre - mc_pre) * (-gamma * q_pre / p_pre)
  expect_lt(max(abs(foc_pre)), 1e-10)

  tariff <- c(.10, 0, .20)
  shocked <- simulate(fit, tariffPost = tariff)
  expected_mc <- mc_pre / (1 - tariff)
  expected_price <- p_pre / (1 - tariff)
  expect_equal(unname(shocked@mcPre), unname(mc_pre), tolerance = 0)
  expect_equal(unname(shocked@mcPost), unname(expected_mc),
               tolerance = 1e-10)
  expect_equal(unname(shocked@pricePost), unname(expected_price),
               tolerance = 1e-10)

  q_post <- as.numeric(calcQuantities(shocked, FALSE))
  p_post <- as.numeric(shocked@pricePost)
  mc_post <- as.numeric(shocked@mcPost)
  foc_post <- q_post + (p_post - mc_post) * (-gamma * q_post / p_post)
  expect_lt(max(abs(foc_post)), 1e-10)

  ## MonCom pricing is atomistic; ownership labels cannot change the result.
  other_owner <- specify(
    "ces", "moncom", prices = prices, parameters = parameters,
    owner = c("X", "Y", "Z"), insideSize = revenue_total,
    priceOutside = 1
  )
  expect_equal(unname(other_owner@model@pricePost),
               unname(fit@model@pricePost), tolerance = 1e-10)
})

test_that("trade and core retain distinct noisy MonCom CES calibration objectives", {
  prices <- c(2, 2.5, 3)
  revenue_shares <- c(.30, .25, .25)
  quantities <- 100 * revenue_shares / prices
  margins <- c(.50, .40, .25)

  core <- antitrust::calibrate(
    "ces", "moncom", prices = prices, shares = revenue_shares,
    margins = margins, ownerPre = c("A", "A", "B"), insideSize = 100,
    priceOutside = 1
  )
  trade_fit <- calibrate(
    "ces", "moncom", prices = prices, quantities = quantities,
    margins = margins, priceOutside = 1
  )

  ## Core estimates the weighted mean of inverse margins; the mature trade
  ## calibrator minimizes squared margin distance and therefore estimates the
  ## inverse of the mean margin. These are distinct objectives under noise.
  expect_equal(unname(core@parameters$gamma), mean(1 / margins),
               tolerance = 1e-10)
  expect_equal(unname(trade_fit@parameters$slopes$gamma), 1 / mean(margins),
               tolerance = 1e-4)
  expect_true(abs(core@parameters$gamma - trade_fit@parameters$slopes$gamma) >
              1e-3)
})

test_that("quota policy is a separate complete model path", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", policy = "quota",
    prices = x$prices, quantities = x$quantities, margins = x$margins,
    owner = x$owner)
  old <- suppressWarnings(
    bertrand_quota("logit", x$prices, x$quantities, x$margins,
      owner = x$owner, quotaPost = x$quota)
  )

  new <- simulate(fit, quotaPost = x$quota)
  expect_equal(fit@spec$id, "logit::bertrand::quota")
  expect_trade_result_parity(new, old)
})

test_that("post-policy inputs are kept at the simulation boundary", {
  x <- trade_fit_data()
  expect_error(
    calibrate("logit", "bertrand", prices = x$prices,
      quantities = x$quantities, margins = x$margins, owner = x$owner,
      tariffPost = x$tariff),
    "belong in simulate"
  )
  expect_error(
    specify("logit", "bertrand", prices = x$prices,
      parameters = list(alpha = -48, meanval = rep(0, 6)),
      owner = x$owner, tariffPost = x$tariff),
    "belong in simulate"
  )
})

test_that("update genuinely recalibrates a stored trade baseline", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)

  unchanged <- update(fit)
  expect_equal(unchanged@model@slopes, fit@model@slopes, tolerance = 1e-9)
  expect_equal(unchanged@model@mcPre, fit@model@mcPre, tolerance = 1e-9)

  new_margins <- x$margins * .9
  changed <- update(fit, margins = new_margins)
  direct <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = new_margins, owner = x$owner)
  expect_equal(changed@model@slopes, direct@model@slopes, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(changed@model@slopes,
                                fit@model@slopes)))
  expect_true(is.call(update(fit, evaluate = FALSE)))
})

test_that("trade update preserves model identity and respecify changes conduct", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)

  expect_error(
    update(fit, conduct = "moncom"),
    "same demand, conduct, variant, and policy calibration"
  )

  respecified <- respecify(fit, conduct = "moncom")
  expect_equal(respecified@model@slopes, fit@model@slopes, tolerance = 0)
  expect_equal(respecified@diagnostics$transition$from,
               "logit::bertrand")
  expect_equal(respecified@diagnostics$transition$to,
               "logit::moncom")
  expect_null(respecified@diagnostics$calibration_args)
  expect_equal(respecified@diagnostics$source_calibration_args,
               fit@diagnostics$calibration_args)
  expect_error(update(respecified), "fit.*created by calibrate.*respecify")
  expect_equal(fit@spec$conduct, "bertrand")
})

test_that("trade respecification rejects nonportable demand and Cournot transitions", {
  x <- trade_fit_data()
  fit <- calibrate("logit", "bertrand", prices = x$prices,
    quantities = x$quantities, margins = x$margins, owner = x$owner)
  expect_s4_class(respecify(fit, demand = "ces", gamma = 1.2), "TradeFit")
  expect_error(respecify(fit, conduct = "cournot"),
    "not supported.*use update")

  specified <- specify("logit", "bertrand", prices = x$prices,
    parameters = list(alpha = -48.0457,
      meanval = c(0, .4149233, 1.1899885, .8252482, .1460183, 1.4865730)),
    owner = x$owner)
  expect_error(update(specified), "requires a fit created by calibrate")
  expect_equal(fit@model@slopes, fit@model@slopes, tolerance = 0)
})
