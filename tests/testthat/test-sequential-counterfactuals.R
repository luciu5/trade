## Tests for sequential (multi-step) trade counterfactual composition:
## tariff sequencing, quota sequencing, quality sequencing (moncom only),
## interactions with exit, and the explicit rejections (ownership
## unsupported, entry unsupported).

.trade_seq_fit_data <- function() {
    list(
        prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
        quantities = c(.066, .172, .253, .187, .099, .223) * 100,
        margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
        owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
    )
}

.trade_seq_fit <- function() {
    x <- .trade_seq_fit_data()
    calibrate("logit", "bertrand", prices = x$prices,
             quantities = x$quantities, margins = x$margins, owner = x$owner)
}

test_that("sequential tariff -> tariff resolves to the same level as a direct one-shot tariff", {
    fit <- .trade_seq_fit()
    cf <- counterfactual(tariff = c(rep(0, 5), .1))
    cf <- add_step(cf, tariff = c(rep(0, 5), .2))
    path <- simulate(fit, cf)

    expect_s4_class(path, "CounterfactualPath")
    expect_length(path@steps, 2L)

    step2 <- result_at(path, 2)
    reference <- simulate(fit, tariffPost = c(rep(0, 5), .2))
    ## Tariffs are absolute target levels (not deltas), so a sequential path
    ## that ends at 0.2 must match a direct one-shot 0.2 tariff exactly.
    expect_equal(step2@mcPost, reference@mcPost, tolerance = 1e-8)
    expect_equal(step2@pricePost, reference@pricePost, tolerance = 1e-8)
})

test_that("an unspecified tariff persists at its current active level across steps", {
    fit <- .trade_seq_fit()
    cf <- counterfactual(tariff = c(rep(0, 5), .1))
    cf <- add_step(cf, exit = 1L)
    path <- simulate(fit, cf)

    step2 <- result_at(path, 2)
    expect_equal(step2@tariffPost, c(rep(0, 5), .1), tolerance = 1e-10)
})

test_that("simulate() preserves the ordinary legacy result for one-step Counterfactuals", {
    fit <- .trade_seq_fit()
    result <- simulate(fit, counterfactual(tariff = c(rep(0, 5), .1)))
    expect_s4_class(result, "TariffLogit")
    expect_false(methods::is(result, "CounterfactualPath"))
})

test_that("simulate() can resume tariff sequencing from a CounterfactualPath", {
    fit <- .trade_seq_fit()
    cf1 <- counterfactual(tariff = c(rep(0, 5), .1))
    cf1 <- add_step(cf1, tariff = c(rep(0, 5), .2))
    p1 <- simulate(fit, cf1)

    p2 <- simulate(p1, counterfactual(exit = 1L))
    expect_s4_class(p2, "CounterfactualPath")
    expect_length(p2@steps, 3L)
    step3 <- result_at(p2, 3)
    expect_equal(step3@tariffPost, c(rep(0, 5), .2), tolerance = 1e-10)
    expect_false(step3@subset[1])
})

## ---- Quality (moncom only) --------------------------------------------------

test_that("quality is verified only for logit::moncom::tariff and ces::moncom::tariff", {
    x <- .trade_seq_fit_data()
    fit <- calibrate("logit", "moncom", prices = x$prices,
                     quantities = x$quantities, margins = x$margins)
    baseline_meanval <- fit@model@slopes$meanval

    cf <- counterfactual(quality = stats::setNames(.1, names(baseline_meanval)[1]))
    result <- simulate(fit, cf)
    expect_equal(
        result@slopes$meanval[[1]],
        baseline_meanval[[1]] * 1.1, tolerance = 1e-10
    )
})

test_that("quality persists into a later tariff change and vice versa", {
    x <- .trade_seq_fit_data()
    fit <- calibrate("logit", "moncom", prices = x$prices,
                     quantities = x$quantities, margins = x$margins)
    baseline_meanval <- fit@model@slopes$meanval
    label1 <- names(baseline_meanval)[1]

    cf <- counterfactual(quality = stats::setNames(.1, label1))
    cf <- add_step(cf, tariff = c(rep(0, 5), .1))
    path <- simulate(fit, cf)
    step2 <- result_at(path, 2)
    expect_equal(step2@slopes$meanval[[1]], baseline_meanval[[1]] * 1.1, tolerance = 1e-10)

    cf2 <- counterfactual(tariff = c(rep(0, 5), .1))
    cf2 <- add_step(cf2, quality = stats::setNames(.1, label1))
    path2 <- simulate(fit, cf2)
    step2b <- result_at(path2, 2)
    expect_equal(step2b@slopes$meanval[[1]], baseline_meanval[[1]] * 1.1, tolerance = 1e-10)
    expect_equal(step2b@tariffPost, c(rep(0, 5), .1), tolerance = 1e-10)
})

test_that("quality is rejected for tariff Bertrand (LogitALM-descended, unverified)", {
    fit <- .trade_seq_fit()
    expect_error(
        simulate(fit, counterfactual(quality = c(BUD = .1))),
        "does not support"
    )
})

## ---- Exit + tariff interaction ---------------------------------------------

test_that("exit -> tariff and tariff -> exit both resolve consistently", {
    fit <- .trade_seq_fit()

    cf1 <- counterfactual(exit = 1L)
    cf1 <- add_step(cf1, tariff = c(rep(0, 5), .1))
    path1 <- simulate(fit, cf1)
    step2a <- result_at(path1, 2)
    expect_false(step2a@subset[1])
    expect_equal(step2a@tariffPost, c(rep(0, 5), .1), tolerance = 1e-10)

    cf2 <- counterfactual(tariff = c(rep(0, 5), .1))
    cf2 <- add_step(cf2, exit = 1L)
    path2 <- simulate(fit, cf2)
    step2b <- result_at(path2, 2)
    expect_false(step2b@subset[1])
    expect_equal(step2b@tariffPost, c(rep(0, 5), .1), tolerance = 1e-10)
})

## ---- Explicit rejections ----------------------------------------------------

test_that("ownership is rejected for trade counterfactuals (not a registered capability)", {
    fit <- .trade_seq_fit()
    expect_error(
        simulate(fit, counterfactual(ownership = c("A", "A", "B", "B", "C", "C"))),
        "does not support"
    )
})

test_that("entry is rejected outright for trade in this release", {
    expect_error(counterfactual(entry = antitrust::entrant(
        label = "E1", meanval = .1, cost = 1, priceStart = 2
    )), "not supported for trade")
})

## ---- Quota --------------------------------------------------------------

.trade_seq_quota_fit <- function() {
    x <- .trade_seq_fit_data()
    calibrate("logit", "bertrand", policy = "quota", prices = x$prices,
             quantities = x$quantities, margins = x$margins, owner = x$owner)
}

test_that("sequential quota -> quota resolves to the same level as a direct one-shot quota", {
    fit <- .trade_seq_quota_fit()
    cf <- counterfactual(quota = c(rep(Inf, 5), .8))
    cf <- add_step(cf, quota = c(rep(Inf, 5), .6))
    path <- simulate(fit, cf)

    expect_s4_class(path, "CounterfactualPath")
    step2 <- result_at(path, 2)
    reference <- simulate(fit, quotaPost = c(rep(Inf, 5), .6))
    ## Quotas are absolute target levels (not deltas), so a sequential path
    ## that ends at 0.6 must match a direct one-shot 0.6 quota exactly.
    expect_equal(step2@capacitiesPost, reference@capacitiesPost, tolerance = 1e-8)
    expect_equal(step2@pricePost, reference@pricePost, tolerance = 1e-8)
})

test_that("an unspecified quota persists at its current active level across steps", {
    fit <- .trade_seq_quota_fit()
    cf <- counterfactual(quota = c(rep(Inf, 5), .8))
    cf <- add_step(cf, exit = 1L)
    path <- simulate(fit, cf)

    step2 <- result_at(path, 2)
    expect_equal(step2@quotaPost, c(rep(Inf, 5), .8), tolerance = 1e-10)
})
