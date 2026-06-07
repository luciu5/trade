beer_data <- function() {
  labels <- c("BUD", "OLD STYLE", "MILLER", "MILLER-LITE", "OTHER-LITE", "OTHER-REG")
  prices <- c(.0441, .0328, .0409, .0396, .0387, .0497)
  quantities <- c(.066, .172, .253, .187, .099, .223) * 100
  shares <- c(.066, .172, .253, .187, .099, .223)
  margins <- c(.3830, .5515, .5421, .5557, .4453, .3769)
  owner <- c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
  tariff <- c(0, 0, 0, 0, .1, .1)
  quota <- c(Inf, Inf, Inf, Inf, .8, .8)
  list(
    labels = labels,
    prices = prices,
    quantities = quantities,
    shares = shares,
    margins = margins,
    owner = owner,
    tariff = tariff,
    quota = quota
  )
}

test_that("Bertrand tariff and quota examples construct valid objects", {
  x <- beer_data()

  tariff <- suppressWarnings(
    bertrand_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  quota <- suppressWarnings(
    bertrand_quota(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      quotaPost = x$quota,
      labels = x$labels
    )
  )

  expect_s4_class(tariff, "TariffLogit")
  expect_s4_class(quota, "QuotaLogit")
  expect_equal(length(tariff@weights), length(x$prices))
  expect_equal(length(quota@weights), length(x$prices))
})

test_that("other documented tariff constructors construct valid objects", {
  x <- beer_data()

  auction <- suppressWarnings(
    auction2nd_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins * x$prices,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  bargaining <- suppressWarnings(
    bargaining_tariff(
      demand = "logit",
      prices = x$prices,
      shares = x$shares,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  moncom_logit <- suppressWarnings(
    monopolistic_competition_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  moncom_ces <- suppressWarnings(
    monopolistic_competition_tariff(
      demand = "ces",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  expect_s4_class(auction, "Tariff2ndLogit")
  expect_s4_class(bargaining, "TariffBargainingLogit")
  expect_s4_class(moncom_logit, "TariffMonComLogit")
  expect_s4_class(moncom_ces, "TariffMonComCES")
  expect_true(methods::isClass("TariffMonComCES"))
})

test_that("constructors require explicit ownership when ownership affects conduct", {
  x <- beer_data()

  expect_error(
    bertrand_tariff("logit", x$prices, x$quantities, x$margins, tariffPost = x$tariff),
    "owner"
  )
  expect_error(
    bertrand_quota("logit", x$prices, x$quantities, x$margins, quotaPost = x$quota),
    "owner"
  )
  expect_error(
    auction2nd_tariff("logit", x$prices, x$quantities, x$margins * x$prices, tariffPost = x$tariff),
    "owner"
  )
  expect_error(
    bargaining_tariff("logit", x$prices, x$shares, x$margins, tariffPost = x$tariff),
    "owner"
  )
})

test_that("no-change defaults are valid no-op simulations", {
  x <- beer_data()

  tariff <- suppressWarnings(
    bertrand_tariff("logit", x$prices, x$quantities, x$margins, owner = x$owner)
  )
  quota <- suppressWarnings(
    bertrand_quota("logit", x$prices, x$quantities, x$margins, owner = x$owner)
  )

  expect_equal(tariff@pricePost, tariff@pricePre, tolerance = 1e-7)
  expect_equal(quota@pricePost, quota@pricePre, tolerance = 1e-7)
})

test_that("sim supports documented auction2nd supply and deprecated auction alias", {
  x <- beer_data()
  demand_param <- list(
    alpha = -48.0457,
    meanval = c(0, 0.4149233, 1.1899885, 0.8252482, 0.1460183, 1.4865730)
  )

  bertrand <- suppressWarnings(
    sim(
      x$prices,
      demand = "logit",
      supply = "bertrand",
      demand_param,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )
  auction2nd <- suppressWarnings(
    sim(
      x$prices,
      demand = "logit",
      supply = "auction2nd",
      demand_param,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  expect_s4_class(bertrand, "TariffLogit")
  expect_s4_class(auction2nd, "Tariff2ndLogit")
  expect_warning(
    sim(
      x$prices,
      demand = "logit",
      supply = "auction",
      demand_param,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    ),
    "deprecated"
  )
})

test_that("Cournot tariff example constructs with plant-level ownership", {
  n <- 5
  cap <- seq(.42, .58, length.out = n)
  intercept <- 10
  slope <- -.25
  tariffPre <- tariffPost <- rep(0, n)
  tariffPost[1] <- .75

  b_pre <- matrix(slope, nrow = n, ncol = n)
  diag(b_pre) <- 2 * diag(b_pre) - 1 / cap
  quantity_pre <- rowSums(solve(b_pre) * -intercept)
  price_pre <- intercept + slope * sum(quantity_pre)
  mc_pre <- quantity_pre / cap
  margin_pre <- 1 - mc_pre / price_pre
  owner <- diag(n)

  result <- suppressWarnings(
    cournot_tariff(
      prices = price_pre,
      quantities = as.matrix(quantity_pre),
      margins = as.matrix(margin_pre),
      owner = owner,
      tariffPre = as.matrix(tariffPre),
      tariffPost = as.matrix(tariffPost)
    )
  )

  expect_s4_class(result, "TariffCournot")
  expect_equal(dim(result@quantityPost), c(n, 1))
  expect_error(
    cournot_tariff(
      prices = price_pre,
      quantities = as.matrix(quantity_pre),
      margins = as.matrix(margin_pre),
      owner = diag(1),
      tariffPre = as.matrix(tariffPre),
      tariffPost = as.matrix(tariffPost)
    ),
    "owner"
  )
})

test_that("Logit Cournot tariff constructs with diversion calibration", {
  x <- beer_data()

  result <- suppressWarnings(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  expect_s4_class(result, "TariffLogitCournot")
  expect_true(is.matrix(result@ownerPre))
  expect_equal(dim(result@ownerPre), c(length(x$prices), length(x$prices)))
  expect_true(all(is.finite(result@pricePost)))
  expect_true(all(is.finite(result@mcPost)))
  expect_named(calcProducerSurplus(result, TRUE), x$labels)
  expect_output(summary(result), "TariffLogitCournot")
})

test_that("Logit Cournot tariff supports ALM calibration", {
  x <- beer_data()

  result <- suppressWarnings(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      mktElast = -1.5,
      calibration = "alm",
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  expect_s4_class(result, "TariffLogitCournotALM")
  expect_true(result@shareInside < 1)
  expect_true(all(is.finite(result@pricePost)))
  expect_named(calcProducerSurplus(result, FALSE), x$labels)
  expect_output(summary(result, market = TRUE), "Industry Price Change")
})

test_that("Logit Cournot tariff requires ownership and validates dimensions", {
  x <- beer_data()

  expect_error(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      tariffPost = x$tariff
    ),
    "owner"
  )

  expect_error(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = diag(1),
      tariffPost = x$tariff
    ),
    "owner"
  )

  expect_error(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = rep(0, 2)
    ),
    "length-k"
  )
})

test_that("Logit Cournot tariff no-change case matches antitrust", {
  x <- beer_data()
  owner <- factor(x$owner, levels = unique(x$owner))
  owner <- model.matrix(~-1+owner)
  owner <- tcrossprod(owner)

  result <- suppressWarnings(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = owner,
      labels = x$labels
    )
  )

  base <- suppressWarnings(
    antitrust::logit.cournot(
      prices = x$prices,
      shares = x$quantities/sum(x$quantities),
      margins = x$margins,
      ownerPre = owner,
      ownerPost = owner,
      insideSize = sum(x$quantities),
      labels = x$labels
    )
  )

  expect_equal(result@pricePost, result@pricePre, tolerance = 1e-7)
  expect_equal(result@pricePre, base@pricePre, tolerance = 1e-7)
  expect_equal(result@pricePost, base@pricePost, tolerance = 1e-7)
  expect_equal(result@mcPre, base@mcPre, tolerance = 1e-7)
})

test_that("Logit Cournot tariff normalizes NA tariffs to zero", {
  x <- beer_data()
  tariff <- x$tariff
  tariff[1] <- NA_real_

  result <- suppressWarnings(
    logit_cournot_tariff(
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = tariff,
      labels = x$labels
    )
  )

  expect_false(any(is.na(result@tariffPost)))
  expect_equal(result@tariffPost[1], 0)
})

test_that("trade re-exports antitrust diagnostics used by tariff workflows", {
  x <- beer_data()

  result <- suppressWarnings(
    bertrand_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff,
      labels = x$labels
    )
  )

  expect_true(exists("calcDiagnostics", mode = "function"))
  expect_true(exists("calcPriceDelta", mode = "function"))
  expect_true(exists("calcShares", mode = "function"))
  expect_true(exists("calcRevenues", mode = "function"))
  expect_true(exists("calcMC", mode = "function"))
  expect_true(exists("CV", mode = "function"))
  expect_true(exists("elast", mode = "function"))
  expect_true(exists("diversion", mode = "function"))
  expect_true(exists("ownerToMatrix", mode = "function"))
  expect_true(exists("ownerToVec", mode = "function"))

  expect_s3_class(calcDiagnostics(result), "data.frame")
  expect_equal(length(calcPriceDelta(result)), length(x$prices))
  expect_equal(length(calcShares(result)), length(x$prices))
  expect_equal(length(calcRevenues(result)), length(x$prices))
  expect_equal(length(calcMC(result)), length(x$prices))
  expect_true(is.matrix(elast(result)))
  expect_true(is.matrix(diversion(result)))
  expect_true(is.numeric(CV(result)))
})

test_that("internal owner and tariff helpers preserve row-wise conduct scaling", {
  owner <- trade:::.owner_to_matrix(c("A", "A", "B"), 3, "bad owner")
  tariff <- c(0, .25, .5)
  scaled <- trade:::.apply_tariff_to_owner(owner, tariff)

  expect_equal(unname(owner), matrix(c(1,1,0,1,1,0,0,0,1), nrow = 3))
  expect_equal(scaled[1, ], owner[1, ])
  expect_equal(scaled[2, ], owner[2, ] * .75)
  expect_equal(scaled[3, ], owner[3, ] * .5)
  expect_equal(trade:::.tariff_mc_delta(rep(0, 3), tariff), tariff/(1 - tariff))
  expect_equal(trade:::.normalize_tariff(c(NA, .1), 2, "tariff"), c(0, .1))
  expect_error(trade:::.normalize_tariff(c(0, 1), 2, "tariff"), "less than 1")
  expect_equal(trade:::.normalize_quota(c(NA, 2), 2, "quota"), c(Inf, 2))
})
