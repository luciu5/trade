trade_oracle_data <- function() {
  list(
    prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
    quantities = c(.066, .172, .253, .187, .099, .223) * 100,
    shares = c(.066, .172, .253, .187, .099, .223),
    margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
    owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG"),
    tariff = c(0, 0, 0, 0, .1, .1),
    quota = c(Inf, Inf, Inf, Inf, .8, .8)
  )
}

test_that("master tariff Bertrand Logit oracle is stable", {
  x <- trade_oracle_data()
  result <- suppressWarnings(
    bertrand_tariff(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )

  expect_s4_class(result, "TariffLogit")
  expect_equal(unname(result@slopes$alpha), -61.7009350646228, tolerance = 1e-8)
  expect_equal(unname(result@slopes$meanval),
    c(.428164783544307, .688783952101098, 1.57445653803861,
      1.19196445058167, .500444842303509, 1.99120704933989),
    tolerance = 1e-8
  )
  expect_equal(unname(result@mcPre),
    c(.0272190226433795, .0147112883434306, .0188167596936236,
      .0175167596936236, .0214606854960865, .0309664386911101),
    tolerance = 1e-8
  )
  expect_equal(unname(result@pricePost),
    c(.0441240203491097, .0328663515875998, .0410906127373377,
      .0397906127373377, .0409754833394578, .0527370740005461),
    tolerance = 1e-8
  )
  expect_equal(
    unname(calcShares(result, preMerger = FALSE)),
    c(.0412770212027558, .107289824787497, .156610494869286,
      .115755583164255, .0538850834180932, .115805812655655),
    tolerance = 1e-8
  )
})

test_that("master quota Logit oracle is stable", {
  x <- trade_oracle_data()
  result <- suppressWarnings(
    bertrand_quota(
      demand = "logit",
      prices = x$prices,
      quantities = x$quantities,
      margins = x$margins,
      owner = x$owner,
      quotaPost = x$quota
    )
  )

  expect_s4_class(result, "QuotaLogit")
  expect_equal(unname(result@pricePost),
    c(.0441374437401975, .0329034000037767, .0411964685156242,
      .0398964685841351, .0431946226188822, .0541946256671267),
    tolerance = 1e-8
  )
  expect_equal(unname(result@mcPre),
    c(.0272190224547355, .0147112875784321, .0188167564869581,
      .0175167564869581, .0214606851442789, .0309664375875976),
    tolerance = 1e-8
  )
  expect_equal(unname(calcMargins(result, preMerger = FALSE)),
    c(.38331221320101, .552894605187181, .543243461008386,
      .560944688389038, .4453, .3769),
    tolerance = 1e-8
  )
})

test_that("master supplied-parameter Logit oracle is stable", {
  x <- trade_oracle_data()
  parameters <- list(
    alpha = -48.0457,
    meanval = c(0, .4149233, 1.1899885, .8252482, .1460183, 1.4865730)
  )
  result <- suppressWarnings(
    sim(
      prices = x$prices,
      demand = "logit",
      supply = "bertrand",
      demand.param = parameters,
      owner = x$owner,
      tariffPost = x$tariff
    )
  )

  expect_s4_class(result, "TariffLogit")
  expect_equal(unname(result@slopes$alpha), parameters$alpha, tolerance = 1e-12)
  expect_equal(unname(result@pricePost),
    c(.0441635422022628, .0329808672675438, .0414213594964696,
      .0401213594964696, .0403428359542767, .0518125635454066),
    tolerance = 1e-8
  )
})
