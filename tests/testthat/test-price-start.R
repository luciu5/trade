test_that("AIDS uses available pre-merger prices as the default start", {
  prices <- c(10, 12, 15)
  fit <- suppressWarnings(
    bertrand_tariff(
      demand = "aids",
      prices = prices,
      quantities = c(30, 40, 50),
      margins = c(.2, .25, .3),
      owner = c("A", "B", "C"),
      tariffPost = c(0, .1, 0)
    )
  )

  expect_equal(fit@priceStart, prices)
})
