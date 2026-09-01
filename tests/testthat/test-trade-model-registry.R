test_that("trade model specifications normalize historical aliases", {
  spec <- model_spec("Log-It", "monopolistic competition")

  expect_s3_class(spec, "trade_model_spec")
  expect_equal(spec$demand, "logit")
  expect_equal(spec$conduct, "moncom")
  expect_equal(spec$variant, "standard")
  expect_equal(spec$policy, "tariff")
  expect_equal(spec$id, "logit::moncom")

  quota <- model_spec("logit", "bertrand", variant = "quota")
  expect_equal(quota$policy, "quota")
  expect_equal(quota$id, "logit::bertrand::quota")
})

test_that("registry contains only complete implemented trade models", {
  registry <- supportedModels()

  expect_true(nrow(registry) >= 10)
  expect_true(all(nzchar(registry$id)))
  expect_equal(length(unique(registry$id)), nrow(registry))
  expect_true(all(registry$calibrate))
  expect_true(all(registry$simulate))
  expect_true(all(registry$tariff | registry$quota))

  expect_true(any(registry$demand == "logit" & registry$conduct == "bertrand" &
    registry$policy == "tariff"))
  expect_true(any(registry$demand == "logit" & registry$conduct == "bertrand" &
    registry$policy == "quota"))
  expect_true(any(registry$demand == "linear" & registry$conduct == "cournot"))
})

test_that("unsupported combinations fail clearly", {
  expect_error(model_spec("ces", "auction2nd"), "unsupported trade model")
  expect_error(model_spec("logit", "stackelberg"), "unsupported conduct")
  expect_error(model_spec("not-a-demand", "bertrand"), "unsupported demand")
})
