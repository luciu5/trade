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

test_that("registered trade counterfactual capabilities have behavioral anchors", {
  capabilities <- getFromNamespace(
    ".trade_counterfactual_capabilities", "trade"
  )
  fit <- calibrate(
    "logit", "bertrand",
    prices = c(2, 2.2, 2.5),
    quantities = c(17.5, 11.36363636, 8),
    margins = c(.4, .35, .3),
    owner = c("A", "B", "C")
  )
  model_capabilities <- capabilities(fit@spec)

  expect_true(all(model_capabilities[c("exit", "tariff", "quality")]))
  expect_false(any(model_capabilities[c(
    "ownership", "costs", "capacity", "bargaining", "leader", "products",
    "quota", "entry"
  )]))

  quality <- simulate(fit, counterfactual(quality = c(Prod1 = .1)))
  tariff <- simulate(fit, counterfactual(tariff = rep(.01, 3)))
  expect_true(all(is.finite(quality@pricePost)))
  expect_true(all(is.finite(tariff@pricePost)))
  expect_error(
    simulate(fit, counterfactual(ownership = c("A", "A", "C"))),
    "does not support.*ownership"
  )
  expect_error(
    simulate(fit, counterfactual(entry = antitrust::entrant(
      "E1", meanval = .1, cost = 1.5, priceStart = 2.2
    ))),
    "does not support.*entry"
  )

  ces_spec <- model_spec("ces", "moncom")
  ces_capabilities <- capabilities(ces_spec)
  expect_true(all(ces_capabilities[c("exit", "tariff", "quality")]))
  expect_false(any(ces_capabilities[c(
    "ownership", "costs", "capacity", "bargaining", "leader", "products",
    "quota", "entry"
  )]))

  cournot_spec <- model_spec("linear", "cournot")
  cournot_capabilities <- capabilities(cournot_spec)
  expect_true(all(cournot_capabilities[c("exit", "products", "tariff")]))
  expect_false(any(cournot_capabilities[c(
    "ownership", "costs", "capacity", "bargaining", "leader", "quality",
    "quota", "entry"
  )]))
})

test_that("unsupported combinations fail clearly", {
  expect_error(model_spec("ces", "auction2nd"), "unsupported trade model")
  expect_error(model_spec("logit", "stackelberg"), "unsupported conduct")
    expect_error(model_spec("not-a-demand", "bertrand"), "unsupported demand")
})

test_that("trade transition registry records only supported supplied paths", {
    transitions <- getFromNamespace(
        ".trade_transition_registry", "trade"
    )()
    keys <- vapply(
        transitions,
        function(entry) paste(entry$from, entry$to, sep = "->"),
        character(1)
    )

    expect_false(anyDuplicated(keys) > 0L)
    expect_true(all(vapply(
        transitions,
        function(entry) entry$kind %in% c(
            "structural-restriction", "algebraic-translation",
            "conditional-translation", "conduct_change"
        ),
        logical(1)
    )))
    expect_true(any(keys == "logit::bertrand->ces::bertrand"))
    expect_true(any(keys == "logit::bertrand->logit::moncom"))
    expect_true(any(grepl("cournot->", keys)))
    expect_false(any(grepl("->.*cournot", keys)))
})
