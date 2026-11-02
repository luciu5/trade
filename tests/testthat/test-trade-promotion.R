## S4 promotion tests.  The source of truth in each case is an
## antitrust::AntitrustFit; trade only supplies the policy wrapper.

.promotion_prices <- c(1.5, 1.8, 2.1)
.promotion_shares <- c(.30, .25, .15)
.promotion_owner <- c("A", "A", "B")
.promotion_labels <- c("P1", "P2", "P3")

.promotion_logit_parameters <- list(
  alpha = -1.5,
  meanval = c(.5, .2, -.1)
)
.promotion_ces_parameters <- list(
  alpha = .2,
  gamma = 2,
  meanval = c(.5, .2, .1)
)

.promotion_basic_fit <- function(demand = "logit", conduct = "bertrand") {
  parameters <- if (identical(demand, "logit")) {
    .promotion_logit_parameters
  } else {
    .promotion_ces_parameters
  }
  antitrust::specify(
    demand = demand, conduct = conduct,
    prices = .promotion_prices,
    parameters = parameters, ownerPre = .promotion_owner,
    margins = rep(.3, length(.promotion_prices)),
    bargpowerPre = rep(.5, length(.promotion_prices)),
    insideSize = 100, priceOutside = if (identical(demand, "ces")) 1 else 0,
    labels = .promotion_labels
  )
}

.promotion_blp_fit <- function(conduct = "bertrand") {
  antitrust::specify(
    demand = "blp", conduct = conduct,
    prices = .promotion_prices, shares = .promotion_shares,
    parameters = list(
      alphaMean = -1.5, sigma = .2,
      meanval = c(.5, .2, -.1)
    ),
    ownerPre = .promotion_owner, margins = rep(.3, 3),
    bargpowerPre = rep(.5, 3), insideSize = 100,
    draws = c(-1.5, -.5, .5, 1.5),
    integrationWeights = c(.1, .2, .3, .4),
    labels = .promotion_labels
  )
}

.promotion_aids_fit <- function() {
  antitrust::calibrate(
    demand = "aids", conduct = "bertrand",
    prices = rep(10, 4), shares = c(.4, .3, .2, .1),
    margins = c(.25, .25, NA, NA),
    ownerPre = c("A", "B", "C", "C"),
    mktElast = -1, insideSize = 100,
    labels = paste0("P", 1:4)
  )
}

.promotion_quantity_fit <- function(demand = "linear") {
  ## A one-market, three-plant Cournot fixture gives the target trade class
  ## the plant-by-product tariff matrix used by its legacy implementation.
  quantities <- matrix(c(.5, .6, .7), nrow = 3, ncol = 1,
                       dimnames = list(paste0("O", 1:3), "P1"))
  margins <- matrix(c(.30, .35, .40), nrow = 3, ncol = 1)
  antitrust::calibrate(
    demand = demand, conduct = "cournot", prices = 7,
    quantities = quantities, margins = margins,
    ownerPre = diag(3), labels = list(rownames(quantities), "P1"),
    mktElast = if (identical(demand, "loglin")) -1.5 else NULL
  )
}

.promotion_alm_fit <- function() {
  antitrust::calibrate(
    demand = "logit", conduct = "cournot", variant = "alm",
    prices = .promotion_prices, shares = c(.4, .3, .3),
    margins = rep(.3, 3), ownerPre = .promotion_owner,
    mktElast = -2, insideSize = 100, labels = .promotion_labels
  )
}

.promotion_source_fit <- function(key) {
  switch(key,
    "logit::bertrand::standard" = .promotion_basic_fit("logit", "bertrand"),
    "logit::moncom::standard" = .promotion_basic_fit("logit", "moncom"),
    "logit::cournot::standard" = .promotion_basic_fit("logit", "cournot"),
    "logit::auction2nd::standard" = .promotion_basic_fit("logit", "auction2nd"),
    "logit::bargaining::standard" = .promotion_basic_fit("logit", "bargaining"),
    "ces::bertrand::standard" = .promotion_basic_fit("ces", "bertrand"),
    "ces::moncom::standard" = .promotion_basic_fit("ces", "moncom"),
    "ces::bargaining::standard" = .promotion_basic_fit("ces", "bargaining"),
    "aids::bertrand::standard" = .promotion_aids_fit(),
    "blp::bertrand::standard" = .promotion_blp_fit("bertrand"),
    "blp::cournot::standard" = .promotion_blp_fit("cournot"),
    "blp::auction2nd::standard" = .promotion_blp_fit("auction2nd"),
    "blp::bargaining::standard" = .promotion_blp_fit("bargaining"),
    "linear::cournot::standard" = .promotion_quantity_fit("linear"),
    "loglin::cournot::standard" = .promotion_quantity_fit("loglin"),
    "logit::cournot::alm" = .promotion_alm_fit(),
    stop("No promotion fixture for source registry key '", key, "'.")
  )
}

.promotion_key <- function(x) {
  paste(x$demand, x$conduct, x$variant, sep = "::")
}

.promotion_target_policy <- function(target) {
  if (identical(target$policy, "quota")) "quota" else "tariff"
}

.promotion_policy_zero <- function(source, target) {
  if (identical(.promotion_target_policy(target), "quota")) {
    return(rep(Inf, length(source@model@pricePre)))
  }
  if (identical(target$class, "TariffCournot")) {
    q <- source@model@quantities
    return(matrix(0, nrow = nrow(q), ncol = ncol(q)))
  }
  rep(0, length(source@model@pricePre))
}

.promotion_compare_if_defined <- function(fun, source, target, info) {
  source_value <- try(fun(source@model), silent = TRUE)
  target_value <- try(fun(target@model), silent = TRUE)
  if (!inherits(source_value, "try-error")) {
    expect_false(inherits(target_value, "try-error"), info = info)
    if (!inherits(target_value, "try-error")) {
      ## Welfare is allowed to differ by the small numerical residual left by
      ## a calibrated source fit whose post-price state was solved once.
      expect_equal(target_value, source_value, tolerance = 1e-6, info = info)
    }
  }
}

test_that("selective tariffs leave domestic firms responsive in every direct promotion family", {
  trade_skip_unless_tier("extended")
  registry <- trade::supportedModels()
  registry <- registry[registry$policy == "tariff" &
    registry$promotion_handler != "tariff_reuse", , drop = FALSE]
  for (i in seq_len(nrow(registry))) {
    entry <- registry[i, , drop = FALSE]
    key <- .promotion_key(entry)
    source <- suppressWarnings(.promotion_source_fit(key))
    fit <- suppressWarnings(trade::as_trade_fit(source))
    tau <- .promotion_policy_zero(source, entry)
    tau[length(tau)] <- .3
    result <- suppressWarnings(trade::simulate(fit, tariffPost = tau))
    expect_true(all(is.finite(result@pricePost)), info = key)
    expect_equal(unname(result@ownerPost), unname(source@model@ownerPre), info = key)
    if (methods::is(result, "TariffCournot")) {
      expect_true(abs(result@quantityPost[1] - result@quantityPre[1]) > 1e-7, info = key)
    } else {
      pre <- antitrust::calcShares(fit@model, TRUE, revenue = FALSE)
      post <- antitrust::calcShares(result, FALSE, revenue = FALSE)
      expect_true(abs(post[1] - pre[1]) > 1e-7, info = key)
      expect_equal(as.numeric(result@mcPost) * (1 - tau),
                   as.numeric(fit@model@mcPre), tolerance = 1e-7, info = key)
    }
  }
})


test_that("promotion eligibility is the exact live registry overlap", {
  source <- antitrust::supportedModels()
  target <- trade::supportedModels()
  source_keys <- unique(.promotion_key(source))
  target_keys <- unique(.promotion_key(target))
  overlap <- intersect(source_keys, target_keys)

  expected <- c(
    "logit::bertrand::standard", "ces::bertrand::standard",
    "aids::bertrand::standard", "blp::bertrand::standard",
    "blp::cournot::standard", "blp::auction2nd::standard",
    "blp::bargaining::standard", "logit::moncom::standard",
    "ces::moncom::standard", "logit::cournot::standard",
    "logit::cournot::alm", "linear::cournot::standard",
    "loglin::cournot::standard", "logit::auction2nd::standard",
    "logit::bargaining::standard", "ces::bargaining::standard",
    "ces::cournot::standard", "logit::bertrand::alm",
    "ces::bertrand::alm", "ces::cournot::alm"
  )
  expect_setequal(overlap, expected)

  tariff <- target[target$policy == "tariff", , drop = FALSE]
  quota <- target[target$policy == "quota", , drop = FALSE]
  expect_equal(nrow(tariff), 24L)
  expect_equal(nrow(quota), 1L)
  expect_equal(.promotion_key(quota), "logit::bertrand::standard")
  expect_true(all(tariff$promote))
  expect_true(all(quota$promote))
})


test_that("as_trade_fit is an S4 generic with explicit fallback behavior", {
  expect_true(methods::isGeneric("as_trade_fit"))
  expect_true(methods::existsMethod("as_trade_fit", "AntitrustFit"))
  expect_true(methods::existsMethod("as_trade_fit", "StructuralFit"))

  setClass("PromotionDummyStructuralFit", contains = "StructuralFit")
  on.exit(removeClass("PromotionDummyStructuralFit"), add = TRUE)
  empty <- methods::new("PromotionDummyStructuralFit")
  expect_error(
    trade::as_trade_fit(empty),
    "no promotion route|promotion.*defined|StructuralFit"
  )

  unsupported <- methods::new(
    "AntitrustFit",
    spec = antitrust::model_spec("logit_nests", "bertrand"),
    model = list(), parameters = list(), observed = list(), diagnostics = list()
  )
  expect_error(
    trade::as_trade_fit(unsupported),
    "unsupported|no promotion route|not eligible|no registered implementation"
  )

  ## Registry eligibility includes the source model's concrete S4 class.  A
  ## fit whose labels claim Bertrand but whose model is MonCom must not be
  ## silently promoted through the shared demand class.
  wrong_class <- .promotion_basic_fit("logit", "bertrand")
  wrong_class@model <- methods::as(wrong_class@model, "MonComLogit")
  expect_error(
    trade::as_trade_fit(wrong_class),
    "does not match.*registered.*logit::bertrand"
  )
})


test_that("every exact tariff overlap has a direct promotion route", {
  trade_skip_unless_tier("extended")
  target_registry <- trade::supportedModels()
  target_registry <- target_registry[
    target_registry$policy == "tariff" &
      target_registry$promotion_handler != "tariff_reuse", , drop = FALSE
  ]

  for (i in seq_len(nrow(target_registry))) {
    target_entry <- target_registry[i, , drop = FALSE]
    key <- .promotion_key(target_entry)
    source <- suppressWarnings(suppressMessages(.promotion_source_fit(key)))
    source_model_before <- source@model
    promoted <- suppressWarnings(suppressMessages(
      trade::as_trade_fit(source, policy = "tariff")
    ))

    expect_true(methods::is(promoted, "TradeFit"), info = key)
    expect_equal(promoted@spec$demand, target_entry$demand, info = key)
    expect_equal(promoted@spec$conduct, target_entry$conduct, info = key)
    expect_equal(promoted@spec$variant, target_entry$variant, info = key)
    expect_equal(promoted@spec$policy, "tariff", info = key)
    expect_equal(class(promoted@model)[[1L]], target_entry$class, info = key)

    zero <- .promotion_policy_zero(source, target_entry)
    expect_equal(promoted@model@tariffPre, zero, tolerance = 0, info = key)
    expect_equal(promoted@model@tariffPost, zero, tolerance = 0, info = key)
    expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre),
                 tolerance = 1e-10, info = paste(key, "pricePre"))
    expect_equal(promoted@model@labels, source@model@labels, info = key)
    expect_equal(promoted@model@ownerPre, source@model@ownerPre,
                 tolerance = 1e-10, info = paste(key, "ownerPre"))
    common_slots <- intersect(
      c("priceOutside", "insideSize", "shareInside", "normIndex", "weights",
        "slopes", "intercepts", "bargpowerPre", "nDraws", "quantityPre",
        "productsPre", "capacitiesPre"),
      intersect(methods::slotNames(promoted@model),
                methods::slotNames(source@model))
    )
    for (slot_name in common_slots) {
      expect_equal(methods::slot(promoted@model, slot_name),
                   methods::slot(source@model, slot_name), tolerance = 1e-10,
                   info = paste(key, slot_name))
    }
    expect_equal(unname(promoted@model@mcPre), unname(source@model@mcPre),
                 tolerance = 1e-10, info = paste(key, "mcPre"))
    expect_equal(promoted@parameters, source@parameters,
                 tolerance = 1e-10, info = paste(key, "parameters"))

    source_quantities <- try(antitrust::calcQuantities(source@model, TRUE), silent = TRUE)
    target_quantities <- try(trade::calcQuantities(promoted@model, TRUE), silent = TRUE)
    if (!inherits(source_quantities, "try-error")) {
      expect_false(inherits(target_quantities, "try-error"),
                   info = paste(key, "quantity method"))
      if (!inherits(target_quantities, "try-error")) {
        expect_equal(target_quantities, source_quantities,
                     tolerance = 1e-7, info = paste(key, "quantities"))
      }
    }
    expect_equal(trade::calcShares(promoted@model, TRUE),
                 antitrust::calcShares(source@model, TRUE),
                 tolerance = 1e-7, info = paste(key, "shares"))

    .promotion_compare_if_defined(
      function(model) antitrust::elast(model, TRUE), source, promoted,
      paste(key, "elasticities")
    )
    .promotion_compare_if_defined(
      function(model) antitrust::diversion(model, TRUE), source, promoted,
      paste(key, "diversion")
    )
    .promotion_compare_if_defined(
      function(model) antitrust::calcProducerSurplus(model, TRUE),
      source, promoted, paste(key, "producer surplus")
    )
    .promotion_compare_if_defined(
      function(model) antitrust::CV(model), source, promoted,
      paste(key, "consumer welfare")
    )

    if (identical(key, "blp::bertrand::standard")) {
      for (field in c("consDraws", "priceDraws", "demogDraws",
                      "drawWeights", "integrationWeights",
                      "integrationPoints", "factorOrder", "nodesPerAxis")) {
        if (!is.null(source@model@slopes[[field]])) {
          expect_equal(promoted@model@slopes[[field]], source@model@slopes[[field]],
                       tolerance = 0, info = paste(key, field))
        }
      }
    }
    expect_equal(source@model, source_model_before,
                 info = paste(key, "source immutability"))
  }
})


test_that("quota promotion uses its exact corresponding implementation", {
  source <- .promotion_basic_fit("logit", "bertrand")
  promoted <- suppressWarnings(suppressMessages(
    trade::as_trade_fit(source, policy = "quota")
  ))

  expect_s4_class(promoted, "TradeFit")
  expect_equal(class(promoted@model)[[1L]], "QuotaLogit")
  expect_equal(promoted@spec$policy, "quota")
  expect_equal(promoted@model@quotaPre, rep(Inf, 3), tolerance = 0)
  expect_equal(promoted@model@quotaPost, rep(Inf, 3), tolerance = 0)
  expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre), tolerance = 1e-10)
  expect_equal(promoted@model@ownerPre, source@model@ownerPre, tolerance = 1e-10)
  expect_equal(promoted@model@mcPre, source@model@mcPre, tolerance = 1e-10)
  expect_equal(promoted@model@insideSize, source@model@insideSize, tolerance = 0)
  expect_equal(promoted@model@shareInside, source@model@shareInside, tolerance = 0)
  expect_equal(trade::calcShares(promoted@model, TRUE),
               antitrust::calcShares(source@model, TRUE), tolerance = 1e-7)
})


test_that("promotion copies baseline state without recalibration or a price solve", {
  target_registry <- trade::supportedModels()
  target_registry <- target_registry[
    target_registry$policy == "tariff" &
      target_registry$promotion_handler != "tariff_reuse", , drop = FALSE
  ]
  keys <- .promotion_key(target_registry)
  sources <- lapply(keys, function(key) {
    suppressWarnings(suppressMessages(.promotion_source_fit(key)))
  })
  names(sources) <- keys
  local_mocked_bindings(
    calibrate = function(...) stop("promotion unexpectedly called calibrate"),
    specify = function(...) stop("promotion unexpectedly called specify"),
    calcSlopes = function(...) stop("promotion unexpectedly called calcSlopes"),
    calcMC = function(...) stop("promotion unexpectedly called calcMC"),
    calcPrices = function(...) stop("promotion unexpectedly called calcPrices"),
    calcMeanval = function(...) stop("promotion unexpectedly called calcMeanval"),
    `.blp_contract` = function(...) stop("promotion unexpectedly called BLP contraction"),
    .package = "antitrust"
  )
  local_mocked_bindings(
    calibrate = function(...) stop("promotion unexpectedly called trade calibrate"),
    specify = function(...) stop("promotion unexpectedly called trade specify"),
    calcPrices = function(...) stop("promotion unexpectedly called trade calcPrices"),
    calcSlopes = function(...) stop("promotion unexpectedly called trade calcSlopes"),
    .package = "trade"
  )
  for (key in names(sources)) {
    source <- sources[[key]]
    promoted <- suppressWarnings(suppressMessages(
      trade::as_trade_fit(source, policy = "tariff")
    ))
    expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre),
                 tolerance = 0, info = key)
    expect_equal(unname(promoted@model@pricePost), unname(source@model@pricePre),
                 tolerance = 0, info = paste(key, "pricePost"))
    expect_equal(promoted@model@shares, source@model@shares,
                 tolerance = 0, info = paste(key, "shares"))
    if ("shareInside" %in% methods::slotNames(promoted@model) &&
        "shareInside" %in% methods::slotNames(source@model)) {
      expect_equal(unname(promoted@model@shareInside),
                   unname(source@model@shareInside), tolerance = 0,
                   info = paste(key, "shareInside"))
    }
    expect_equal(unname(promoted@model@mcPre), unname(source@model@mcPre),
                 tolerance = 0, info = paste(key, "mcPre"))
    expect_equal(unname(promoted@model@mcPost), unname(source@model@mcPre),
                 tolerance = 0, info = paste(key, "mcPost"))
  }
  quota_source <- sources[["logit::bertrand::standard"]]
  quota_fit <- trade::as_trade_fit(quota_source, policy = "quota")
  expect_identical(quota_fit@model@pricePre, quota_source@model@pricePre)
  expect_identical(quota_fit@model@pricePost, quota_source@model@pricePre)
  expect_identical(quota_fit@model@mcPre, quota_source@model@mcPre)
})


test_that("tariff baseline state is retained and policy shocks are relative", {
  source <- .promotion_basic_fit("logit", "bertrand")
  tariff_pre <- c(.10, .10, .20)
  tariff_post <- c(.25, .05, .30)
  promoted <- suppressWarnings(suppressMessages(
    trade::as_trade_fit(source, policy = "tariff", tariffPre = tariff_pre)
  ))

  expect_equal(promoted@model@tariffPre, tariff_pre, tolerance = 0)
  expect_equal(promoted@model@tariffPost, tariff_pre, tolerance = 0)
  expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre), tolerance = 1e-10)
  expect_equal(unname(promoted@model@mcPre), unname(source@model@mcPre), tolerance = 1e-10)

  unchanged <- suppressWarnings(suppressMessages(
    trade::simulate(promoted, tariffPost = tariff_pre)
  ))
  expect_equal(unname(unchanged@pricePost), unname(source@model@pricePre), tolerance = 1e-7)
  expect_equal(unname(unchanged@mcPost), unname(source@model@mcPre), tolerance = 1e-7)
  expect_equal(trade::calcShares(unchanged, TRUE),
               antitrust::calcShares(source@model, TRUE), tolerance = 1e-7)

  shocked <- suppressWarnings(suppressMessages(
    trade::simulate(promoted, tariffPost = tariff_post)
  ))
  expected_mc <- source@model@mcPre * (1 - tariff_pre) / (1 - tariff_post)
  expect_equal(shocked@mcPost, expected_mc, tolerance = 1e-7)
  expect_true(any(abs(shocked@pricePost - source@model@pricePre) > 1e-8))
  expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre), tolerance = 0)
})


test_that("promotion resets stale source post-state without mutating the source", {
  source <- .promotion_basic_fit("logit", "bertrand")
  source@model@pricePost <- source@model@pricePre + c(.20, .30, .40)
  source@model@mcPost <- source@model@mcPre + c(.10, .20, .30)
  source_before <- source@model

  promoted <- suppressWarnings(suppressMessages(
    trade::as_trade_fit(source, policy = "tariff")
  ))
  expect_equal(promoted@model@pricePre, source@model@pricePre, tolerance = 0)
  expect_equal(promoted@model@pricePost, source@model@pricePre, tolerance = 0)
  expect_equal(promoted@model@mcPost, source@model@mcPre, tolerance = 0)
  expect_equal(source@model, source_before)
})


test_that("nonzero policy states work for each specialized tariff wrapper", {
  keys <- c(
    "logit::auction2nd::standard", "logit::bargaining::standard",
    "ces::bargaining::standard", "blp::auction2nd::standard",
    "blp::bargaining::standard", "logit::moncom::standard",
    "ces::moncom::standard", "logit::cournot::standard"
  )
  for (key in keys) {
    source <- suppressWarnings(suppressMessages(.promotion_source_fit(key)))
    pre <- c(.05, .05, .10)
    post <- c(.20, .20, .25)
    promoted <- suppressWarnings(suppressMessages(
      trade::as_trade_fit(source, policy = "tariff", tariffPre = pre)
    ))
    unchanged <- suppressWarnings(suppressMessages(
      trade::simulate(promoted, tariffPost = pre)
    ))
    expect_equal(unname(unchanged@pricePost), unname(source@model@pricePre),
                 tolerance = 1e-6, info = paste(key, "no-change price"))
    expect_equal(unname(unchanged@mcPost), unname(source@model@mcPre),
                 tolerance = 1e-6, info = paste(key, "no-change mc"))

    shocked <- suppressWarnings(suppressMessages(
      trade::simulate(promoted, tariffPost = post)
    ))
    expected_mc <- source@model@mcPre * (1 - pre) / (1 - post)
    expect_equal(unname(shocked@mcPost), unname(expected_mc),
                 tolerance = 1e-6, info = paste(key, "shock mc"))
    expect_true(any(abs(shocked@pricePost - source@model@pricePre) > 1e-8),
                info = paste(key, "shock price"))
    expect_equal(unname(promoted@model@pricePre),
                 unname(source@model@pricePre), tolerance = 0,
                 info = paste(key, "source baseline"))
  }
})


test_that("plant-by-product tariff states are relative for Cournot promotion", {
  for (demand in c("linear", "loglin")) {
    source <- suppressWarnings(suppressMessages(
      .promotion_source_fit(paste0(demand, "::cournot::standard"))
    ))
    tariff_pre <- matrix(c(.05, .10, .15), nrow = 3, ncol = 1,
                         dimnames = dimnames(source@model@quantities))
    tariff_post <- matrix(c(.25, .20, .30), nrow = 3, ncol = 1,
                          dimnames = dimnames(source@model@quantities))
    promoted <- suppressWarnings(suppressMessages(
      trade::as_trade_fit(source, policy = "tariff", tariffPre = tariff_pre)
    ))

    expected_mc <- vapply(seq_len(nrow(source@model@quantityPre)), function(i) {
      source@model@mcfunPre[[i]](source@model@quantityPre[i, ])
    }, numeric(1)) * as.numeric(1 - tariff_pre)
    expect_equal(as.numeric(trade::calcMC(promoted@model, TRUE)),
                 as.numeric(expected_mc), info = paste(demand, "physical costs"))
    unchanged <- suppressWarnings(suppressMessages(
      trade::simulate(promoted, tariffPost = tariff_pre)
    ))
    expect_equal(unname(unchanged@quantityPost),
                 unname(source@model@quantityPre), tolerance = 1e-6,
                 info = paste(demand, "no-change quantity"))
    expect_equal(unname(unchanged@pricePost),
                 unname(source@model@pricePre), tolerance = 1e-6,
                 info = paste(demand, "no-change price"))
    expect_equal(as.numeric(trade::calcMC(unchanged, TRUE)),
                 as.numeric(expected_mc),
                 tolerance = 1e-6, info = paste(demand, "no-change mc"))

    shocked <- suppressWarnings(suppressMessages(
      trade::simulate(promoted, tariffPost = tariff_post)
    ))
    expect_true(any(abs(shocked@quantityPost - source@model@quantityPre) > 1e-8),
                info = paste(demand, "shock quantity"))
    expect_equal(as.numeric(trade::calcMC(promoted@model, TRUE)),
                 as.numeric(expected_mc), info = paste(demand, "immutable physical costs"))
  }
})


test_that("quota promotion derives capacities from fitted quantities", {
  source <- .promotion_basic_fit("logit", "bertrand")
  quota_pre <- c(1.20, Inf, 1.50)
  promoted <- suppressWarnings(suppressMessages(
    trade::as_trade_fit(source, policy = "quota", quotaPre = quota_pre)
  ))
  fitted_quantities <- antitrust::calcQuantities(source@model, TRUE)
  expected_capacities <- quota_pre * fitted_quantities
  expected_capacities[is.infinite(quota_pre)] <- Inf

  expect_equal(promoted@model@quotaPre, quota_pre, tolerance = 0)
  expect_equal(promoted@model@quotaPost, quota_pre, tolerance = 0)
  expect_equal(unname(promoted@model@capacitiesPre), unname(expected_capacities), tolerance = 0)
  expect_equal(unname(promoted@model@capacitiesPost), unname(expected_capacities), tolerance = 0)
  expect_equal(unname(promoted@model@pricePre), unname(source@model@pricePre),
               tolerance = 1e-10)

  shocked <- suppressWarnings(suppressMessages(
    trade::simulate(promoted, quotaPost = c(.80, Inf, .75))
  ))
  expect_true(all(is.finite(shocked@pricePost)))
  expect_true(any(abs(shocked@pricePost - source@model@pricePre) > 1e-8))
})


test_that("quota promotion handles zero fitted output and infinite capacity", {
  source <- .promotion_basic_fit("logit", "bertrand")
  ## Retain one zero-output product while leaving the other fitted quantities
  ## positive.  This exercises both the below-one quota exception for zero
  ## output and the Inf * 0 capacity edge case.
  ## A market has one total size. Use a finite, extremely unattractive fitted
  ## quality to obtain machine-zero demand without making mktSize a vector.
  source@model@slopes$meanval[[1L]] <- -1000
  fitted_quantities <- antitrust::calcQuantities(source@model, TRUE)
  expect_true(fitted_quantities[[1L]] == 0)
  expect_true(all(fitted_quantities[-1L] > 0))

  quota_pre <- c(0, 1, Inf)
  promoted <- suppressWarnings(suppressMessages(
    trade::as_trade_fit(source, policy = "quota", quotaPre = quota_pre)
  ))
  expect_equal(promoted@model@quotaPre, quota_pre, tolerance = 0)
  expect_equal(promoted@model@capacitiesPre[[1L]], 0, tolerance = 0)
  expect_equal(promoted@model@capacitiesPre[[2L]],
               fitted_quantities[[2L]], tolerance = 0)
  expect_true(is.infinite(promoted@model@capacitiesPre[[3L]]))

  subunit <- trade::as_trade_fit(source, policy = "quota",
                                 quotaPre = c(.5, 1, Inf))
  expect_equal(subunit@model@capacitiesPre[[1L]], 0, tolerance = 0)
  unconstrained <- trade::as_trade_fit(source, policy = "quota",
                                       quotaPre = c(Inf, 1, 1))
  expect_true(is.infinite(unconstrained@model@capacitiesPre[[1L]]))
})


test_that("promotion validates policy-specific arguments", {
  source <- .promotion_basic_fit("logit", "bertrand")
  expect_error(
    trade::as_trade_fit(source, policy = "not-a-policy"),
    "unsupported policy|policy"
  )
  expect_error(
    trade::as_trade_fit(source, policy = "tariff", tariffPre = c(0, 0)),
    "tariffPre.*length|length-k"
  )
  named <- trade::as_trade_fit(
    source, policy = "tariff",
    tariffPre = setNames(rep(0, 3), .promotion_labels)
  )
  expect_equal(unname(named@model@tariffPre), rep(0, 3), tolerance = 0)
  scalar <- trade::as_trade_fit(source, policy = "tariff", tariffPre = 0)
  expect_equal(scalar@model@tariffPre, rep(0, 3), tolerance = 0)
  expect_error(
    trade::as_trade_fit(source, policy = "tariff",
                        tariffPre = 0, tariffPre = 0),
    "more than once|duplicate|once"
  )
  expect_error(
    trade::as_trade_fit(source, policy = "tariff", quotaPre = rep(Inf, 3)),
    "quotaPre|policy|not meaningful|unsupported"
  )

  quantity_source <- .promotion_quantity_fit("linear")
  expect_error(
    trade::as_trade_fit(quantity_source, policy = "tariff",
                        tariffPre = rep(0, 3)),
    "tariffPre.*matrix|dimensions|plant|matrix"
  )
  expect_error(
    trade::as_trade_fit(quantity_source, policy = "tariff",
                        tariffPre = matrix(0, nrow = 1, ncol = 1)),
    "tariffPre.*matrix|dimensions|3 x 1|plant"
  )
  expect_error(
    trade::as_trade_fit(quantity_source, policy = "tariff",
                        tariffPre = matrix(0, nrow = 3, ncol = 2)),
    "tariffPre.*matrix|dimensions|3 x 1|plant"
  )
  quantity_zero <- trade::as_trade_fit(quantity_source, policy = "tariff",
                                       tariffPre = 0)
  expect_equal(quantity_zero@model@tariffPre,
               matrix(0, nrow = 3, ncol = 1), tolerance = 0)
  quantity_matrix <- trade::as_trade_fit(
    quantity_source, policy = "tariff", tariffPre = matrix(0, 3, 1)
  )
  expect_equal(quantity_matrix@model@tariffPre,
               matrix(0, nrow = 3, ncol = 1), tolerance = 0)
  expect_error(
    trade::as_trade_fit(source, policy = "quota", tariffPre = rep(0, 3)),
    "tariffPre|quotaPre|policy|not meaningful|unsupported"
  )
  expect_error(
    trade::as_trade_fit(source, policy = "quota", quotaPre = c(.8, Inf, 1)),
    "quotaPre|below 1|greater than or equal to 1|quota"
  )
  expect_error(
    trade::as_trade_fit(source, policy = "quota", quotaPre = c(-.1, 1, Inf)),
    "quotaPre|non-negative|quota"
  )
  expect_error(
    trade::as_trade_fit(source, policy = "quota", quotaPre = rep(-Inf, 3)),
    "quotaPre|non-negative|quota"
  )
})
