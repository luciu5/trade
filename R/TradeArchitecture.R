#' A calibrated trade model
#'
#' `TradeFit` is a small wrapper around the existing trade S4 model object.
#' The wrapped object remains the implementation of the economic model; the
#' wrapper marks its calibrated baseline before a particular policy scenario.
#'
#' @name trade-architecture
NULL

#' @rdname trade-architecture
#' @export
setClass(
  "TradeFit",
  representation = representation(
    spec = "ANY",
    model = "ANY",
    parameters = "list",
    observed = "list",
    diagnostics = "list"
  ),
  prototype = list(
    spec = list(),
    parameters = list(),
    observed = list(),
    diagnostics = list()
  )
)

.trade_has_slot <- function(object, name) name %in% methods::slotNames(object)

.trade_slot <- function(object, name, default = NULL) {
  if (.trade_has_slot(object, name)) methods::slot(object, name) else default
}

.trade_n_products <- function(arguments) {
  candidates <- c("prices", "shares", "quantities")
  for (name in candidates) {
    if (!is.null(arguments[[name]])) {
      value <- arguments[[name]]
      if (is.matrix(value) && name == "quantities") return(ncol(value))
      return(length(value))
    }
  }
  NULL
}

.trade_policy_arguments <- c(
  "tariffPost", "quotaPost", "ownerPost", "mcDelta", "subset",
  "capacitiesPost", "productsPost", "mcfunPost", "vcfunPost",
  "bargpowerPost"
)

.trade_reject_post_arguments <- function(arguments) {
  supplied <- intersect(names(arguments), .trade_policy_arguments)
  if (length(supplied)) {
    stop(
      "post-policy argument(s) ", paste(supplied, collapse = ", "),
      " belong in simulate(), not calibrate() or specify()"
    )
  }
}

.trade_policy_pre <- function(arguments, policy, n) {
  if (policy == "tariff") {
    if (is.null(arguments$tariffPre)) rep(0, n) else arguments$tariffPre
  } else {
    if (is.null(arguments$quotaPre)) rep(Inf, n) else arguments$quotaPre
  }
}

.trade_baseline_arguments <- function(arguments, spec) {
  n <- .trade_n_products(arguments)
  if (is.null(n)) {
    stop("calibration requires prices, shares, or quantities")
  }

  pre <- .trade_policy_pre(arguments, spec$policy, n)
  if (spec$policy == "tariff" && spec$conduct == "cournot" &&
      spec$demand != "logit" && is.null(arguments$tariffPre)) {
    quantities <- arguments$quantities
    pre <- matrix(0, nrow = nrow(quantities), ncol = ncol(quantities))
  }
  if (spec$policy == "tariff") {
    arguments$tariffPre <- pre
    arguments$tariffPost <- pre
  } else {
    arguments$quotaPre <- pre
    arguments$quotaPost <- pre
  }
  arguments
}

.trade_legacy_function <- function(name) {
  get(name, envir = parent.env(environment()), inherits = FALSE)
}

.trade_capture_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
    },
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
    }
  )
  list(value = value, warnings = warnings, messages = messages)
}

.trade_parameters <- function(model) {
  result <- list()
  for (name in c("slopes", "intercepts", "bargpowerPre")) {
    if (.trade_has_slot(model, name)) result[[name]] <- methods::slot(model, name)
  }
  result
}

.trade_fit <- function(spec, model, arguments, conditions, route) {
  observed <- arguments
  observed$tariffPost <- NULL
  observed$quotaPost <- NULL
  observed$ownerPost <- NULL
  observed$mcDelta <- NULL
  observed$subset <- NULL
  observed$capacitiesPost <- NULL
  observed$productsPost <- NULL
  observed$mcfunPost <- NULL
  observed$vcfunPost <- NULL
  observed$bargpowerPost <- NULL

  new(
    "TradeFit",
    spec = spec,
    model = model,
    parameters = .trade_parameters(model),
    observed = observed,
    diagnostics = list(
      route = route,
      legacy_class = class(model)[1],
      warnings = conditions$warnings,
      messages = conditions$messages
    )
  )
}

.trade_calibrate_arguments <- function(spec, arguments) {
  arguments <- .trade_baseline_arguments(arguments, spec)

  if (spec$conduct == "cournot" && spec$demand == "logit") {
    # logit_cournot_tariff is the one legacy calibrator whose demand system is
    # implicit in the function name rather than a formal argument.
    if (spec$variant != "alm") arguments$calibration <- "diversion"
  } else if (spec$conduct == "cournot") {
    n <- .trade_n_products(arguments)
    arguments$demand <- rep(if (spec$demand == "loglin") "log" else "linear", n)
  } else {
    arguments$demand <- spec$demand
  }

  if (spec$variant == "alm" && spec$conduct == "cournot") {
    arguments$calibration <- "alm"
  }
  arguments
}

#' Calibrate a trade model from observed market data
#'
#' `calibrate()` dispatches to a complete registered legacy trade constructor.
#' It creates a baseline object with the pre-policy state in both policy
#' slots; post-policy changes are applied by [simulate()].
#'
#' @param demand A demand-system name or a `trade_model_spec` object.
#' @param conduct A conduct name when `demand` is not a specification object.
#' @param variant An implemented model variant.
#' @param policy The baseline policy family, currently `"tariff"` or
#'   `"quota"`.
#' @param ... Baseline inputs accepted by the selected legacy constructor.
#' @return A `TradeFit` object.
#' @rdname trade-architecture
#' @export
calibrate <- function(demand, conduct = NULL, variant = "standard",
                      policy = "tariff", ...) {
  if (inherits(demand, "trade_model_spec")) {
    if (!is.null(conduct)) stop("'conduct' must be omitted when using a model specification")
    spec <- demand
  } else {
    spec <- model_spec(demand, conduct, variant = variant, policy = policy)
  }

  entry <- .trade_registry_entry(spec)
  if (!isTRUE(entry$calibrate)) {
    stop("calibrate() is not supported for trade model '", spec$id, "'")
  }

  arguments <- list(...)
  .trade_reject_post_arguments(arguments)
  arguments <- .trade_calibrate_arguments(spec, arguments)
  calibrator <- .trade_legacy_function(entry$legacy_calibrator)
  captured <- .trade_capture_conditions(do.call(calibrator, arguments))

  .trade_fit(spec, captured$value, arguments, captured, route = "calibrate")
}

.trade_specify_arguments <- function(spec, prices, parameters, arguments) {
  if (!is.list(parameters) || is.null(names(parameters)) ||
      any(!nzchar(names(parameters)))) {
    stop("'parameters' must be a named list of structural parameters")
  }
  if (spec$policy != "tariff") {
    stop("specify() is not supported for policy '", spec$policy, "'")
  }

  n <- length(prices)
  pre <- if (is.null(arguments$tariffPre)) rep(0, n) else arguments$tariffPre
  arguments$prices <- prices
  arguments$demand <- spec$demand
  arguments$supply <- spec$conduct
  arguments$demand.param <- parameters
  arguments$tariffPre <- pre
  arguments$tariffPost <- pre
  arguments
}

#' Load a trade model from supplied structural parameters
#'
#' This route uses the legacy `sim()` parameterized construction path where it
#' exists. Optimizer starting values are not reinterpreted as supplied
#' structural parameters.
#'
#' @param demand A demand-system name or a `trade_model_spec` object.
#' @param conduct A conduct name when `demand` is not a specification object.
#' @param prices A length-kk vector of observed prices.
#' @param parameters A named list of supplied structural parameters.
#' @param variant An implemented model variant.
#' @param policy The policy family; supplied-parameter construction currently
#'   supports tariff paths only.
#' @param ... Additional baseline arguments accepted by `sim()`.
#' @return A `TradeFit` object.
#' @rdname trade-architecture
#' @export
specify <- function(demand, conduct = NULL, prices, parameters,
                    variant = "standard", policy = "tariff", ...) {
  if (inherits(demand, "trade_model_spec")) {
    if (!is.null(conduct)) stop("'conduct' must be omitted when using a model specification")
    spec <- demand
  } else {
    spec <- model_spec(demand, conduct, variant = variant, policy = policy)
  }

  entry <- .trade_registry_entry(spec)
  if (!isTRUE(entry$specify)) {
    stop("specify() is not supported for trade model '", spec$id, "'")
  }

  arguments <- list(...)
  .trade_reject_post_arguments(arguments)
  arguments <- .trade_specify_arguments(spec, prices, parameters, arguments)
  captured <- .trade_capture_conditions(do.call(sim, arguments))

  .trade_fit(spec, captured$value, arguments, captured, route = "specify")
}

.trade_normalize_policy_vector <- function(value, n, name, quota = FALSE) {
  if (length(value) != n) stop("'", name, "' must be a length-k vector")
  if (quota) {
    value[is.na(value)] <- Inf
  } else {
    value[is.na(value)] <- 0
    if (any(value >= 1, na.rm = TRUE)) stop("'", name, "' must be less than 1")
  }
  value
}

.trade_normalize_policy_matrix <- function(value, dims, name) {
  if (!isTRUE(all.equal(dim(value), dims))) {
    stop("'", name, "' must have the same dimensions as the fitted tariff matrix")
  }
  value[is.na(value)] <- 0
  value
}

.trade_raw_owner <- function(fit, model) {
  if (!is.null(fit@observed$owner)) return(fit@observed$owner)
  model@ownerPre
}

.trade_scaled_tariff_owner <- function(model) {
  is(model, "TariffLogit") || is(model, "TariffCES") ||
    is(model, "TariffAIDS") || is(model, "TariffLogitCournotModels") ||
    is(model, "TariffBargainingLogit") || is(model, "TariffBargainingCES")
}

.trade_recalculate_tariff <- function(fit, tariffPost, subset, priceStart,
                                      isMax, arguments) {
  model <- fit@model
  n <- length(model@shares)
  tariffPre <- model@tariffPre
  tariffPost <- .trade_normalize_policy_vector(tariffPost, n, "tariffPost")

  model@tariffPost <- tariffPost
  if (.trade_scaled_tariff_owner(model)) {
    owner <- .owner_to_matrix(.trade_raw_owner(fit, model), n,
                              "'owner' must be supplied as a length-k vector or k x k ownership matrix")
    model@ownerPost <- .apply_tariff_to_owner(owner, tariffPost)
  }

  delta <- .tariff_mc_delta(tariffPre, tariffPost)
  if (is(model, "Tariff2ndLogit")) {
    model@mcDelta <- model@mcPre * delta
  } else if (is(model, "TariffBargainingLogit") ||
             is(model, "TariffBargainingCES")) {
    model@mcDelta <- model@mcPre * delta
  } else {
    model@mcDelta <- delta
  }
  model@subset <- subset
  if (!is.null(priceStart)) model@priceStart <- priceStart
  model@mcPost <- calcMC(model, preMerger = FALSE)

  if (is(model, "Tariff2ndLogit")) {
    model@pricePost <- calcPrices(model, preMerger = FALSE)
  } else {
    price_arguments <- c(
      list(object = model, preMerger = FALSE, subset = subset, isMax = isMax),
      arguments
    )
    model@pricePost <- do.call(calcPrices, price_arguments)
  }
  model
}

.trade_recalculate_quota <- function(fit, quotaPost, subset, priceStart,
                                     isMax, arguments) {
  model <- fit@model
  n <- length(model@shares)
  quotaPost <- .trade_normalize_policy_vector(quotaPost, n, "quotaPost", quota = TRUE)
  model@quotaPost <- quotaPost

  if (is(model, "QuotaLogit")) {
    model@capacitiesPost <- quotaPost * model@shares * model@insideSize
  }
  model@subset <- subset
  if (!is.null(priceStart)) model@priceStart <- priceStart
  model@mcPost <- calcMC(model, preMerger = FALSE)
  price_arguments <- c(
    list(object = model, preMerger = FALSE, subset = subset, isMax = isMax),
    arguments
  )
  model@pricePost <- do.call(calcPrices, price_arguments)
  model
}

.trade_recalculate_cournot <- function(fit, tariffPost, subset, arguments) {
  model <- fit@model
  dims <- dim(model@tariffPre)
  tariffPost <- .trade_normalize_policy_matrix(tariffPost, dims, "tariffPost")
  model@tariffPost <- tariffPost
  model@subset <- subset

  if (!is.null(arguments$capacitiesPost)) {
    model@capacitiesPost <- arguments$capacitiesPost
  }
  if (!is.null(arguments$productsPost)) {
    model@productsPost <- arguments$productsPost
  }
  if (!is.null(arguments$quantityStart)) {
    model@quantityStart <- arguments$quantityStart
  }
  model@quantityPost <- calcQuantities(model, preMerger = FALSE)
  model@mcPost <- calcMC(model, preMerger = FALSE)
  model@pricePost <- calcPrices(model, preMerger = FALSE)
  model
}

#' Simulate a policy counterfactual from a fitted trade model
#'
#' The fitted baseline is cloned and the selected legacy policy-aware methods
#' are called for one post-policy equilibrium. A fit can therefore be reused
#' for repeated tariff or quota scenarios without recalibration.
#'
#' @param fit A `TradeFit` returned by [calibrate()] or [specify()].
#' @param tariffPost A post-policy ad valorem tariff vector for tariff models.
#' @param quotaPost A post-policy quota vector for quota models.
#' @param subset A logical vector selecting products in the post-policy market.
#' @param priceStart Optional price starting values for the post-policy solve.
#' @param isMax Passed to legacy price solvers where supported.
#' @param ... Additional model-specific solver or post-state arguments.
#' @return The existing trade S4 result object, not a new result hierarchy.
#' @rdname trade-architecture
#' @export
simulate <- function(fit, tariffPost, quotaPost, subset, priceStart,
                     isMax = FALSE, ...) {
  if (!is(fit, "TradeFit")) stop("'fit' must be a TradeFit object")
  spec <- fit@spec
  entry <- .trade_registry_entry(spec)
  if (!isTRUE(entry$simulate)) {
    stop("simulate() is not supported for trade model '", spec$id, "'")
  }

  arguments <- list(...)
  if (any(c("tariffPre", "quotaPre", "ownerPost", "mcDelta") %in% names(arguments))) {
    bad <- intersect(names(arguments), c("tariffPre", "quotaPre", "ownerPost", "mcDelta"))
    stop("scenario argument(s) ", paste(bad, collapse = ", "),
         " cannot replace the fitted baseline")
  }

  model <- fit@model
  n <- length(model@shares)
  if (missing(subset)) subset <- rep(TRUE, n)
  if (!is.logical(subset) || length(subset) != n || !any(subset)) {
    stop("'subset' must be a logical vector the same length as the fitted products with at least one TRUE value")
  }

  if (spec$policy == "tariff") {
    if (!missing(quotaPost)) stop("'quotaPost' is not supported by a tariff fit")
    if (missing(tariffPost)) tariffPost <- model@tariffPre

    if (is(model, "TariffCournot")) {
      .trade_recalculate_cournot(fit, tariffPost, subset, arguments)
    } else {
      .trade_recalculate_tariff(fit, tariffPost, subset,
                                if (missing(priceStart)) NULL else priceStart,
                                isMax, arguments)
    }
  } else {
    if (!missing(tariffPost)) stop("'tariffPost' is not supported by a quota fit")
    if (missing(quotaPost)) quotaPost <- model@quotaPre
    .trade_recalculate_quota(fit, quotaPost, subset,
                             if (missing(priceStart)) NULL else priceStart,
                             isMax, arguments)
  }
}
