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

.trade_fit <- function(spec, model, arguments, conditions, route,
                       calibration_args = NULL, specification_args = NULL) {
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
      calibration_args = calibration_args,
      specification_args = specification_args,
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
  calibration_args <- c(
    list(demand = spec$demand, conduct = spec$conduct,
         variant = spec$variant, policy = spec$policy),
    arguments
  )
  .trade_reject_post_arguments(arguments)
  arguments <- .trade_calibrate_arguments(spec, arguments)
  calibrator <- .trade_legacy_function(entry$legacy_calibrator)
  captured <- .trade_capture_conditions(do.call(calibrator, arguments))

  .trade_fit(spec, captured$value, arguments, captured, route = "calibrate",
             calibration_args = calibration_args)
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
  specification_args <- c(
    list(demand = spec$demand, conduct = spec$conduct,
         variant = spec$variant, policy = spec$policy,
         prices = prices, parameters = parameters),
    arguments
  )
  .trade_reject_post_arguments(arguments)
  arguments <- .trade_specify_arguments(spec, prices, parameters, arguments)
  captured <- .trade_capture_conditions(do.call(.sim_legacy, arguments))

  .trade_fit(spec, captured$value, arguments, captured, route = "specify",
             specification_args = specification_args)
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
                                      bargpowerPost, isMax, arguments) {
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
    # Preserve the legacy auction sequence: the initial object stores the
    # tariff ratio, calculates mcPost, and only then replaces mcDelta with the
    # level change used by the post-policy price calculation.
    model@mcDelta <- delta
    model@mcPost <- calcMC(model, preMerger = FALSE)
    model@mcDelta <- model@mcPre * delta
  } else if (is(model, "TariffBargainingLogit") ||
             is(model, "TariffBargainingCES")) {
    # bargaining_tariff has the same historical two-step assignment.
    model@mcDelta <- delta
    model@mcPost <- calcMC(model, preMerger = FALSE)
    model@mcDelta <- model@mcPre * delta
  } else {
    model@mcDelta <- delta
    model@mcPost <- calcMC(model, preMerger = FALSE)
  }
  model@subset <- subset
  if (!is.null(bargpowerPost) &&
      (is(model, "TariffBargainingLogit") || is(model, "TariffBargainingCES"))) {
    model@bargpowerPost <- bargpowerPost
  }
  if (!is.null(priceStart)) model@priceStart <- priceStart
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
#' @param tariffPost A post-policy ad valorem tariff vector for tariff models,
#' or a `Counterfactual` object.
#' @param quotaPost A post-policy quota vector for quota models.
#' @param subset A logical vector selecting products in the post-policy market.
#' @param priceStart Optional price starting values for the post-policy solve.
#' @param isMax Passed to legacy price solvers where supported.
#' @param ... Additional model-specific solver or post-state arguments.
#' @return The existing trade S4 result object, not a new result hierarchy.
#' @rdname trade-architecture
#' @export
simulate <- function(fit, tariffPost = NULL, quotaPost = NULL, subset = NULL,
                     priceStart = NULL, bargpowerPost = NULL,
                     isMax = FALSE, ...) {
  if (!is(fit, "TradeFit")) stop("'fit' must be a TradeFit object")
  spec <- fit@spec
  entry <- .trade_registry_entry(spec)
  if (!isTRUE(entry$simulate)) {
    stop("simulate() is not supported for trade model '", spec$id, "'")
  }

  arguments <- list(...)
  cf <- if (inherits(tariffPost, "Counterfactual")) tariffPost else NULL
  if (!is.null(cf)) {
    .validate_counterfactual(cf, spec)
    conflicts <- c(
      if (!is.null(quotaPost)) "quotaPost",
      if (!is.null(subset)) "subset",
      if (!is.null(priceStart)) "priceStart",
      if (!is.null(bargpowerPost)) "bargpowerPost",
      if (length(arguments)) names(arguments)
    )
    if (length(conflicts)) {
      stop("cannot combine a Counterfactual with legacy scenario argument(s): ",
           paste(unique(conflicts), collapse = ", "))
    }
    tariffPost <- cf$tariff
    quotaPost <- cf$quota
    exit <- cf$exit
    if (!is.null(cf$products)) arguments$productsPost <- cf$products
  } else {
    exit <- NULL
    fields <- list(tariff = tariffPost, quota = quotaPost, exit = subset)
    fields <- fields[!vapply(fields, is.null, logical(1))]
    cf <- do.call(counterfactual, fields)
  }
  if (any(c("tariffPre", "quotaPre", "ownerPost", "mcDelta") %in% names(arguments))) {
    bad <- intersect(names(arguments), c("tariffPre", "quotaPre", "ownerPost", "mcDelta"))
    stop("scenario argument(s) ", paste(bad, collapse = ", "),
         " cannot replace the fitted baseline")
  }

  model <- fit@model
  n <- length(model@shares)
  if (!is.null(exit)) subset <- .counterfactual_subset(
    exit, n, .trade_slot(model, "labels")
  )
  if (is.null(subset)) subset <- rep(TRUE, n)
  if (!is.logical(subset) || length(subset) != n || !any(subset)) {
    stop("'subset' must be a logical vector the same length as the fitted products with at least one TRUE value")
  }

  if (spec$policy == "tariff") {
    if (!is.null(quotaPost)) stop("'quotaPost' is not supported by a tariff fit")
    if (is.null(tariffPost)) tariffPost <- model@tariffPre

    if (is(model, "TariffCournot")) {
      result <- .trade_recalculate_cournot(fit, tariffPost, subset, arguments)
    } else {
      result <- .trade_recalculate_tariff(fit, tariffPost, subset,
                                          priceStart, bargpowerPost,
                                          isMax, arguments)
    }
  } else {
    if (!is.null(tariffPost)) stop("'tariffPost' is not supported by a quota fit")
    if (is.null(quotaPost)) quotaPost <- model@quotaPre
    result <- .trade_recalculate_quota(fit, quotaPost, subset, priceStart,
                                       isMax, arguments)
  }
  .counterfactual_attach(result, fit, cf)
}


#' Recalibrate a fitted trade model
#'
#' `update()` rebuilds the stored baseline call and invokes `calibrate()`
#' again. Post-policy arguments remain simulation-only.
#'
#' @param object A `TradeFit` returned by `calibrate()`.
#' @param ... Baseline data, model-specification arguments, or model-specific
#'   calibration options to replace.
#' @param evaluate If `FALSE`, return the reconstructed calibration call.
#' @return A newly calibrated `TradeFit`, or a call when `evaluate` is
#'   `FALSE`.
#' @rdname trade-architecture
#' @export
#' @exportS3Method stats::update TradeFit
update.TradeFit <- function(object, ..., evaluate = TRUE) {
  if (!is(object, "TradeFit")) {
    stop("'object' must be a TradeFit returned by calibrate()")
  }
  calibration_args <- object@diagnostics$calibration_args
  if (!is.list(calibration_args) || is.null(names(calibration_args))) {
    stop("this fit does not retain a calibration call; update() requires a fit created by calibrate()")
  }

  replacements <- list(...)
  if (length(replacements)) {
    if (is.null(names(replacements)) || any(!nzchar(names(replacements)))) {
      stop("update() arguments must be named calibration or model-specification arguments")
    }
    calibration_args[names(replacements)] <- replacements
  }

  target <- model_spec(
    calibration_args$demand,
    calibration_args$conduct,
    variant = if (is.null(calibration_args$variant)) {
      "standard"
    } else {
      calibration_args$variant
    },
    policy = if (is.null(calibration_args$policy)) {
      "tariff"
    } else {
      calibration_args$policy
    }
  )
  if (target$conduct == "moncom") calibration_args$owner <- NULL
  if (target$policy == "quota") {
    calibration_args$tariffPre <- NULL
  } else {
    calibration_args$quotaPre <- NULL
  }

  if (!isTRUE(evaluate)) {
    return(as.call(c(list(quote(calibrate)), calibration_args)))
  }
  do.call(calibrate, calibration_args)
}


.trade_structural_parameters <- function(fit) {
  model <- fit@model
  if (.trade_has_slot(model, "slopes") && is.list(methods::slot(model, "slopes"))) {
    return(methods::slot(model, "slopes"))
  }
  if (is.list(fit@parameters) && !is.null(fit@parameters$slopes) &&
      is.list(fit@parameters$slopes)) {
    return(fit@parameters$slopes)
  }
  stop("source fit does not expose portable demand parameters")
}

.trade_respecify_arguments <- function(fit, target, parameters) {
  model <- fit@model
  observed <- fit@observed
  owner <- observed$owner
  if (is.null(owner)) owner <- .trade_slot(model, "ownerPre")
  if (is.null(owner)) {
    stop("source fit does not retain the ownership input needed for respecify()")
  }
  prices <- observed$prices
  if (is.null(prices)) prices <- .trade_slot(model, "prices")
  if (is.null(prices)) {
    stop("source fit does not retain prices needed for respecify()")
  }

  arguments <- list(
    demand = target$demand,
    conduct = target$conduct,
    variant = target$variant,
    policy = target$policy,
    prices = prices,
    parameters = parameters,
    owner = owner
  )
  for (name in c("tariffPre", "insideSize", "priceOutside", "labels")) {
    value <- observed[[name]]
    if (is.null(value)) value <- .trade_slot(model, name)
    if (!is.null(value)) arguments[[name]] <- value
  }
  arguments
}

#' Respecify a fitted trade model
#'
#' Only transitions with a complete supplied-parameter path are permitted.
#' Same-demand primitives are retained and the target conduct state is
#' reconstructed through `specify()`. Registered flat Logit/CES transitions
#' instead translate the demand locally by matching baseline shares and
#' minimizing baseline elasticity distance; source margins are not used to
#' recalibrate translated demand. This is not a global equivalence claim
#' between price-level Logit and log-price CES.
#'
#' @param fit A `TradeFit` returned by `calibrate()` or `specify()`.
#' @param demand Optional target demand-system name.
#' @param conduct Optional target conduct name.
#' @param variant Optional target model variant.
#' @param ... Reserved for future transition-specific options.
#' @return A newly constructed `TradeFit` under the target specification.
#' @seealso [`specify()`], [`update.TradeFit()`]
#' @rdname trade-architecture
#' @export
respecify <- function(fit, demand = NULL, conduct = NULL,
                      variant = NULL, ...) {
  if (!is(fit, "TradeFit")) {
    stop("'fit' must be a TradeFit returned by calibrate() or specify()")
  }
  if (length(list(...))) {
    stop("respecify() does not accept transition-specific arguments yet")
  }

  source <- fit@spec
  target <- model_spec(
    demand = if (is.null(demand)) source$demand else demand,
    conduct = if (is.null(conduct)) source$conduct else conduct,
    variant = if (is.null(variant)) source$variant else variant,
    policy = source$policy
  )
  if (identical(source$id, target$id)) {
    stop("respecify() requires a different registered model specification")
  }
  transition <- .trade_transition_entry(source, target)

  if (identical(transition$kind, "local-demand-translation")) {
    translated <- .translate_trade_demand(fit, target)
    result <- translated$fit
    result@parameters <- .trade_parameters(result@model)
    result@observed <- fit@observed
    result@observed$demand <- target$demand
    result@diagnostics$source <- "respecify"
    result@diagnostics$route <- "respecify"
    result@diagnostics$transition <- list(
      from = source$id,
      to = target$id,
      kind = transition$kind,
      retained = transition$retain,
      recomputed = transition$recompute,
      invalidated = transition$invalidate,
      calibration_required = transition$calibration_required
    )
    result@diagnostics$local_translation <- translated$diagnostics
    result@diagnostics$source_calibration_args <-
      fit@diagnostics$calibration_args
    if (is.list(fit@diagnostics$calibration_args)) {
      target_calibration <- fit@diagnostics$calibration_args
      target_calibration$demand <- target$demand
      target_calibration$conduct <- target$conduct
      target_calibration$variant <- target$variant
      target_calibration$policy <- target$policy
      target_calibration$tariffPre <- translated$state$model@tariffPre
      target_calibration$tariffPost <- NULL
      target_calibration$quotaPre <- NULL
      result@diagnostics$calibration_args <- target_calibration
    }
    return(result)
  }

  parameters <- .trade_structural_parameters(fit)
  missing_parameters <- setdiff(transition$retain, names(parameters))
  if (length(missing_parameters)) {
    stop("source fit does not contain portable parameter(s): ",
         paste(missing_parameters, collapse = ", "))
  }
  parameters <- parameters[transition$retain]

  result <- do.call(specify, .trade_respecify_arguments(
    fit, target, parameters
  ))
  result@parameters <- .trade_parameters(result@model)
  result@observed <- fit@observed
  result@diagnostics$source <- "respecify"
  result@diagnostics$route <- "respecify"
  result@diagnostics$transition <- list(
    from = source$id,
    to = target$id,
    retained = transition$retain,
    recomputed = transition$recompute,
    invalidated = transition$invalidate,
    calibration_required = transition$calibration_required
  )
  if (is.list(fit@diagnostics$calibration_args)) {
    result@diagnostics$calibration_args <- fit@diagnostics$calibration_args
    result@diagnostics$calibration_args$demand <- target$demand
    result@diagnostics$calibration_args$conduct <- target$conduct
    result@diagnostics$calibration_args$variant <- target$variant
    result@diagnostics$calibration_args$policy <- target$policy
  }
  result
}
