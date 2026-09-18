# Firm-uniform consumer-price tariff reuse for fitted output games.
#
# This adapter is deliberately separate from the historical Tariff* classes.
# A TariffGameFit retains the fitted effective-cost game and keeps production
# costs in an explicit policy state.  Promotion only copies state; all later
# equilibrium work is delegated to the public coordination or antitrust
# simulator.

#' Tariff reuse fit
#'
#' A `TariffGameFit` is a `TradeFit` wrapper for a fitted output-market game
#' whose saved marginal costs are effective costs.  The wrapper records the
#' consumer-price tariff fraction, recovered physical costs, and the immutable
#' source fit used for subsequent scenarios.
#'
#' @export
#' @importClassesFrom antitrust StructuralFit
#' @importClassesFrom coordination StackelbergLogit StackelbergCES
#'   CoreFringeLogit CoreFringeCES
setClass(
  "TariffGameFit",
  contains = "TradeFit",
  representation = representation(
    tariff_state = "list",
    source_fit = "ANY"
  ),
  prototype = prototype(tariff_state = list(), source_fit = NULL)
)

.trade_game_error <- function(type, message) {
  condition <- simpleError(message)
  class(condition) <- c(type, "trade_tariff_error", class(condition))
  stop(condition)
}

.trade_game_has <- function(object, name) name %in% methods::slotNames(object)

.trade_game_slot <- function(object, name, default = NULL) {
  if (.trade_game_has(object, name)) methods::slot(object, name) else default
}

.trade_game_validate_ownership_matrix <- function(object, preMerger = TRUE) {
  field <- if (preMerger) "ownerPre" else "ownerPost"
  value <- .trade_game_slot(object, field)
  if (is.null(value) || !is.matrix(value)) return(invisible(TRUE))
  ## antitrust ownership matrices are product-by-product block matrices.  A
  ## fractional or non-block matrix changes the ordinary game's incentives;
  ## converting it to firm IDs here would silently change that game.
  if (nrow(value) != ncol(value) || any(!is.finite(value)) ||
      any(abs(value - round(value)) > 1e-10) ||
      any(!value %in% c(0, 1))) {
    .trade_game_error(
      "trade_tariff_unsupported_ownership",
      "tariff reuse requires a binary full-firm ownership matrix"
    )
  }
  binary <- value > 0.5
  if (!all(diag(binary)) || !isTRUE(all(binary == t(binary)))) {
    .trade_game_error(
      "trade_tariff_unsupported_ownership",
      "tariff reuse requires a symmetric block ownership matrix"
    )
  }
  ## A block ownership matrix is an equivalence relation.  Equal rows identify
  ## candidate firms; each row must contain exactly that firm's number of
  ## products.  With the symmetry and diagonal checks above, this is an O(k^2)
  ## block check and avoids a cubic transitivity scan.
  row_key <- apply(binary, 1L, paste, collapse = ":")
  group <- match(row_key, unique(row_key))
  sizes <- tabulate(group, nbins = max(group))
  if (any(rowSums(binary) != sizes[group])) {
    .trade_game_error(
      "trade_tariff_unsupported_ownership",
      "tariff reuse requires a block ownership matrix with full-firm ownership"
    )
  }
  invisible(TRUE)
}

.trade_game_native <- function(object) {
  methods::is(object, "StackelbergLogit") ||
    methods::is(object, "StackelbergCES") ||
    methods::is(object, "CoreFringeLogit") ||
    methods::is(object, "CoreFringeCES")
}

.trade_game_native_fit <- function(object) {
  if (methods::is(object, "TariffGameFit")) {
    return(.trade_game_native(object@tariff_state$source_model))
  }
  .trade_game_native(object)
}

.trade_game_is_stack <- function(object) {
  source <- if (methods::is(object, "TariffGameFit")) {
    object@tariff_state$source_model
  } else object
  methods::is(source, "StackelbergLogit") || methods::is(source, "StackelbergCES")
}

.trade_game_is_core <- function(object) {
  source <- if (methods::is(object, "TariffGameFit")) {
    object@tariff_state$source_model
  } else object
  methods::is(source, "CoreFringeLogit") || methods::is(source, "CoreFringeCES")
}

.trade_game_native_class <- function(object) {
  if (methods::is(object, "StackelbergLogit")) return("StackelbergLogit")
  if (methods::is(object, "StackelbergCES")) return("StackelbergCES")
  if (methods::is(object, "CoreFringeLogit")) return("CoreFringeLogit")
  if (methods::is(object, "CoreFringeCES")) return("CoreFringeCES")
  NULL
}

.trade_game_demand <- function(object) {
  if (methods::is(object, "CES")) "ces" else "logit"
}

.trade_game_n <- function(object) {
  for (name in c("shares", "prices", "labels")) {
    value <- .trade_game_slot(object, name)
    if (!is.null(value)) return(length(value))
  }
  .trade_game_error("trade_tariff_invalid_model",
                    "the fitted game does not expose a product dimension")
}

.trade_game_labels <- function(object, n = .trade_game_n(object)) {
  labels <- .trade_game_slot(object, "labels")
  if (is.null(labels)) labels <- names(.trade_game_slot(object, "prices"))
  if (is.null(labels) || length(labels) != n || anyNA(labels) ||
      any(!nzchar(as.character(labels)))) labels <- paste0("Prod", seq_len(n))
  labels <- as.character(labels)
  if (anyDuplicated(labels)) {
    .trade_game_error("trade_tariff_invalid_model",
                      "product labels must be unique for tariff reuse")
  }
  labels
}

.trade_game_prices <- function(object, preMerger = TRUE) {
  name <- if (preMerger) "pricePre" else "pricePost"
  value <- .trade_game_slot(object, name)
  if (is.null(value) || !length(value)) value <- .trade_game_slot(object, "prices")
  as.numeric(value)
}

.trade_game_owner <- function(object, preMerger = TRUE) {
  field <- if (preMerger) "firmOwnerPre" else "firmOwnerPost"
  value <- .trade_game_slot(object, field)
  if (!is.null(value)) return(as.character(value))
  .trade_game_validate_ownership_matrix(object, preMerger)
  ## Coordination and newer antitrust classes expose ownerToVec() publicly;
  ## this fallback also handles older matrix-only model objects.
  value <- try(antitrust::ownerToVec(object, preMerger = preMerger), silent = TRUE)
  if (!inherits(value, "try-error") && !is.matrix(value) && length(value)) {
    return(as.character(value))
  }
  field <- if (preMerger) "ownerPre" else "ownerPost"
  value <- .trade_game_slot(object, field)
  if (is.null(value)) {
    .trade_game_error("trade_tariff_invalid_model",
                      "the fitted game does not expose product ownership")
  }
  if (!is.matrix(value)) return(as.character(value))
  ## Binary ownership matrices are sufficient for ordinary antitrust fits.
  key <- apply(value, 1L, function(row) paste(which(row > 0), collapse = ":"))
  match(key, unique(key))
}

.trade_game_roles <- function(object, preMerger = TRUE) {
  leaders <- .trade_game_slot(object, if (preMerger) "leadersPre" else "leadersPost")
  core <- .trade_game_slot(object, if (preMerger) "corePre" else "corePost")
  list(
    leaders = if (is.null(leaders)) character() else as.character(leaders),
    core = if (is.null(core)) character() else as.character(core)
  )
}

.trade_game_normalize_tariff <- function(value, n, labels, name = "tariffPre") {
  if (is.null(value)) value <- 0
  if (is.matrix(value) || is.array(value) || is.list(value) || !is.numeric(value)) {
    .trade_game_error("trade_tariff_invalid_policy",
                      paste0("'", name, "' must be a finite numeric scalar or length-k vector"))
  }
  value_names <- names(value)
  if (!is.null(value_names) && length(value_names)) {
    if (length(value) != n || any(is.na(value_names)) || any(!nzchar(value_names)) ||
        anyDuplicated(value_names) || any(!value_names %in% labels)) {
      .trade_game_error("trade_tariff_invalid_policy",
                        paste0("named '", name,
                               "' values must name every product exactly once"))
    }
    value <- as.numeric(value[match(labels, value_names)])
  } else if (length(value) == 1L) {
    value <- rep(value, n)
  }
  if (length(value) != n || any(!is.finite(value)) || any(value >= 1)) {
    .trade_game_error("trade_tariff_invalid_policy",
                      paste0("'", name, "' must be finite and less than one"))
  }
  stats::setNames(as.numeric(value), labels)
}

.trade_game_normalize_delta <- function(value, n, labels) {
  if (is.null(value)) return(stats::setNames(rep(0, n), labels))
  if (is.matrix(value) || is.array(value) || is.list(value) || !is.numeric(value)) {
    .trade_game_error("trade_tariff_invalid_cost_shock",
                      "'mcDelta' must be a finite physical proportional cost shock")
  }
  value_names <- names(value)
  if (!is.null(value_names) && length(value_names)) {
    if (any(is.na(value_names)) || any(!nzchar(value_names)) ||
        anyDuplicated(value_names) || any(!value_names %in% labels)) {
      .trade_game_error("trade_tariff_invalid_cost_shock",
                        "named 'mcDelta' values must use unique product labels")
    }
    out <- stats::setNames(rep(0, n), labels)
    out[value_names] <- value
    value <- out
  } else if (length(value) == 1L) {
    value <- rep(value, n)
  }
  if (length(value) != n || any(!is.finite(value)) || any(value <= -1)) {
    .trade_game_error("trade_tariff_invalid_cost_shock",
                      "'mcDelta' must be finite and greater than -1")
  }
  stats::setNames(as.numeric(value), labels)
}

.trade_game_normalize_owner <- function(value, default, n, name = "ownerPost") {
  if (is.null(value)) return(as.character(default))
  if (is.matrix(value) || is.array(value) || is.list(value) || length(value) != n) {
    .trade_game_error("trade_tariff_invalid_ownership",
                      paste0("'", name, "' must be a length-k firm-ID vector"))
  }
  value <- as.character(value)
  if (anyNA(value) || any(!nzchar(value))) {
    .trade_game_error("trade_tariff_invalid_ownership",
                      paste0("'", name, "' must contain non-empty firm IDs"))
  }
  value
}

.trade_game_uniform_tariff <- function(tariff, owner, when = "post-policy") {
  for (firm in unique(owner)) {
    values <- 1 - tariff[owner == firm]
    relative_spread <- if (length(values)) {
      (max(values) - min(values)) / max(values)
    } else 0
    if (length(values) && relative_spread > 1e-10) {
      .trade_game_error(
        "trade_tariff_heterogeneous",
        paste0("the ", when, " tariff is heterogeneous within firm '", firm,
               "'; the initial tariff reuse route requires firm-uniform retention")
      )
    }
  }
  invisible(TRUE)
}

.trade_game_copy_attributes <- function(source, target) {
  source_attributes <- attributes(source)
  ignored <- c(methods::slotNames(source), "class")
  for (name in setdiff(names(source_attributes), ignored)) {
    attr(target, name) <- source_attributes[[name]]
  }
  target
}

.trade_game_baseline_model <- function(model) {
  out <- model
  n <- .trade_game_n(out)
  ## Native coordination roles and ordinary antitrust ownership use different
  ## representations; each is copied only when its slot exists.
  for (pair in list(c("firmOwnerPre", "firmOwnerPost"),
                    c("ownerPre", "ownerPost"),
                    c("leadersPre", "leadersPost"),
                    c("corePre", "corePost"),
                    c("pricePre", "pricePost"),
                    c("mcPre", "mcPost"))) {
    if (.trade_game_has(out, pair[[1L]]) && .trade_game_has(out, pair[[2L]])) {
      methods::slot(out, pair[[2L]]) <- methods::slot(out, pair[[1L]])
    }
  }
  if (.trade_game_has(out, "mcDelta")) out@mcDelta <- rep(0, n)
  if (.trade_game_has(out, "subset")) out@subset <- rep(TRUE, n)
  out
}

.trade_game_source_costs <- function(model) {
  value <- try(antitrust::calcMC(model, preMerger = TRUE), silent = TRUE)
  if (inherits(value, "try-error")) {
    .trade_game_error("trade_tariff_invalid_cost_state",
                      "the fitted game does not expose effective baseline costs through calcMC()")
  }
  value <- as.numeric(value)
  n <- .trade_game_n(model)
  if (length(value) != n || any(!is.finite(value)) || any(value <= 0)) {
    .trade_game_error("trade_tariff_invalid_cost_state",
                      "effective baseline costs must be finite and positive")
  }
  value
}

.trade_game_validate_output <- function(model) {
  output <- .trade_game_slot(model, "output", TRUE)
  if (length(output) != 1L || !isTRUE(output)) {
    .trade_game_error("trade_tariff_unsupported_market_side",
                      "tariff reuse is initially supported only for output-market games")
  }
}

.trade_game_parse_contract <- function(arguments, tau, native = FALSE) {
  supplied <- names(arguments)
  if (length(arguments) &&
      (is.null(supplied) || any(is.na(supplied)) || any(!nzchar(supplied)) ||
       anyDuplicated(supplied))) {
    .trade_game_error("trade_tariff_invalid_contract",
                      "tariff reuse arguments must be named")
  }
  allowed <- c("tariffPre", "cost_basis", "margin_basis")
  bad <- setdiff(supplied, allowed)
  if (length(bad)) {
    .trade_game_error("trade_tariff_invalid_contract",
                      paste("unsupported tariff reuse argument(s):", paste(bad, collapse = ", ")))
  }
  zero <- all(abs(tau) <= 1e-14)
  cost_basis <- arguments$cost_basis
  margin_basis <- arguments$margin_basis
  if (is.null(cost_basis)) cost_basis <- if (zero) "effective" else NA_character_
  if (is.null(margin_basis)) margin_basis <- if (zero) "net_revenue" else NA_character_
  if (length(cost_basis) != 1L || is.na(cost_basis) ||
      !identical(as.character(cost_basis), "effective")) {
    .trade_game_error("trade_tariff_unsupported_cost_basis",
                      "tariff reuse currently supports cost_basis = 'effective' only; physical costs are unsupported")
  }
  if (length(margin_basis) != 1L || is.na(margin_basis)) {
    .trade_game_error("trade_tariff_missing_margin_basis",
                      "a nonzero baseline tariff requires explicit margin_basis = 'net_revenue'")
  }
  if (!identical(as.character(margin_basis), "net_revenue")) {
    .trade_game_error("trade_tariff_incompatible_basis",
                      "tariff reuse requires margin_basis = 'net_revenue'")
  }
  list(cost_basis = "effective", margin_basis = "net_revenue")
}

.trade_game_contract_requested <- function(arguments) {
  any(c("cost_basis", "margin_basis") %in% names(arguments))
}

.trade_game_spec_native <- function(model) {
  demand <- .trade_game_demand(model)
  conduct <- if (methods::is(model, "StackelbergLogit") ||
                 methods::is(model, "StackelbergCES")) {
    "stackelberg"
  } else {
    "core_fringe"
  }
  model_spec(demand, conduct, variant = "standard", policy = "tariff")
}

.trade_game_spec_ordinary <- function(object) {
  source <- object@spec
  if (!is.list(source) || any(!c("demand", "conduct", "variant") %in% names(source))) {
    .trade_game_error("trade_tariff_unsupported_model",
                      "the AntitrustFit does not contain a normalized model specification")
  }
  source <- try(antitrust::model_spec(source$demand, source$conduct,
                                      variant = source$variant), silent = TRUE)
  if (inherits(source, "try-error")) {
    .trade_game_error("trade_tariff_unsupported_model",
                      "tariff reuse requires a registered AntitrustFit model specification")
  }
  auction <- identical(source$demand, "logit") &&
    identical(source$conduct, "auction2nd") &&
    identical(source$variant, "standard")
  ordinary <- source$demand %in% c("logit", "ces") &&
    source$conduct %in% c("bertrand", "cournot", "moncom") &&
    source$variant %in% c("standard", "alm")
  if (!ordinary && !auction) {
    .trade_game_error("trade_tariff_unsupported_model",
                      "tariff reuse supports ordinary output Logit/CES Bertrand, Cournot, or MonCom fits plus standard output Logit second-score auction fits")
  }
  source_registry <- try(antitrust::supportedModels(), silent = TRUE)
  source_key <- paste(source$demand, source$conduct, source$variant, sep = "::")
  if (inherits(source_registry, "try-error") ||
      !is.data.frame(source_registry) ||
      !all(c("demand", "conduct", "variant", "class") %in%
           names(source_registry))) {
    .trade_game_error("trade_tariff_unsupported_model",
                      "cannot verify the AntitrustFit class against antitrust::supportedModels()")
  }
  source_match <- paste(source_registry$demand, source_registry$conduct,
                        source_registry$variant, sep = "::") == source_key
  registered_classes <- unique(source_registry$class[source_match])
  if (!length(registered_classes) ||
      !class(object@model)[[1L]] %in% registered_classes) {
    .trade_game_error(
      "trade_tariff_unsupported_model",
      paste0("source model class '", class(object@model)[[1L]],
             "' does not match antitrust::supportedModels() for '",
             source_key, "'")
    )
  }
  model_spec(source$demand, source$conduct, variant = source$variant,
             policy = "tariff")
}

.trade_game_state <- function(model, tariff, kappa, source_model,
                              source_fit = NULL, spec = NULL) {
  n <- length(kappa)
  labels <- .trade_game_labels(model, n)
  owner <- .trade_game_owner(source_model, TRUE)
  roles <- .trade_game_roles(source_model, TRUE)
  if (length(owner) != n) {
    .trade_game_error("trade_tariff_invalid_ownership",
                      "baseline ownership must contain one ID per product")
  }
  cpre <- kappa * (1 - tariff)
  if (any(!is.finite(cpre) | cpre <= 0)) {
    .trade_game_error("trade_tariff_invalid_cost_state",
                      "recovered physical baseline costs must be finite and positive")
  }
  stats::setNames(cpre, labels) -> cpre
  stats::setNames(kappa, labels) -> kappa
  list(
    tariff_convention = "consumer_price_fraction",
    tariffPre = tariff,
    tariffPost = tariff,
    retentionPre = 1 - tariff,
    retentionPost = 1 - tariff,
    cost_basis = "effective",
    margin_basis = "net_revenue",
    kappaPre = kappa,
    kappaPost = kappa,
    effective_cost_pre = kappa,
    effective_cost_post = kappa,
    cPre = cpre,
    cPost = cpre,
    physical_cost_pre = cpre,
    physical_cost_post = cpre,
    ownerPre = owner,
    ownerPost = owner,
    leadersPre = roles$leaders,
    leadersPost = roles$leaders,
    corePre = roles$core,
    corePost = roles$core,
    subsetPre = rep(TRUE, n),
    subsetPost = rep(TRUE, n),
    labels = labels,
    source_fit = source_fit,
    source_model = source_model,
    source_model_class = class(source_model)[[1L]],
    source_spec = if (is.null(spec)) NULL else spec,
    promotion_solves = 0L
  )
}

.trade_game_new_fit <- function(source_object, source_model, state, spec,
                                route = "tariff_reuse", source_fit = NULL) {
  observed <- if (!is.null(source_fit) && is.list(source_fit@observed)) {
    source_fit@observed
  } else list()
  observed$prices <- .trade_game_prices(source_model, TRUE)
  observed$shares <- .trade_game_slot(source_model, "shares")
  observed$ownerPre <- state$ownerPre
  observed$labels <- state$labels
  observed$tariffPre <- state$tariffPre
  observed$tariffPost <- state$tariffPost
  diagnostics <- if (!is.null(source_fit) && is.list(source_fit@diagnostics)) {
    source_fit@diagnostics
  } else list()
  diagnostics$route <- route
  diagnostics$source <- if (is.null(source_fit)) "coordination" else "antitrust"
  diagnostics$tariff_reuse <- state
  diagnostics$tariff_reuse$source_model <- NULL
  diagnostics$tariff_reuse$source_fit <- NULL
  parameters <- if (!is.null(source_fit)) source_fit@parameters else .trade_parameters(source_model)
  result <- new("TariffGameFit", spec = spec, model = source_model,
                parameters = parameters, observed = observed,
                diagnostics = diagnostics, tariff_state = state,
                source_fit = source_fit)
  ## Posterior draw plans and other package-neutral provenance may be attached
  ## to either the native model or an ordinary AntitrustFit.  Keep those
  ## attributes on the wrapper as well as on its copied model.
  if (!is.null(source_object)) {
    source_attributes <- attributes(source_object)
    ignored <- c(methods::slotNames(source_object), "class")
    for (name in setdiff(names(source_attributes), ignored)) {
      attr(result, name) <- source_attributes[[name]]
    }
  }
  result
}

.trade_game_promote <- function(object, policy, arguments, native = .trade_game_native(object)) {
  if (!identical(.normalize_trade_policy(policy), "tariff")) {
    .trade_game_error("trade_tariff_unsupported_policy",
                      "TariffGameFit supports policy = 'tariff' only")
  }
  model <- if (native) object else object@model
  .trade_game_validate_output(model)
  n <- .trade_game_n(model)
  labels <- .trade_game_labels(model, n)
  tau <- .trade_game_normalize_tariff(arguments$tariffPre, n, labels)
  .trade_game_uniform_tariff(tau, .trade_game_owner(model, TRUE), "baseline")
  .trade_game_parse_contract(arguments, tau, native = native)
  source_diagnostics <- if (native) NULL else object@diagnostics
  if (any(abs(tau) > 1e-14)) {
    for (diagnostics in list(source_diagnostics, .trade_game_slot(model, "diagnostics"))) {
      if (!is.list(diagnostics)) next
      for (field in c("margin_basis", "margin_semantics", "cost_basis", "cost_semantics")) {
        recorded <- diagnostics[[field, exact = TRUE]]
        expected <- if (startsWith(field, "margin")) "net_revenue" else "effective"
        if (!is.null(recorded) && !identical(as.character(recorded), expected)) {
          .trade_game_error("trade_tariff_incompatible_basis",
                            paste("the source fit records incompatible", field))
        }
      }
    }
  }
  kappa <- .trade_game_source_costs(model)
  source_model <- .trade_game_baseline_model(model)
  spec <- if (native) .trade_game_spec_native(model) else .trade_game_spec_ordinary(object)
  source_fit <- if (native) NULL else object
  state <- .trade_game_state(model, tau, kappa, source_model, source_fit, spec)
  wrapped_model <- .trade_game_copy_attributes(model, source_model)
  .trade_game_new_fit(if (native) model else object, wrapped_model, state, spec,
                      route = "tariff_reuse_promote", source_fit = source_fit)
}

.trade_game_promote_antitrust_fit <- function(object, policy = "tariff",
                                              arguments = list()) {
  .trade_game_promote(object, policy = policy, arguments = arguments,
                      native = FALSE)
}

.trade_game_unpack_counterfactual <- function(value) {
  if (!methods::is(value, "Counterfactual")) return(NULL)
  if (length(value@steps) != 1L) {
    .trade_game_error("trade_tariff_multiple_steps",
                      "TariffGameFit initially supports exactly one counterfactual step")
  }
  changes <- value@steps[[1L]]@changes
  supported <- c("ownership", "costs", "exit", "tariff", "leader", "core")
  bad <- setdiff(names(changes), supported)
  if (length(bad)) {
    .trade_game_error("trade_tariff_unsupported_counterfactual",
                      paste("unsupported counterfactual field(s):", paste(bad, collapse = ", ")))
  }
  list(
    tariffPost = changes$tariff,
    ownerPost = changes$ownership,
    mcDelta = changes$costs,
    subset = changes$exit,
    leadersPost = changes$leader,
    corePost = changes$core
  )
}

.trade_game_simulate_native <- function(state, model, owner, leaders, core,
                                        delta, subset, priceStart) {
  source <- state$source_model
  if (!is.null(priceStart) && .trade_game_has(source, "priceStart")) {
    if (length(priceStart) != length(state$labels) ||
        any(!is.finite(priceStart)) || any(priceStart <= 0)) {
      .trade_game_error("trade_tariff_invalid_price_start",
                        "'priceStart' must be finite and positive with one value per product")
    }
    source@priceStart <- as.numeric(priceStart)
  }
  if (methods::is(source, "StackelbergLogit") || methods::is(source, "StackelbergCES")) {
    coordination::stackelberg_simulate(
      source, ownerPost = owner, leadersPost = leaders,
      mcDelta = as.numeric(delta), subset = subset
    )
  } else {
    coordination::core_fringe_simulate(
      source, ownerPost = owner, corePost = core,
      mcDelta = as.numeric(delta), subset = subset
    )
  }
}

.trade_game_simulate_ordinary <- function(state, owner, delta, subset, priceStart) {
  if (is.null(state$source_fit)) {
    .trade_game_error("trade_tariff_invalid_source", "ordinary tariff reuse is missing its original source fit")
  }
  ## antitrust owns cost-state interpretation and all ordinary equilibrium
  ## equations.  Passing the immutable source fit prevents cumulative tariff
  ## ratios from entering a repeated scenario.  Auction2ndLogit is the one
  ## registered ordinary model whose mcDelta is an additive effective-cost
  ## level; the tariff state stores delta as a proportional effective change.
  cost_delta <- as.numeric(delta)
  if (methods::is(state$source_model, "Auction2ndLogit")) {
    cost_delta <- as.numeric(state$kappaPre) * cost_delta
  }
  args <- list(state$source_fit, owner, mcDelta = cost_delta, subset = subset)
  if (!is.null(priceStart)) args$priceStart <- priceStart
  result <- try(do.call(antitrust::simulate, args), silent = TRUE)
  if (inherits(result, "try-error")) {
    .trade_game_error("trade_tariff_simulation_failed",
                      conditionMessage(attr(result, "condition") %||%
                                       simpleError(as.character(result))))
  }
  if (methods::is(result, "AntitrustFit")) result@model else result
}

.trade_game_simulate <- function(object, tariffPost = NULL, quotaPost = NULL,
                                 ownerPost = NULL, leadersPost = NULL,
                                 corePost = NULL, mcDelta = NULL, subset = NULL,
                                 priceStart = NULL, ...) {
  if (!is.null(quotaPost)) {
    .trade_game_error("trade_tariff_unsupported_policy",
                      "TariffGameFit does not support quotaPost")
  }
  extra <- list(...)
  if (length(extra)) {
    .trade_game_error("trade_tariff_invalid_simulation",
                      paste("unsupported simulation argument(s):", paste(names(extra), collapse = ", ")))
  }
  cf <- .trade_game_unpack_counterfactual(tariffPost)
  if (!is.null(cf)) {
    conflicts <- c(if (!is.null(ownerPost)) "ownerPost",
                   if (!is.null(leadersPost)) "leadersPost",
                   if (!is.null(corePost)) "corePost",
                   if (!is.null(mcDelta)) "mcDelta",
                   if (!is.null(subset)) "subset")
    if (length(conflicts)) {
      .trade_game_error("trade_tariff_invalid_simulation",
                        paste("cannot combine a Counterfactual with:", paste(conflicts, collapse = ", ")))
    }
    tariffPost <- cf$tariffPost; ownerPost <- cf$ownerPost
    mcDelta <- cf$mcDelta; subset <- cf$subset
    leadersPost <- cf$leadersPost; corePost <- cf$corePost
  }

  state <- object@tariff_state
  n <- length(state$labels)
  tau <- .trade_game_normalize_tariff(
    if (is.null(tariffPost)) state$tariffPost else tariffPost,
    n, state$labels, "tariffPost"
  )
  owner <- .trade_game_normalize_owner(ownerPost, state$ownerPost, n)
  owner_changed <- !identical(owner, state$ownerPost)
  subset <- if (is.null(subset)) state$subsetPost else subset
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) {
    if (is.numeric(subset) || is.character(subset)) {
      subset <- .counterfactual_subset(subset, n, state$labels)
    } else {
      .trade_game_error("trade_tariff_invalid_subset",
                        "'subset' must retain at least one product")
    }
  }
  delta_physical <- .trade_game_normalize_delta(
    if (is.null(mcDelta)) state$mcDeltaPhysical else mcDelta,
    n, state$labels
  )
  ## Inactive products do not participate in ownership blocks.  Their stored
  ## policy values may differ without creating a strategic within-firm tariff
  ## heterogeneity in the active market.
  .trade_game_uniform_tariff(tau[subset], owner[subset], "post-policy")

  native_fit <- .trade_game_native_fit(object)
  is_stack <- .trade_game_is_stack(object)
  is_core <- .trade_game_is_core(object)
  if (!native_fit && (!is.null(leadersPost) || !is.null(corePost))) {
    .trade_game_error("trade_tariff_invalid_roles",
                      "leadersPost/corePost are only meaningful for native coordination games")
  }
  if (native_fit && is_stack && !is.null(corePost)) {
    .trade_game_error("trade_tariff_invalid_roles",
                      "corePost is only meaningful for CoreFringe games")
  }
  if (native_fit && is_core && !is.null(leadersPost)) {
    .trade_game_error("trade_tariff_invalid_roles",
                      "leadersPost is only meaningful for Stackelberg games")
  }
  roles_pre <- state$leadersPre
  cores_pre <- state$corePre
  leaders <- if (is.null(leadersPost)) {
    if (owner_changed && is_stack) {
      .trade_game_error("trade_tariff_roles_required",
                        "leadersPost must be supplied when ownership changes")
    }
    state$leadersPost
  } else as.character(leadersPost)
  core <- if (is.null(corePost)) {
    if (owner_changed && is_core) {
      .trade_game_error("trade_tariff_roles_required",
                        "corePost must be supplied when ownership changes")
    }
    state$corePost
  } else as.character(corePost)
  if (native_fit) {
    valid_firms <- unique(owner[subset])
    if (length(leaders) && anyDuplicated(leaders)) {
      .trade_game_error("trade_tariff_invalid_roles",
                        "leadersPost must contain unique firm IDs")
    }
    if (length(core) && anyDuplicated(core)) {
      .trade_game_error("trade_tariff_invalid_roles",
                        "corePost must contain unique firm IDs")
    }
    if (length(leaders) && any(!leaders %in% valid_firms)) {
      .trade_game_error("trade_tariff_invalid_roles",
                        "leadersPost contains a firm absent from active post ownership")
    }
    if (length(core) && any(!core %in% valid_firms)) {
      .trade_game_error("trade_tariff_invalid_roles",
                        "corePost contains a firm absent from active post ownership")
    }
  }

  cpost <- state$cPre * (1 + delta_physical)
  rpost <- 1 - tau
  kpost <- cpost / rpost
  if (any(!is.finite(cpost) | cpost <= 0) || any(!is.finite(kpost) | kpost <= 0)) {
    .trade_game_error("trade_tariff_invalid_cost_state",
                      "post physical and effective costs must be finite and positive")
  }
  delta_effective <- kpost / state$kappaPre - 1

  unchanged <- identical(tau, state$tariffPost) &&
    identical(owner, state$ownerPost) && identical(subset, state$subsetPost) &&
    identical(leaders, state$leadersPost) &&
    identical(core, state$corePost) &&
    all(abs(kpost - state$kappaPost) <= 1e-12)
  result_model <- if (unchanged) {
    object@model
  } else if (native_fit) {
    .trade_game_simulate_native(state, object@model, owner, leaders, core,
                                delta_effective, subset, priceStart)
  } else {
    .trade_game_simulate_ordinary(state, owner, delta_effective, subset, priceStart)
  }
  ## Public simulators may reconstruct a new model and drop package-neutral
  ## provenance attributes.  Carry the immutable source model's attributes
  ## forward when available; wrapper attributes are copied separately below.
  if (!unchanged) {
    result_model <- .trade_game_copy_attributes(state$source_model, result_model)
  }

  result_state <- state
  result_state$tariffPost <- tau
  result_state$retentionPost <- rpost
  result_state$kappaPost <- stats::setNames(kpost, state$labels)
  result_state$effective_cost_post <- result_state$kappaPost
  result_state$cPost <- stats::setNames(cpost, state$labels)
  result_state$physical_cost_post <- result_state$cPost
  result_state$ownerPost <- owner
  result_state$leadersPost <- leaders
  result_state$corePost <- core
  result_state$subsetPost <- subset
  result_state$mcDeltaPhysical <- delta_physical
  result_state$mcDeltaEffective <- delta_effective
  result_state$promotion_solves <- 0L
  .trade_game_new_fit(object, result_model, result_state, object@spec,
                      route = if (unchanged) "tariff_reuse_unchanged" else "tariff_reuse_simulate",
                      source_fit = object@source_fit)
}

#' @rdname as_trade_fit
#' @export
setMethod("as_trade_fit", "StackelbergLogit", function(object, policy = "tariff", ...) {
  .trade_game_promote(object, policy, list(...), native = TRUE)
})

#' @rdname as_trade_fit
#' @export
setMethod("as_trade_fit", "StackelbergCES", function(object, policy = "tariff", ...) {
  .trade_game_promote(object, policy, list(...), native = TRUE)
})

#' @rdname as_trade_fit
#' @export
setMethod("as_trade_fit", "CoreFringeLogit", function(object, policy = "tariff", ...) {
  .trade_game_promote(object, policy, list(...), native = TRUE)
})

#' @rdname as_trade_fit
#' @export
setMethod("as_trade_fit", "CoreFringeCES", function(object, policy = "tariff", ...) {
  .trade_game_promote(object, policy, list(...), native = TRUE)
})

#' @rdname trade-architecture
#' @export
setMethod("simulate", "TariffGameFit", function(object, tariffPost = NULL,
                                                   quotaPost = NULL, ownerPost = NULL,
                                                   leadersPost = NULL, corePost = NULL,
                                                   mcDelta = NULL, subset = NULL,
                                                   priceStart = NULL, ...) {
  .trade_game_simulate(object, tariffPost = tariffPost, quotaPost = quotaPost,
                       ownerPost = ownerPost, leadersPost = leadersPost,
                       corePost = corePost, mcDelta = mcDelta, subset = subset,
                       priceStart = priceStart, ...)
})

#' @rdname trade-architecture
#' @export
setMethod("validate_counterfactual", "TariffGameFit",
          function(object, counterfactual) {
            .trade_game_unpack_counterfactual(counterfactual)
            invisible(counterfactual)
          })

#' @rdname trade-architecture
#' @export
setMethod("simulate_steps", "TariffGameFit",
          function(object, last_result, steps, ...) {
            .trade_game_error(
              "trade_tariff_multiple_steps",
              "TariffGameFit initially supports one policy step; multi-step paths are unsupported"
            )
          })

#' @rdname trade-architecture
#' @export
setMethod("respecify", "TariffGameFit", function(object, demand = NULL,
                                                   conduct = NULL, variant = NULL,
                                                   ...) {
  .trade_game_error(
    "trade_tariff_unsupported_lifecycle",
    "respecify() is unsupported for TariffGameFit; promote the immutable source fit under a new specification"
  )
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcPrices", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  antitrust::calcPrices(object@model, preMerger = preMerger, ...)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcShares", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  antitrust::calcShares(object@model, preMerger = preMerger, ...)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcQuantities", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  .trade_game_account_quantities(object, preMerger = preMerger)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcSlopes", "TariffGameFit", function(object, ...) {
  antitrust::calcSlopes(object@model, ...)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcMC", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  value <- if (preMerger) object@tariff_state$cPre else object@tariff_state$cPost
  stats::setNames(as.numeric(value), object@tariff_state$labels)
})

.trade_game_account_quantities <- function(object, preMerger = TRUE) {
  quantities <- try(antitrust::calcQuantities(object@model,
                                               preMerger = preMerger),
                    silent = TRUE)
  if (inherits(quantities, "try-error") || is.matrix(quantities)) {
    .trade_game_error(
      "trade_tariff_invalid_output",
      "the stored game must expose product quantities through calcQuantities()"
    )
  }
  quantities <- as.numeric(quantities)
  n <- length(object@tariff_state$labels)
  if (length(quantities) != n) {
    .trade_game_error("trade_tariff_invalid_output",
                      "the stored game returned the wrong number of product quantities")
  }
  active_name <- if (preMerger) "subsetPre" else "subsetPost"
  active <- object@tariff_state[[active_name]]
  quantities[!active] <- 0
  if (any(!is.finite(quantities[active])) || any(quantities[active] < 0)) {
    .trade_game_error(
      "trade_tariff_invalid_output",
      "active product quantities must be finite and non-negative"
    )
  }
  quantities
}

#' @rdname antitrust-reexports
#' @export
setMethod("calcProducerSurplus", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  prices <- .trade_game_prices(object@model, preMerger)
  quantities <- .trade_game_account_quantities(object, preMerger)
  costs <- if (preMerger) object@tariff_state$cPre else object@tariff_state$cPost
  tau <- if (preMerger) object@tariff_state$tariffPre else object@tariff_state$tariffPost
  unit_profit <- (1 - tau) * prices - costs
  unit_profit[as.numeric(quantities) == 0 & !is.finite(unit_profit)] <- 0
  stats::setNames(unit_profit * as.numeric(quantities),
                  object@tariff_state$labels)
})

#' @rdname antitrust-reexports
#' @export
setMethod("CV", "TariffGameFit", function(object, ...) {
  antitrust::CV(object@model, ...)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcRevenues", "TariffGameFit", function(object, preMerger = TRUE, ...) {
  accounts <- tariff_accounts(object, preMerger = preMerger)
  stats::setNames(accounts$gross_revenue, accounts$product)
})

#' @rdname antitrust-reexports
#' @export
setMethod("calcMargins", "TariffGameFit", function(object, preMerger = TRUE,
                                                     level = FALSE, ...) {
  ## Derive both reports from the same account rows used by producer surplus
  ## and government revenue.  This keeps the wrapper's public margin report
  ## exactly aligned with physical costs and net consumer revenue, including
  ## solver residuals in stored post-policy prices.
  accounts <- tariff_accounts(object, preMerger = preMerger)
  value <- if (isTRUE(level)) accounts$physical_unit_profit else accounts$net_margin
  stats::setNames(as.numeric(value), accounts$product)
})

#' Compact tariff summary
#'
#' The tariff wrapper does not use the historical tariff summary because that
#' summary treats effective costs as physical costs.  The market summary below
#' reports physical producer surplus and government transfers separately from
#' antitrust's established CV convention.  Product accounts are returned when
#' `market = FALSE`.
#'
#' @param object A `TariffGameFit`.
#' @param market If `TRUE`, return one aggregate market row; otherwise return
#'   `tariff_accounts()`.
#' @param revenue,levels,parameters,insideOnly Legacy summary controls.  They
#'   are unsupported for this wrapper because their historical calculations use
#'   an untaxed margin convention.
#' @param digits Number of printed digits.
#' @param ... Unused.
#' @return An invisible data frame.
#' @export
setMethod("summary", "TariffGameFit", function(object, market = FALSE,
                                                revenue = FALSE, levels = FALSE,
                                                parameters = FALSE,
                                                insideOnly = TRUE, digits = 2,
                                                ...) {
  if (!isTRUE(market) && !identical(market, FALSE)) {
    .trade_game_error("trade_tariff_invalid_summary", "'market' must be TRUE or FALSE")
  }
  if (!identical(revenue, FALSE) || !identical(levels, FALSE) ||
      !identical(parameters, FALSE) || !identical(insideOnly, TRUE)) {
    .trade_game_error(
      "trade_tariff_unsupported_summary",
      "legacy revenue/levels/parameters/insideOnly summary controls are unsupported for TariffGameFit"
    )
  }
  extra <- list(...)
  if (length(extra)) {
    .trade_game_error("trade_tariff_unsupported_summary",
                      paste("unsupported summary argument(s):", paste(names(extra), collapse = ", ")))
  }
  if (length(digits) != 1L || !is.finite(digits)) {
    .trade_game_error("trade_tariff_invalid_summary", "'digits' must be finite")
  }
  pre <- tariff_accounts(object, preMerger = TRUE)
  post <- tariff_accounts(object, preMerger = FALSE)
  if (!isTRUE(market)) {
    print(pre)
    cat("\nPost-policy tariff accounts:\n")
    print(post)
    invisible(post)
  } else {
    weighted_price <- function(accounts) {
      q <- accounts$q
      denominator <- sum(q, na.rm = TRUE)
      if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
      sum(accounts$price * q, na.rm = TRUE) / denominator
    }
    welfare <- tariff_welfare(object)
    result <- data.frame(
      pricePre = weighted_price(pre),
      pricePost = weighted_price(post),
      quantityPre = sum(pre$q, na.rm = TRUE),
      quantityPost = sum(post$q, na.rm = TRUE),
      physicalProducerSurplusDelta = welfare$physical_producer_surplus_delta,
      importingGovernmentRevenueDelta = welfare$importing_government_revenue_delta,
      physicalPlusGovernmentDelta = welfare$physical_plus_government_delta,
      consumerVariation = welfare$consumer_variation,
      stringsAsFactors = FALSE
    )
    print(round(result, digits = as.integer(digits)))
    attr(result, "consumer_variation_sign") <- welfare$consumer_variation_sign
    attr(result, "consumer_variation_scope") <- welfare$consumer_variation_scope
    attr(result, "producer_scope") <- welfare$producer_scope
    attr(result, "government_scope") <- welfare$government_scope
    invisible(result)
  }
})

#' Product tariff accounts
#'
#' @param object A `TariffGameFit`.
#' @param preMerger If `TRUE`, report the stored baseline; otherwise report the
#'   current post-policy state.
#' @return A data frame with consumer prices, quantities, tariff fractions,
#'   gross and net revenue, effective and physical costs, producer profit, and
#'   government revenue.
#' @export
tariff_accounts <- function(object, preMerger = FALSE) {
  if (!methods::is(object, "TariffGameFit")) {
    .trade_game_error("trade_tariff_invalid_object", "object must be a TariffGameFit")
  }
  state <- object@tariff_state
  prices <- .trade_game_prices(object@model, preMerger)
  quantities <- .trade_game_account_quantities(object, preMerger)
  tau <- if (preMerger) state$tariffPre else state$tariffPost
  kappa <- if (preMerger) state$kappaPre else state$kappaPost
  costs <- if (preMerger) state$cPre else state$cPost
  gross <- prices * quantities
  ## Equilibrium result objects conventionally retain NA prices for exited
  ## products.  Their zero output makes every account exactly zero; avoid
  ## propagating the indeterminate NA * 0 into welfare sums.
  gross[quantities == 0 & !is.finite(gross)] <- 0
  net <- (1 - tau) * gross
  profit <- (1 - tau) * gross - costs * quantities
  government <- tau * gross
  data.frame(
    product = state$labels,
    firm = if (preMerger) state$ownerPre else state$ownerPost,
    price = prices,
    q = quantities,
    tau = as.numeric(tau),
    gross_revenue = gross,
    net_revenue = net,
    effective_cost = as.numeric(kappa),
    physical_cost = as.numeric(costs),
    effective_unit_markup = prices - as.numeric(kappa),
    physical_unit_profit = (1 - tau) * prices - costs,
    net_margin = ifelse(net == 0, NA_real_,
                        ((1 - tau) * prices - costs) / ((1 - tau) * prices)),
    producer_profit = profit,
    government_revenue = government,
    stringsAsFactors = FALSE,
    row.names = state$labels
  )
}

#' Tariff welfare accounting
#'
#' Returns physical producer-surplus and importing-government transfers
#' separately from the established antitrust consumer-variation statistic.
#' Producer surplus and government revenue are summed over all products in the
#' wrapper; no nationality is inferred.
#'
#' @param object A `TariffGameFit` with a solved post state.
#' @return A named list containing the separate changes and their sum.
#' @export
tariff_welfare <- function(object) {
  if (!methods::is(object, "TariffGameFit")) {
    .trade_game_error("trade_tariff_invalid_object", "object must be a TariffGameFit")
  }
  pre <- tariff_accounts(object, TRUE)
  post <- tariff_accounts(object, FALSE)
  ps_delta <- sum(post$producer_profit) - sum(pre$producer_profit)
  gov_delta <- sum(post$government_revenue) - sum(pre$government_revenue)
  cv <- try(CV(object), silent = TRUE)
  cv <- if (inherits(cv, "try-error")) NA_real_ else as.numeric(cv)
  list(
    physical_producer_surplus_delta = ps_delta,
    importing_government_revenue_delta = gov_delta,
    physical_plus_government_delta = ps_delta + gov_delta,
    consumer_variation = cv,
    monetary_consumer_change = if (is.na(cv)) NA_real_ else -cv,
    consumer_variation_sign = "positive compensation required for a price increase",
    consumer_variation_scope = "established antitrust CV",
    producer_scope = "all included producers",
    government_scope = "importing government"
  )
}
