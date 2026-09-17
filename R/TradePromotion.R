# Promotion of a fitted antitrust model into a policy-aware trade model.
#
# This file intentionally does not call calibrate(), specify(), calcSlopes(),
# or an equilibrium solver.  The source AntitrustFit is already the fitted
# structural state; promotion adds only the registered trade policy state.

#' Promote a fitted antitrust model into a trade model
#'
#' \code{as_trade_fit()} creates a policy-aware \linkS4class{TradeFit} from a
#' fitted \link[antitrust:AntitrustFit-class]{AntitrustFit}. The demand and
#' conduct state is copied from the source fit. Promotion does not recalibrate
#' demand or solve an equilibrium.
#'
#' @param object A fitted \code{AntitrustFit}, or a \code{StructuralFit} for the
#'   fallback error method.
#' @param policy The registered trade policy family, currently \code{"tariff"}
#'   or \code{"quota"}.
#' @param ... The baseline policy state. Tariff models accept \code{tariffPre},
#'   defaulting to zero; the Logit quota model accepts \code{quotaPre},
#'   defaulting to \code{Inf} (unconstrained). The post-policy state is
#'   initialized to the same value.
#' @return A \code{TradeFit} containing the copied source model and policy state.
#' @details
#' Eligibility requires the exact normalized demand, conduct, and variant in
#' both packages' \code{supportedModels()} registries, under the requested
#' trade policy. Source parameters, fitted model slots, integration state,
#' ownership, and baseline equilibrium are preserved. Post-policy slots are
#' initialized to the source baseline; use \code{simulate()} to solve a
#' subsequent policy change.
#'
#' Tariffs use a product vector except for Linear and LogLin Cournot models,
#' which require a plant-by-product matrix matching the fitted quantities.
#' A numeric scalar expands to the required dimensions. Product tariffs must
#' be finite and less than one; matrix tariffs must be finite and greater than
#' minus one, following the target models' tariff conventions. Missing tariff
#' entries become zero.
#'
#' Quotas are non-negative multiples of fitted baseline output. Missing quota
#' entries become \code{Inf}. A quota below one is incompatible with the
#' copied baseline for a product with positive fitted output and is rejected.
#' Unsupported or irrelevant policy arguments are rejected.
#' @examples
#' source_fit <- antitrust::specify(
#'   "logit", "bertrand", prices = c(2, 2.2, 2.5),
#'   parameters = list(alpha = -1.5, meanval = c(1, 0.8, 1.2)),
#'   ownerPre = diag(3), insideSize = 100
#' )
#' policy_fit <- as_trade_fit(source_fit, tariffPre = 0)
#' policy_fit@model@pricePre
#' source_fit@model@pricePre
#' @export
#' @importClassesFrom antitrust AntitrustFit StructuralFit
setGeneric(
  "as_trade_fit",
  function(object, policy = "tariff", ...) {
    standardGeneric("as_trade_fit")
  }
)

.trade_promotion_has_slot <- function(object, name) {
  name %in% methods::slotNames(object)
}

.trade_promotion_slot <- function(object, name, default = NULL) {
  if (.trade_promotion_has_slot(object, name)) {
    methods::slot(object, name)
  } else {
    default
  }
}

.trade_promotion_source_spec <- function(object, policy) {
  source <- object@spec
  required <- c("demand", "conduct", "variant")
  if (!is.list(source) || any(!required %in% names(source))) {
    stop("the AntitrustFit does not contain a normalized model specification")
  }

  ## Normalize through antitrust first.  This also prevents a hand-created fit
  ## with an unsupported source combination from entering the adapter.
  source_normalized <- try(
    antitrust::model_spec(source$demand, source$conduct, source$variant),
    silent = TRUE
  )
  if (inherits(source_normalized, "try-error")) {
    source_error <- attr(source_normalized, "condition")
    source_error <- if (is.null(source_error)) {
      as.character(source_normalized)
    } else {
      conditionMessage(source_error)
    }
    stop("cannot promote an AntitrustFit with source specification '",
         paste(source$demand, source$conduct, source$variant, sep = " / "),
         "': ", source_error)
  }

  source_registry <- try(antitrust::supportedModels(), silent = TRUE)
  source_key <- paste(source_normalized$demand, source_normalized$conduct,
                      source_normalized$variant, sep = "::")
  source_match <- if (inherits(source_registry, "try-error") ||
                      !is.data.frame(source_registry)) {
    logical()
  } else {
    paste(source_registry$demand, source_registry$conduct,
          source_registry$variant, sep = "::") == source_key
  }
  if (!length(source_match) || !any(source_match)) {
    stop("source model '", source_key,
         "' is not present in antitrust::supportedModels()")
  }
  target <- try(
    model_spec(source_normalized$demand, source_normalized$conduct,
               variant = source_normalized$variant, policy = policy),
    silent = TRUE
  )
  if (inherits(target, "try-error")) {
    stop("trade has no registered implementation for source model '",
         source_key, "' under policy '", policy, "'")
  }

  ## The source registry is authoritative about the class implementing the
  ## normalized source specification.  Check this only after target lookup so
  ## an unsupported trade route reports the eligibility failure first.
  source_class <- class(object@model)[[1L]]
  registered_classes <- unique(source_registry$class[source_match])
  if (!source_class %in% registered_classes) {
    stop("source model class '", source_class, "' does not match the registered antitrust class '",
         paste(registered_classes, collapse = "' or '"), "' for specification '",
         source_key, "'")
  }
  target
}

.trade_promotion_policy_state <- function(arguments, handler, model) {
  allowed <- switch(handler,
    tariff_vector = "tariffPre",
    tariff_matrix = "tariffPre",
    quota_vector = "quotaPre",
    stop("unknown promotion handler '", handler, "' in the trade registry")
  )
  supplied <- names(arguments)
  if (length(arguments) && (is.null(supplied) || any(!nzchar(supplied)))) {
    stop("promotion policy arguments must be named")
  }
  if (length(supplied) && anyDuplicated(supplied)) {
    stop("promotion policy arguments may not be supplied more than once")
  }
  unsupported <- setdiff(supplied, allowed)
  if (length(unsupported)) {
    stop("policy argument(s) ", paste(unsupported, collapse = ", "),
         " are not meaningful for this registered trade model; expected '",
         allowed, "'")
  }

  if (handler == "tariff_matrix") {
    if (!.trade_promotion_has_slot(model, "quantities") ||
        !is.matrix(model@quantities)) {
      stop("tariff promotion for the registered Cournot model requires a fitted plant-by-product quantity matrix")
    }
    dims <- dim(model@quantities)
    value <- arguments[[allowed]]
    if (is.null(value)) {
      value <- matrix(0, nrow = dims[[1L]], ncol = dims[[2L]])
    } else if (is.null(dim(value)) && length(value) == 1L) {
      value <- matrix(value, nrow = dims[[1L]], ncol = dims[[2L]])
    }
    if (!is.matrix(value) || !isTRUE(all.equal(dim(value), dims))) {
      stop("'tariffPre' must be a matrix with dimensions ", dims[[1L]],
           " x ", dims[[2L]], " for this registered Cournot model")
    }
    if (!is.numeric(value)) {
      stop("'tariffPre' must be a finite numeric tariff matrix")
    }
    value[is.na(value)] <- 0
    if (any(!is.finite(value)) || any(value <= -1)) {
      stop("'tariffPre' must be greater than -1 and finite for this Cournot tariff matrix")
    }
    return(list(name = allowed, pre = value, post = value,
                dimensions = dims, quantities = model@quantities))
  }

  n <- if (.trade_promotion_has_slot(model, "shares")) {
    length(model@shares)
  } else if (.trade_promotion_has_slot(model, "prices")) {
    length(model@prices)
  } else {
    0L
  }
  if (!n) stop("the fitted source model does not expose a product dimension")
  value <- arguments[[allowed]]
  default <- if (handler == "quota_vector") Inf else 0
  if (is.null(value)) value <- rep(default, n)
  ## A scalar with an explicit dim attribute is a malformed matrix input,
  ## rather than a scalar policy.  Check this before rep(), which strips dims.
  if (is.null(dim(value)) && length(value) == 1L && n > 1L) {
    value <- rep(value, n)
  }
  if (!is.numeric(value) || !is.null(dim(value)) || length(value) != n) {
    stop("'", allowed, "' must be a numeric length-k vector")
  }
  if (handler == "quota_vector") {
    ## Replace missing quotas only after checking the supplied type so malformed
    ## character values receive the policy argument error below.
    if (!is.numeric(value)) {
      stop("'quotaPre' must be a numeric length-k vector")
    }
    value[is.na(value)] <- Inf
    if (any(value < 0)) {
      stop("'quotaPre' must be non-negative, with Inf representing no quota")
    }
  } else {
    if (!is.numeric(value)) {
      stop("'tariffPre' must be a numeric length-k vector")
    }
    value[is.na(value)] <- 0
    if (any(!is.finite(value)) || any(value >= 1)) {
      stop("'tariffPre' must be finite and less than 1")
    }
  }
  list(name = allowed, pre = value, post = value,
       dimensions = n, quantities = NULL)
}

.trade_promotion_quantities <- function(object, model) {
  quantities <- NULL
  ## This reads the source model's stored pre-policy output.  For Logit it is
  ## algebraic; it does not run a policy or equilibrium solve.
  calculated <- try(antitrust::calcQuantities(model, preMerger = TRUE),
                    silent = TRUE)
  if (!inherits(calculated, "try-error") && is.numeric(calculated) &&
      !is.matrix(calculated) && all(is.finite(calculated))) {
    quantities <- calculated
  }
  if (is.null(quantities) || !is.numeric(quantities) ||
      length(quantities) != length(model@shares) ||
      any(!is.finite(quantities))) {
    stop("quota promotion requires finite quantities from antitrust::calcQuantities(model, preMerger = TRUE)")
  }
  ## Keep labels attached to fitted quantities so derived quota capacities
  ## retain the source model's product identity.
  quantities
}

.trade_promotion_copy_attributes <- function(source, target) {
  ## S4 slot values are copied below.  Preserve package-independent attributes
  ## attached by antitrust, especially antitrust_cost_state and BLP metadata.
  source_attributes <- attributes(source)
  ignored <- c(methods::slotNames(source), "class")
  custom <- setdiff(names(source_attributes), ignored)
  for (name in custom) attr(target, name) <- source_attributes[[name]]
  target
}

.trade_promotion_wrapper_parms <- function(object, model, target_class) {
  if (!"parmsStart" %in% methods::slotNames(target_class) ||
      "parmsStart" %in% methods::slotNames(model)) {
    return(NULL)
  }
  parameters <- object@parameters
  slopes <- .trade_promotion_slot(model, "slopes")
  first <- NULL
  if (identical(object@spec$demand, "ces") && is.list(slopes)) {
    first <- slopes$gamma
  } else if (is.list(slopes)) {
    first <- slopes$alpha
  }
  if (is.null(first) && is.list(parameters)) {
    first <- parameters[[if (identical(object@spec$demand, "ces")) "gamma" else "alpha"]]
  }
  first <- if (is.numeric(first) && length(first)) as.numeric(first[[1L]]) else 0
  ## parmsStart is an optimizer-start slot required by ALM wrapper validity;
  ## it is not used as a fitted structural primitive after promotion.
  c(first, 0)
}

.trade_promotion_model <- function(object, target, policy_state) {
  source_model <- object@model
  source_for_copy <- source_model
  if (is.null(attr(source_for_copy, "antitrust_cost_state", exact = TRUE)) &&
      exists("initialize_cost_state", asNamespace("antitrust"), inherits = FALSE)) {
    source_for_copy <- antitrust::initialize_cost_state(source_for_copy)
  }

  target_class <- .trade_registry_entry(target)$class
  if (!methods::isClass(target_class)) {
    stop("registered trade class '", target_class, "' is unavailable")
  }
  target_slots <- methods::slotNames(target_class)
  source_slots <- methods::slotNames(source_for_copy)
  values <- lapply(intersect(target_slots, source_slots), function(name) {
    methods::slot(source_for_copy, name)
  })
  names(values) <- intersect(target_slots, source_slots)

  wrapper_parms <- .trade_promotion_wrapper_parms(object, source_for_copy,
                                                   target_class)
  if (!is.null(wrapper_parms)) values$parmsStart <- wrapper_parms

  if (policy_state$name == "tariffPre") {
    values$tariffPre <- policy_state$pre
    values$tariffPost <- policy_state$post
  } else {
    quantities <- .trade_promotion_quantities(object, source_for_copy)
    ## A binding pre-policy quota would make the copied source output
    ## infeasible.  Promotion never solves that counterfactual, so reject only
    ## positive-output products whose supplied quota is below one; a zero
    ## quantity remains feasible at a zero quota.
    if (any(policy_state$pre < 1 & quantities > 0)) {
      stop("'quotaPre' cannot be below 1 for products with positive fitted output: the source baseline would violate the supplied quota")
    }
    ## Inf * 0 is NaN; explicit no-quota handling keeps zero-output products
    ## and the default unconstrained state well-defined.
    capacities <- ifelse(is.infinite(policy_state$pre), Inf,
                         policy_state$pre * quantities)
    if (!is.null(names(quantities))) names(capacities) <- names(quantities)
    values$quotaPre <- policy_state$pre
    values$quotaPost <- policy_state$post
    values$capacitiesPre <- capacities
    values$capacitiesPost <- capacities
  }

  ## Every post-state slot in the source object represents the same fitted
  ## baseline at promotion time.  Policy simulation is the only later step
  ## allowed to change these fields.
  for (name in c("ownerPost", "mcPost", "bargpowerPost",
                 "capacitiesPost", "productsPost", "mcfunPost", "vcfunPost",
                 "dmcfunPost", "quantityPost", "pricePost")) {
    pre_name <- sub("Post$", "Pre", name)
    if (pre_name %in% names(values) && name %in% target_slots) {
      values[[name]] <- values[[pre_name]]
    }
  }
  if ("mcDelta" %in% names(values)) {
    values$mcDelta <- rep(0, length(values$mcDelta))
  }
  if ("priceDelta" %in% names(values)) {
    values$priceDelta <- rep(0, length(values$priceDelta))
  }

  target_model <- do.call(methods::new, c(list(Class = target_class), values))
  target_model <- .trade_promotion_copy_attributes(source_for_copy,
                                                    target_model)
  quantities <- policy_state$quantities
  if (is.null(quantities) && identical(policy_state$name, "quotaPre")) {
    quantities <- .trade_promotion_quantities(object, source_for_copy)
  }
  marker <- list(
    policy = target$policy,
    source_spec = object@spec,
    target_spec = target,
    tariffPre = if (identical(policy_state$name, "tariffPre")) {
      policy_state$pre
    } else NULL,
    quotaPre = if (identical(policy_state$name, "quotaPre")) {
      policy_state$pre
    } else NULL,
    quantities = quantities,
    source_ownerPre = .trade_promotion_slot(source_for_copy, "ownerPre")
  )
  attr(target_model, "trade_promotion") <- marker
  methods::validObject(target_model)
  target_model
}

.trade_promotion_observed <- function(object, model, policy_state) {
  observed <- if (is.list(object@observed)) object@observed else list()
  copy_if_missing <- function(name, value) {
    if (is.null(observed[[name]]) && !is.null(value)) observed[[name]] <<- value
  }
  copy_if_missing("prices", .trade_promotion_slot(model, "pricePre"))
  copy_if_missing("shares", .trade_promotion_slot(model, "shares"))
  copy_if_missing("ownerPre", .trade_promotion_slot(model, "ownerPre"))
  for (name in c("labels", "priceOutside", "priceStart", "output",
                 "insideSize", "mktSize")) {
    copy_if_missing(name, .trade_promotion_slot(model, name))
  }
  if (identical(policy_state$name, "quotaPre") &&
      is.null(observed$quantities)) {
    observed$quantities <- .trade_promotion_quantities(object, model)
  }
  observed[[policy_state$name]] <- policy_state$pre
  observed[[sub("Pre$", "Post", policy_state$name)]] <- policy_state$post
  observed
}

.trade_promote_antitrust_fit <- function(object, policy = "tariff", arguments) {
  policy <- .normalize_trade_policy(policy)
  target <- .trade_promotion_source_spec(object, policy)
  entry <- .trade_registry_entry(target)
  if (!isTRUE(entry$promote) || is.null(entry$promotion_handler)) {
    stop("trade model '", target$id,
         "' has no registered AntitrustFit promotion route")
  }
  policy_state <- .trade_promotion_policy_state(arguments,
                                                 entry$promotion_handler,
                                                 object@model)
  model <- .trade_promotion_model(object, target, policy_state)
  observed <- .trade_promotion_observed(object, model, policy_state)

  diagnostics <- object@diagnostics
  if (!is.list(diagnostics)) diagnostics <- list()
  source_calibration <- diagnostics$calibration_args
  source_specification <- diagnostics$specification_args
  diagnostics$calibration_args <- NULL
  diagnostics$specification_args <- NULL
  diagnostics$route <- "promote"
  diagnostics$source <- "antitrust"
  diagnostics$legacy_class <- class(model)[[1L]]
  diagnostics$source_calibration_args <- source_calibration
  diagnostics$source_specification_args <- source_specification
  diagnostics$promotion <- list(
    source_fit_class = class(object)[[1L]],
    source_model_class = class(object@model)[[1L]],
    source_spec = object@spec,
    target_spec = target,
    policy = target$policy,
    policy_argument = policy_state$name,
    policy_state = policy_state$pre,
    handler = entry$promotion_handler,
    copied = c("fitted model slots", "structural parameters", "observed baseline",
               "custom model attributes", "conduct-specific primitives"),
    transformed = if (target$policy == "quota") {
      c("quota capacities = quotaPre * fitted quantities",
        "quota policy slots")
    } else if (entry$promotion_handler == "tariff_matrix") {
      c("plant-by-product tariff policy state")
    } else {
      c("product tariff policy state")
    },
    recalibrated = FALSE,
    solved_baseline = FALSE
  )

  new("TradeFit", spec = target, model = model,
      parameters = object@parameters, observed = observed,
      diagnostics = diagnostics)
}

#' @rdname as_trade_fit
#' @export
setMethod(
  "as_trade_fit", "AntitrustFit",
  function(object, policy = "tariff", ...) {
    .trade_promote_antitrust_fit(object, policy = policy, arguments = list(...))
  }
)

#' @rdname as_trade_fit
#' @export
setMethod(
  "as_trade_fit", "StructuralFit",
  function(object, policy = "tariff", ...) {
    stop("no AntitrustFit promotion route exists for StructuralFit class '",
         class(object)[[1L]], "'")
  }
)
