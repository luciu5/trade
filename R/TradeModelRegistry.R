#' Normalized trade model specification and registry
#'
#' The registry records complete economic implementations. It is deliberately
#' not a demand/supply composition engine: an entry points to the legacy trade
#' constructor whose calibration and equilibrium equations remain authoritative.
#'
#' @param demand A demand-system name, such as code{"logit"} or code{"ces"}.
#' @param conduct A conduct name, such as code{"bertrand"} or
#'   code{"cournot"}.
#' @param variant An implemented model variant. The standard variant is
#'   code{"standard"}; code{"alm"} is used by the ALM Logit-Cournot path.
#' @param policy The policy state represented by the legacy model. Supported
#'   values currently include code{"tariff"} and code{"quota"}.
#' @return code{model_spec} returns a small normalized list. code{supportedModels}
#'   returns a data frame describing the complete registered models.
#' @name trade-model-registry
NULL

.trade_registry <- local({
  entries <- list(
    list(id = "logit::bertrand", demand = "logit", conduct = "bertrand",
         variant = "standard", policy = "tariff", class = "TariffLogit",
         legacy_calibrator = "bertrand_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "ces::bertrand", demand = "ces", conduct = "bertrand",
         variant = "standard", policy = "tariff", class = "TariffCES",
         legacy_calibrator = "bertrand_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "aids::bertrand", demand = "aids", conduct = "bertrand",
         variant = "standard", policy = "tariff", class = "TariffAIDS",
         legacy_calibrator = "bertrand_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "blp::bertrand", demand = "blp", conduct = "bertrand",
         variant = "standard", policy = "tariff", class = "TariffLogitBLP",
         legacy_calibrator = "blp", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "blp::cournot", demand = "blp", conduct = "cournot",
         variant = "standard", policy = "tariff", class = "TariffCournotBLP",
         legacy_calibrator = "blp", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "blp::auction2nd", demand = "blp", conduct = "auction2nd",
         variant = "standard", policy = "tariff", class = "TariffAuction2ndBLP",
         legacy_calibrator = "blp", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "blp::bargaining", demand = "blp", conduct = "bargaining",
         variant = "standard", policy = "tariff", class = "TariffBargainingBLP",
         legacy_calibrator = "blp", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::moncom", demand = "logit", conduct = "moncom",
         variant = "standard", policy = "tariff", class = "TariffMonComLogit",
         legacy_calibrator = "monopolistic_competition_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "ces::moncom", demand = "ces", conduct = "moncom",
         variant = "standard", policy = "tariff", class = "TariffMonComCES",
         legacy_calibrator = "monopolistic_competition_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::cournot", demand = "logit", conduct = "cournot",
         variant = "standard", policy = "tariff", class = "TariffLogitCournot",
         legacy_calibrator = "logit_cournot_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::cournot::alm", demand = "logit", conduct = "cournot",
         variant = "alm", policy = "tariff", class = "TariffLogitCournotALM",
         legacy_calibrator = "logit_cournot_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "linear::cournot", demand = "linear", conduct = "cournot",
         variant = "standard", policy = "tariff", class = "TariffCournot",
         legacy_calibrator = "cournot_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "loglin::cournot", demand = "loglin", conduct = "cournot",
         variant = "standard", policy = "tariff", class = "TariffCournot",
         legacy_calibrator = "cournot_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::auction2nd", demand = "logit", conduct = "auction2nd",
         variant = "standard", policy = "tariff", class = "Tariff2ndLogit",
         legacy_calibrator = "auction2nd_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::bargaining", demand = "logit", conduct = "bargaining",
         variant = "standard", policy = "tariff", class = "TariffBargainingLogit",
         legacy_calibrator = "bargaining_tariff", calibrate = TRUE,
         specify = TRUE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "ces::bargaining", demand = "ces", conduct = "bargaining",
         variant = "standard", policy = "tariff", class = "TariffBargainingCES",
         legacy_calibrator = "bargaining_tariff", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = TRUE, quota = FALSE),
    list(id = "logit::bertrand::quota", demand = "logit", conduct = "bertrand",
         variant = "standard", policy = "quota", class = "QuotaLogit",
         legacy_calibrator = "bertrand_quota", calibrate = TRUE,
         specify = FALSE, simulate = TRUE, tariff = FALSE, quota = TRUE)
  )

  function() entries
})

.normalize_trade_name <- function(value, aliases, name) {
  if (length(value) != 1L || is.na(value) || !is.character(value)) {
    stop("'", name, "' must be a single character string")
  }
  key <- gsub("[^a-z0-9]", "", tolower(value))
  if (!key %in% names(aliases)) {
    stop("unsupported ", name, " '", value, "'")
  }
  unname(aliases[[key]])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.normalize_trade_demand <- function(demand) {
  .normalize_trade_name(demand, c(
    logit = "logit", multinomiallogit = "logit",
    blp = "blp", logitblp = "blp", randomcoefficientslogit = "blp",
    ces = "ces", aids = "aids", linear = "linear",
    loglinear = "loglin", loglin = "loglin"
  ), "demand")
}

.normalize_trade_conduct <- function(conduct) {
  .normalize_trade_name(conduct, c(
    bertrand = "bertrand", nashbertrand = "bertrand",
    moncom = "moncom", monopolisticcompetition = "moncom",
    cournot = "cournot", auction2nd = "auction2nd",
    secondscore = "auction2nd", auction = "auction2nd",
    bargaining = "bargaining"
  ), "conduct")
}

.normalize_trade_variant <- function(variant) {
  .normalize_trade_name(variant, c(
    standard = "standard", default = "standard", alm = "alm"
  ), "variant")
}

.normalize_trade_policy <- function(policy) {
  .normalize_trade_name(policy, c(
    tariff = "tariff", tariffs = "tariff", advalorem = "tariff",
    quota = "quota", quotas = "quota"
  ), "policy")
}

.trade_registry_key <- function(demand, conduct, variant, policy) {
  fields <- c(demand, conduct, variant, policy)
  paste(fields[nzchar(fields)], collapse = "::")
}

.trade_registry_entry <- function(spec_or_demand, conduct = NULL,
                                  variant = "standard", policy = "tariff") {
  if (inherits(spec_or_demand, "trade_model_spec")) {
    demand <- spec_or_demand$demand
    conduct <- spec_or_demand$conduct
    variant <- spec_or_demand$variant
    policy <- spec_or_demand$policy
  } else {
    demand <- .normalize_trade_demand(spec_or_demand)
    conduct <- .normalize_trade_conduct(conduct)
    variant <- .normalize_trade_variant(variant)
    policy <- .normalize_trade_policy(policy)
  }

  key <- .trade_registry_key(demand, conduct, variant, policy)
  entries <- .trade_registry()
  entry <- Filter(function(x) .trade_registry_key(
    x$demand, x$conduct, x$variant, x$policy
  ) == key, entries)

  if (!length(entry)) {
    stop("unsupported trade model combination: ", key)
  }
  entry[[1L]]
}

#' @rdname trade-model-registry
#' @export
model_spec <- function(demand, conduct, variant = "standard",
                       policy = "tariff") {
  if (length(variant) == 1L && identical(tolower(variant), "quota")) {
    policy <- "quota"
    variant <- "standard"
  }

  demand <- .normalize_trade_demand(demand)
  conduct <- .normalize_trade_conduct(conduct)
  variant <- .normalize_trade_variant(variant)
  policy <- .normalize_trade_policy(policy)
  entry <- .trade_registry_entry(demand, conduct, variant, policy)

  structure(
    list(
      demand = demand,
      conduct = conduct,
      variant = variant,
      policy = policy,
      id = entry$id
    ),
    class = c("trade_model_spec", "list")
  )
}

#' @rdname trade-model-registry
#' @export
supportedModels <- function() {
  entries <- .trade_registry()
  character_fields <- c(
    "id", "demand", "conduct", "variant", "policy", "class",
    "legacy_calibrator"
  )
  fields <- c(character_fields, "calibrate", "specify", "simulate", "tariff", "quota")
  result <- lapply(fields, function(field) {
    if (field %in% character_fields) {
      vapply(entries, function(entry) entry[[field]], character(1))
    } else {
      vapply(entries, function(entry) entry[[field]], logical(1))
    }
  })
  names(result) <- fields
  as.data.frame(result, stringsAsFactors = FALSE)
}

.trade_counterfactual_capabilities <- function(spec) {
  entry <- .trade_registry_entry(spec)
  if (is.null(entry)) return(stats::setNames(logical(), character()))
  c(
    ownership = FALSE,
    costs = FALSE,
    exit = TRUE,
    capacity = FALSE,
    bargaining = FALSE,
    leader = FALSE,
    products = spec$conduct == "cournot",
    tariff = isTRUE(entry$tariff),
    quota = isTRUE(entry$quota),
    ## Verified for every registry entry whose legacy class wraps a
    ## Logit/CES-family antitrust demand system (calibrated and
    ## quality-shocked directly; post-shock FOC residual at machine
    ## precision for each). TariffAIDS (AIDS demand) and TariffCournot
    ## (linear/loglin Cournot demand) have no meanval slot and stay
    ## excluded.
    quality = entry$class %in% .quality_supported_trade_classes,
    ## Entry is not implemented for trade in this release: an entrant's
    ## tariff/quota treatment is an economically substantive primitive not
    ## specified by the existing policy state.
    entry = FALSE
  )
}


## Respecification is validated by antitrust's transition graph. Trade adds
## only policy bookkeeping; MonCom is a first-class antitrust conduct, so its
## demand transitions use the same registered graph and structural formulas.
## This keeps trade from maintaining a second demand-conversion graph that
## could drift from antitrust.
.trade_policy_transition_metadata <- function(policy, target) {
  if (identical(policy, "quota")) {
    list(
      retained = "quotaPre",
      translated = character(),
      discarded = "quotaPost",
      required_from_user = character(),
      recomputed = c("quota-adjusted supply state", "equilibrium state")
    )
  } else {
    list(
      retained = "tariffPre",
      translated = character(),
      discarded = "tariffPost",
      required_from_user = character(),
      recomputed = c("tariff-adjusted ownership", "marginal costs",
                     "equilibrium state")
    )
  }
}

.trade_antitrust_transition_entry <- function(from, to) {
  source <- antitrust::model_spec(
    from$demand, .trade_antitrust_conduct(from$conduct), from$variant
  )
  target <- antitrust::model_spec(
    to$demand, .trade_antitrust_conduct(to$conduct), to$variant
  )
  antitrust::model_transition(source, target)
}

.trade_transition_metadata <- function(from, to) {
  target_entry <- .trade_registry_entry(to)
  if (!isTRUE(target_entry$specify)) {
    stop("respecify() target '", to$id,
         "' is not supported: it has no supplied-parameter construction path; use update()")
  }

  same_flat_conduct <- identical(from$demand, to$demand) &&
    identical(.trade_antitrust_conduct(from$conduct),
              .trade_antitrust_conduct(to$conduct)) &&
    !identical(from$conduct, to$conduct)
  if (same_flat_conduct) {
    entry <- list(
      from = from$id,
      to = to$id,
      kind = "conduct_change",
      required_arguments = character(),
      retain = if (identical(from$demand, "ces")) {
        c("alpha", "gamma", "meanval")
      } else {
        c("alpha", "meanval")
      },
      derived = character(),
      discarded = c("source conduct supply state"),
      recompute = c("marginal costs", "target conduct state"),
      invalidate = c("source conduct supply state"),
      calibration_required = FALSE,
      handler = "portable"
    )
  } else {
    entry <- try(.trade_antitrust_transition_entry(from, to), silent = TRUE)
    if (inherits(entry, "try-error")) {
      stop("respecify() transition from '", from$id, "' to '",
           to$id, "' is not supported by antitrust's transition graph; use update() to recalibrate the target model")
    }
    entry$from <- from$id
    entry$to <- to$id
  }

  if (is.null(entry$required_arguments)) entry$required_arguments <- character()
  if (is.null(entry$kind)) entry$kind <- "structural-restriction"
  if (is.null(entry$derived)) entry$derived <- character()
  if (is.null(entry$discarded)) entry$discarded <- character()
  if (is.null(entry$calibration_required)) entry$calibration_required <- FALSE

  if (identical(to$conduct, "bargaining") &&
      !identical(from$conduct, "bargaining")) {
    entry$required_arguments <- unique(c(
      entry$required_arguments, "bargpowerPre"
    ))
    entry$derived <- unique(setdiff(entry$derived, "bargpowerPre"))
    entry$discarded <- unique(c(entry$discarded,
                                "source conduct supply state"))
  }
  entry$policy <- .trade_policy_transition_metadata(from$policy, to)
  entry
}

## Compatibility view for diagnostics and existing callers. Its contents are
## generated from antitrust's transition graph and the registered complete
## trade models; no demand-conversion formulas live here.
.trade_transition_registry <- function() {
  entries <- .trade_registry()
  candidates <- list()
  for (from in entries) {
    for (to in entries) {
      if (!identical(from$policy, to$policy) || identical(from$id, to$id)) {
        next
      }
      source <- structure(from[c("demand", "conduct", "variant", "policy", "id")],
                          class = c("trade_model_spec", "list"))
      target <- structure(to[c("demand", "conduct", "variant", "policy", "id")],
                          class = c("trade_model_spec", "list"))
      candidate <- try(.trade_transition_metadata(source, target), silent = TRUE)
      if (!inherits(candidate, "try-error")) candidates[[length(candidates) + 1L]] <- candidate
    }
  }
  candidates
}

.trade_transition_entry <- function(from, to) {
  .trade_transition_metadata(from, to)
}
