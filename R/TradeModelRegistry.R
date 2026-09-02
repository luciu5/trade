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


## Respecification is intentionally narrower than model registration. These
## transitions have a complete supplied-parameter path in the legacy package:
## demand primitives can be retained while the distinct Bertrand and
## monopolistic-competition state is reconstructed. Cournot is not listed
## because trade has no supplied-parameter constructor for that model.
.trade_transition_registry <- local({
  entries <- list(
    list(from = "logit::bertrand", to = "logit::moncom",
         retain = c("alpha", "meanval"),
         recompute = c("marginal costs", "monopolistic-competition state"),
         invalidate = c("Bertrand margins"),
         calibration_required = FALSE),
    list(from = "logit::moncom", to = "logit::bertrand",
         retain = c("alpha", "meanval"),
         recompute = c("marginal costs", "Bertrand state"),
         invalidate = c("monopolistic-competition margins"),
         calibration_required = FALSE),
    list(from = "ces::bertrand", to = "ces::moncom",
         retain = c("alpha", "gamma", "meanval"),
         recompute = c("marginal costs", "monopolistic-competition state"),
         invalidate = c("Bertrand margins"),
         calibration_required = FALSE),
    list(from = "ces::moncom", to = "ces::bertrand",
         retain = c("alpha", "gamma", "meanval"),
         recompute = c("marginal costs", "Bertrand state"),
         invalidate = c("monopolistic-competition margins"),
         calibration_required = FALSE)
  )
  function() entries
})

.trade_transition_entry <- function(from, to) {
  entries <- .trade_transition_registry()
  matches <- Filter(function(entry) {
    identical(entry$from, from$id) && identical(entry$to, to$id)
  }, entries)
  if (!length(matches)) {
    stop("respecify() transition from '", from$id, "' to '",
         to$id, "' is not supported; use update() to recalibrate the target model")
  }
  matches[[1L]]
}
