# Sequential tariff reuse. Antitrust owns market advancement and entry;
# trade owns the tariff assignment and physical-cost policy state.

.trade_game_entrant_tariff <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || !is.null(dim(value)) ||
      !is.finite(value) || value >= 1) {
    .trade_game_error("trade_tariff_invalid_policy",
      "'entrant_tariff' and entrant extra 'tariff' must be finite numeric scalars less than one")
  }
  as.numeric(value)
}

.trade_game_validate_path <- function(object, cf) {
  if (!methods::is(cf, "Counterfactual")) {
    .trade_game_error("trade_tariff_invalid_simulation", "expected a Counterfactual")
  }
  supported <- c("ownership", "costs", "exit", "tariff", "leader", "core", "entry")
  for (step in cf@steps) {
    bad <- setdiff(names(step@changes), supported)
    if (length(bad)) {
      .trade_game_error("trade_tariff_unsupported_counterfactual",
        paste("unsupported counterfactual field(s):", paste(bad, collapse = ", ")))
    }
    if (!is.null(step@changes$entry)) {
      if (.trade_game_native_fit(object)) {
        .trade_game_error("trade_tariff_unsupported_counterfactual",
          "entry is unsupported for native coordination tariff games")
      }
      # The source package remains authoritative about concrete entry classes.
      antitrust::validate_counterfactual(object@source_fit,
        antitrust::counterfactual(entry = step@changes$entry))
      for (e in step@changes$entry) {
        if (e@cost <= 0) {
          .trade_game_error("trade_tariff_invalid_cost_state",
            "tariff game entry requires a strictly positive physical cost")
        }
        if (!is.null(e@extras$tariff)) .trade_game_entrant_tariff(e@extras$tariff)
      }
    }
  }
  invisible(cf)
}

.trade_game_entry_owners <- function(existing, entrants) {
  owners <- existing
  for (e in entrants) {
    candidate <- e@label
    while (candidate %in% owners) candidate <- paste0("entrant:", candidate)
    owners <- c(owners, candidate)
  }
  owners
}

.trade_game_step <- function(object, step) {
  previous <- object@tariff_state
  changes <- step@changes
  entrants <- changes$entry
  added <- length(entrants)
  # A policy default has no economic role in antitrust's market expansion.
  # Entry supplies physical costs under retention one. The post-step tariff
  # then transforms these to effective costs exactly once.
  environment <- new("CounterfactualStep", changes =
    if (added) list(entry = entrants) else list())
  model <- antitrust::advance_market(object@model, environment)
  state <- previous
  for (post in c("tariffPost", "retentionPost", "kappaPost", "cPost",
                 "ownerPost", "leadersPost", "corePost", "subsetPost")) {
    pre <- sub("Post$", "Pre", post)
    state[[pre]] <- previous[[post]]
  }
  # A prior step's solved quantities, including its outside alternative,
  # constitute the actual pre-step market. Entry's synthetic starting prices
  # must not rewrite those accounts or add pre-entry output.
  state$previous_model <- object@model
  state$baseline_prices <- c(.trade_game_prices(object@model, FALSE), rep(NA_real_, added))
  state$baseline_quantities <- c(.trade_game_account_quantities(object, FALSE), rep(0, added))
  if (added) {
    labels <- vapply(entrants, function(e) e@label, character(1))
    costs <- vapply(entrants, function(e) e@cost, numeric(1))
    assigned <- vapply(entrants, function(e) {
      .trade_game_entrant_tariff(e@extras$tariff %||% previous$entrant_tariff %||% 0)
    }, numeric(1))
    state$labels <- c(previous$labels, labels)
    state$tariffPre <- c(state$tariffPre, rep(0, added))
    state$tariffPost <- c(previous$tariffPost, assigned)
    state$retentionPre <- c(state$retentionPre, rep(1, added))
    state$retentionPost <- 1 - state$tariffPost
    state$kappaPre <- c(state$kappaPre, costs)
    state$kappaPost <- c(previous$kappaPost, costs)
    state$cPre <- c(state$cPre, costs)
    state$cPost <- c(previous$cPost, costs)
    state$ownerPre <- .trade_game_entry_owners(previous$ownerPost, entrants)
    state$ownerPost <- state$ownerPre
    state$subsetPre <- c(previous$subsetPost, rep(FALSE, added))
    state$subsetPost <- c(previous$subsetPost, rep(TRUE, added))
  }
  for (name in c("tariffPre", "tariffPost", "retentionPre", "retentionPost",
                 "kappaPre", "kappaPost", "cPre", "cPost")) {
    names(state[[name]]) <- state$labels
  }
  state$effective_cost_pre <- state$kappaPre
  state$effective_cost_post <- state$kappaPost
  state$physical_cost_pre <- state$cPre
  state$physical_cost_post <- state$cPost
  state$mcDeltaPhysical <- NULL
  state$mcDeltaEffective <- NULL
  state$environment_changed <- added > 0L
  model <- antitrust::setRetention(model, retentionPre = state$retentionPre,
                                   retentionPost = state$retentionPre)
  # Start at the preceding equilibrium where possible; retain a finite old
  # starting value for inactive products, which remain in indexed storage.
  starts <- .trade_game_prices(model, TRUE)
  finite <- is.finite(starts) & starts > 0
  if (.trade_game_has(model, "priceStart")) model@priceStart[finite] <- starts[finite]
  state$source_model <- model
  if (!is.null(object@source_fit)) {
    working_fit <- object@source_fit
    working_fit@model <- model
    working_fit@parameters <- .trade_parameters(model)
    state$source_fit <- working_fit
  }
  working <- object
  working@model <- model
  working@tariff_state <- state
  if (!is.null(names(changes$costs))) {
    inactive <- intersect(names(changes$costs), state$labels[!state$subsetPost])
    if (length(inactive)) {
      .trade_game_error("trade_tariff_invalid_cost_shock",
        paste("costs reference inactive product(s):", paste(inactive, collapse = ", ")))
    }
  }
  active <- state$subsetPost
  if (!is.null(changes$exit)) {
    active <- active & .counterfactual_subset(changes$exit,
      length(state$labels), state$labels)
  }
  if (!any(active)) {
    .trade_game_error("trade_tariff_invalid_subset", "a path step must retain at least one active product")
  }
  result <- .trade_game_simulate(working, tariffPost = changes$tariff,
    ownerPost = changes$ownership, mcDelta = changes$costs %||% 0,
    subset = active, leadersPost = changes$leader, corePost = changes$core)
  result@parameters <- .trade_parameters(result@model)
  result
}

.trade_game_steps <- function(last_result, steps) {
  if (!methods::is(last_result, "TariffGameFit")) {
    .trade_game_error("trade_tariff_invalid_simulation", "tariff path is missing its last fitted state")
  }
  state <- last_result
  results <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    state <- .trade_game_step(state, steps[[i]])
    results[[i]] <- state
  }
  results
}

.trade_game_path <- function(object, cf) {
  results <- .trade_game_steps(object, cf@steps)
  if (length(cf@steps) == 1L) return(results[[1L]])
  new("CounterfactualPath", initial = object@model,
      steps = cf@steps, results = results, diagnostics = list(fit = object))
}
