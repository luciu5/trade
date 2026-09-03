## Trade's Counterfactual/CounterfactualStep/CounterfactualPath/Entrant
## infrastructure reuses antitrust's exported S4 classes and generics
## (`add_step()`, `final_result()`, `result_at()`) directly via
## `import(antitrust)`; only the trade-specific constructor bodies,
## capability validation, and exit-subset handling live here. Entry is not
## supported for trade in this release: an entrant's tariff/quota treatment
## is an economically substantive primitive that is not specified by the
## existing tariff/quota policy state, so `entry` always errors clearly
## rather than guessing.

#' Define a post-calibration trade-policy counterfactual
#'
#' `counterfactual()` stores only supplied changes to the economic
#' environment, bundled as the single `CounterfactualStep` of a
#' `Counterfactual`. Demand, conduct, and model variants are deliberately
#' not counterfactual fields; use `update()` or `respecify()` for those
#' changes. Use `add_step()` to append further, sequential steps.
#'
#' @param ownership Ownership changes, unsupported unless registered.
#' @param costs Cost changes, unsupported unless registered.
#' @param exit Products to remove, or a logical active-product vector.
#' @param capacity Capacity changes, unsupported unless registered.
#' @param tariff A post-counterfactual tariff vector.
#' @param quota A post-counterfactual quota vector.
#' @param bargaining Bargaining-parameter changes, unsupported unless registered.
#' @param leader Leader changes, unsupported unless registered.
#' @param products Product-structure changes, unsupported unless registered.
#' @param quality A named numeric vector of proportional changes to
#'   calibrated `meanval`, keyed by product label. Verified only for the
#'   `logit::moncom::tariff` and `ces::moncom::tariff` trade models, whose
#'   legacy classes are exactly bare `antitrust::Logit`/`antitrust::CES`.
#' @param entry Not supported for trade in this release; always errors.
#' @param ... Reserved; model specification fields are rejected.
#' @return A `Counterfactual` object with exactly one `CounterfactualStep`.
#' @export
counterfactual <- function(ownership = NULL, costs = NULL, exit = NULL,
                           capacity = NULL, tariff = NULL, quota = NULL,
                           bargaining = NULL, leader = NULL, products = NULL,
                           quality = NULL, entry = NULL, ...) {
    extras <- list(...)
    if (length(extras)) {
        stop("counterfactual() accepts economic-environment fields only; use update() or respecify() for model specification changes")
    }
    if (!is.null(entry)) {
        stop("'entry' is not supported for trade models in this release: an entrant's tariff/quota treatment is not a specified primitive")
    }
    if (!is.null(quality) && (is.null(names(quality)) || any(!nzchar(names(quality))))) {
        stop("'quality' must be a named numeric vector (product label = proportional change)")
    }
    if (!is.null(quality) && anyDuplicated(names(quality))) {
        stop("'quality' contains duplicate product labels")
    }
    changes <- Filter(Negate(is.null), list(
        ownership = ownership, costs = costs, exit = exit,
        capacity = capacity, tariff = tariff, quota = quota,
        bargaining = bargaining, leader = leader, products = products,
        quality = quality
    ))
    step <- new("CounterfactualStep", changes = changes)
    new("Counterfactual", steps = list(step))
}

#' Append a sequential counterfactual step
#'
#' `add_step()` appends one new, simultaneous `CounterfactualStep` to a
#' `Counterfactual`. The appended step is solved starting from the
#' equilibrium produced by the immediately preceding step (or, for the
#' first appended step, the fitted baseline); it never replaces or
#' recalibrates prior steps.
#'
#' @param object A `Counterfactual` object.
#' @param ownership Ownership changes, unsupported unless registered.
#' @param costs Cost changes, unsupported unless registered.
#' @param exit Products to remove, or a logical active-product vector.
#' @param capacity Capacity changes, unsupported unless registered.
#' @param tariff A post-counterfactual tariff vector.
#' @param quota A post-counterfactual quota vector.
#' @param bargaining Bargaining-parameter changes, unsupported unless registered.
#' @param leader Leader changes, unsupported unless registered.
#' @param products Product-structure changes, unsupported unless registered.
#' @param quality A named numeric vector of proportional changes to
#'   calibrated `meanval`, keyed by product label.
#' @param entry Not supported for trade in this release; always errors.
#' @param ... Reserved; model specification fields are rejected.
#' @return A `Counterfactual` object with the new step appended.
#' @export
setMethod("add_step", "Counterfactual", function(object, ownership = NULL,
                                                  costs = NULL, exit = NULL,
                                                  capacity = NULL, tariff = NULL,
                                                  quota = NULL, bargaining = NULL,
                                                  leader = NULL, products = NULL,
                                                  quality = NULL, entry = NULL, ...) {
    extras <- list(...)
    if (length(extras)) {
        stop("add_step() accepts economic-environment fields only; use update() or respecify() for model specification changes")
    }
    if (!is.null(entry)) {
        stop("'entry' is not supported for trade models in this release: an entrant's tariff/quota treatment is not a specified primitive")
    }
    if (!is.null(quality) && (is.null(names(quality)) || any(!nzchar(names(quality))))) {
        stop("'quality' must be a named numeric vector (product label = proportional change)")
    }
    if (!is.null(quality) && anyDuplicated(names(quality))) {
        stop("'quality' contains duplicate product labels")
    }
    changes <- Filter(Negate(is.null), list(
        ownership = ownership, costs = costs, exit = exit,
        capacity = capacity, tariff = tariff, quota = quota,
        bargaining = bargaining, leader = leader, products = products,
        quality = quality
    ))
    step <- new("CounterfactualStep", changes = changes)
    object@steps <- c(object@steps, list(step))
    object
})

#' Combine non-conflicting counterfactual changes into one simultaneous step
#'
#' `combine_counterfactuals()` merges the single steps of several one-step
#' `Counterfactual` objects into one simultaneous `CounterfactualStep`,
#' erroring if any field is supplied with conflicting values. Combining
#' multi-step `Counterfactual` objects is ambiguous and is rejected
#' explicitly; use `add_step()` to sequence changes instead.
#'
#' @param ... `Counterfactual` objects, each with exactly one step.
#' @return A one-step `Counterfactual`.
#' @export
combine_counterfactuals <- function(...) {
    objects <- list(...)
    if (!length(objects) || !all(vapply(objects, methods::is, logical(1), "Counterfactual"))) {
        stop("all arguments must be Counterfactual objects")
    }
    if (any(vapply(objects, function(object) length(object@steps) != 1L, logical(1)))) {
        stop("combine_counterfactuals() only combines single-step Counterfactual objects; use add_step() to sequence multi-step Counterfactuals")
    }
    result <- list()
    for (object in objects) {
        step_changes <- object@steps[[1L]]@changes
        for (name in names(step_changes)) {
            if (!is.null(result[[name]]) &&
                !isTRUE(all.equal(result[[name]], step_changes[[name]]))) {
                stop("conflicting values supplied for counterfactual field '", name, "'")
            }
            result[[name]] <- step_changes[[name]]
        }
    }
    new("Counterfactual", steps = list(new("CounterfactualStep", changes = result)))
}

.counterfactual_subset <- function(exit, n, labels = NULL) {
    if (is.null(exit)) return(rep(TRUE, n))
    if (is.logical(exit)) {
        if (length(exit) != n) stop("'exit' must be a length-k logical vector")
        return(exit)
    }
    if (is.character(exit)) {
        if (is.null(labels) || any(!exit %in% labels)) {
            stop("character 'exit' values must match fitted product labels")
        }
        exit <- match(exit, labels)
    }
    if (!is.numeric(exit) || anyNA(exit) || any(exit < 1) ||
        any(exit > n) || any(exit != as.integer(exit))) {
        stop("'exit' must contain valid product indices or labels")
    }
    active <- rep(TRUE, n)
    active[as.integer(exit)] <- FALSE
    active
}

## The two trade registry entries whose legacy class is exactly bare
## antitrust::Logit / antitrust::CES (logit::moncom::tariff,
## ces::moncom::tariff) are the only models where quality's `meanval`
## multiplier has been verified against antitrust's demand equations.
## Every other trade class (LogitALM/CESALM-descended tariff Bertrand,
## LogitCap-descended quota, Cournot, auction, bargaining) is excluded.
.quality_supported_trade_classes <- c("TariffMonComLogit", "TariffMonComCES")

.validate_counterfactual_step <- function(step, spec) {
    capabilities <- .trade_counterfactual_capabilities(spec)
    unsupported <- names(step@changes)[!vapply(names(step@changes), function(name) {
        isTRUE(capabilities[[name]])
    }, logical(1))]
    if (length(unsupported)) {
        stop("model '", spec$id, "' does not support counterfactual field(s): ",
             paste(unsupported, collapse = ", "))
    }
    invisible(step)
}

.validate_counterfactual <- function(cf, spec) {
    for (step in cf@steps) .validate_counterfactual_step(step, spec)
    invisible(cf)
}

.resolve_named_shock <- function(values, labels, subset, field_name, default = 0) {
    if (is.null(names(values)) || any(!nzchar(names(values)))) {
        stop("'", field_name, "' must be a named vector (product label = value) once the market has changed dimension via exit")
    }
    if (anyDuplicated(names(values))) {
        stop("'", field_name, "' contains duplicate product labels")
    }
    unknown <- setdiff(names(values), labels)
    if (length(unknown)) {
        stop("'", field_name, "' references unknown product label(s): ",
             paste(unknown, collapse = ", "))
    }
    active_labels <- labels[subset]
    excluded <- setdiff(names(values), active_labels)
    if (length(excluded)) {
        stop("'", field_name, "' references product(s) that are not active (exited): ",
             paste(excluded, collapse = ", "))
    }
    full <- stats::setNames(rep(default, length(labels)), labels)
    full[names(values)] <- values
    full
}

.apply_quality <- function(model, quality) {
    if (!(class(model)[[1L]] %in% .quality_supported_trade_classes)) {
        stop("'quality' is only supported for trade models of exact class ",
             paste(.quality_supported_trade_classes, collapse = ", "),
             "; this fit is class '", class(model)[[1L]], "'")
    }
    labels <- .trade_slot(model, "labels")
    subset <- .trade_slot(model, "subset", rep(TRUE, length(labels)))
    unknown <- setdiff(names(quality), labels)
    if (length(unknown)) {
        stop("'quality' references unknown product label(s): ", paste(unknown, collapse = ", "))
    }
    active_labels <- labels[subset]
    excluded <- setdiff(names(quality), active_labels)
    if (length(excluded)) {
        stop("'quality' references product(s) that are not active (exited): ",
             paste(excluded, collapse = ", "))
    }
    meanval <- model@slopes$meanval
    idx <- match(names(quality), labels)
    meanval[idx] <- meanval[idx] * (1 + quality)
    model@slopes$meanval <- meanval
    model
}

.counterfactual_attach <- function(result, fit, cf) {
    attr(result, "counterfactual") <- list(
        model_spec = fit@spec,
        fields = cf
    )
    result
}
