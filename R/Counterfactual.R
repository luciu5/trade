#' Define a post-calibration trade-policy counterfactual
#'
#' `counterfactual()` stores only supplied changes to the economic environment.
#' Demand, conduct, and model variants are deliberately not counterfactual
#' fields; use `update()` or `respecify()` for those changes.
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
#' @param ... Reserved; model specification fields are rejected.
#' @return A lightweight object of class `Counterfactual`.
#' @export
counterfactual <- function(ownership = NULL, costs = NULL, exit = NULL,
                           capacity = NULL, tariff = NULL, quota = NULL,
                           bargaining = NULL, leader = NULL, products = NULL,
                           ...) {
    extras <- list(...)
    if (length(extras)) {
        stop("counterfactual() accepts economic-environment fields only; use update() or respecify() for model specification changes")
    }
    structure(Filter(Negate(is.null), list(
        ownership = ownership, costs = costs, exit = exit,
        capacity = capacity, tariff = tariff, quota = quota,
        bargaining = bargaining, leader = leader, products = products
    )), class = c("Counterfactual", "list"))
}

#' @export
print.Counterfactual <- function(x, ...) {
    cat("Counterfactual\n")
    if (length(x)) cat("  fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
    else cat("  fields: none (leave the fitted baseline unchanged)\n")
    invisible(x)
}

#' @export
combine_counterfactuals <- function(...) {
    objects <- list(...)
    if (!length(objects) || !all(vapply(objects, inherits,
                                        logical(1), "Counterfactual"))) {
        stop("all arguments must be Counterfactual objects")
    }
    result <- list()
    for (object in objects) {
        for (name in names(object)) {
            if (!is.null(result[[name]]) &&
                !isTRUE(all.equal(result[[name]], object[[name]]))) {
                stop("conflicting values supplied for counterfactual field '", name, "'")
            }
            result[[name]] <- object[[name]]
        }
    }
    structure(result, class = c("Counterfactual", "list"))
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

.validate_counterfactual <- function(cf, spec) {
    capabilities <- .trade_counterfactual_capabilities(spec)
    unsupported <- names(cf)[!vapply(names(cf), function(name) {
        isTRUE(capabilities[[name]])
    }, logical(1))]
    if (length(unsupported)) {
        stop("model '", spec$id, "' does not support counterfactual field(s): ",
             paste(unsupported, collapse = ", "))
    }
    invisible(cf)
}

.counterfactual_attach <- function(result, fit, cf) {
    attr(result, "counterfactual") <- list(
        model_spec = fit@spec,
        fields = cf
    )
    result
}
