.normalize_tariff <- function(tariff, n, name) {
  if (!is.numeric(tariff) || !is.null(dim(tariff)) || any(is.nan(tariff))) {
    stop("'", name, "' must be a finite numeric tariff vector")
  }
  if(length(tariff) != n){
    stop("'", name, "' must be a length-k vector")
  }

  tariff[is.na(tariff)] <- 0

  if(any(!is.finite(tariff)) || any(tariff >= 1)){
    stop("'", name, "' must be finite and less than 1")
  }

  tariff
}

.normalize_tariff_matrix <- function(tariff, dims, name) {
  if (!is.numeric(tariff) || !is.matrix(tariff) ||
      !identical(dim(tariff), as.integer(dims)) || any(is.nan(tariff))) {
    stop("'", name, "' must be a numeric matrix matching the fitted quantities")
  }
  tariff[is.na(tariff)] <- 0
  if (any(!is.finite(tariff)) || any(tariff >= 1)) {
    stop("'", name, "' must be finite and less than 1")
  }
  tariff
}

.set_tariff_retention <- function(object) {
  antitrust::setRetention(object, retentionPre = 1 - object@tariffPre,
                         retentionPost = 1 - object@tariffPost)
}

## Attaching baseline policy may rescale a whole firm's payoff but cannot
## change its relative product weights without changing the fitted game.
.trade_check_baseline_retention <- function(model, retention, owner = NULL) {
  original <- antitrust::getRetention(model, preMerger = TRUE)
  if (is.null(owner)) owner <- model@ownerPre
  n <- length(retention)
  owner <- .owner_to_matrix(owner, n, "baseline ownership is not valid")
  log_ratio <- log(retention) - log(original)
  linked <- which(owner != 0, arr.ind = TRUE)
  if (nrow(linked) && any(abs(log_ratio[linked[, 1L]] -
                              log_ratio[linked[, 2L]]) > 1e-10)) {
    .trade_game_error(
      "trade_tariff_incompatible_baseline",
      paste("baseline tariffs are incompatible with the source fitted retention;",
            "calibrate a retention-aware baseline or simulate the tariff change",
            "from the existing baseline instead of attaching it during promotion")
    )
  }
  invisible(TRUE)
}

.normalize_quota <- function(quota, n, name) {
  if(length(quota) != n){
    stop("'", name, "' must be a length-k vector")
  }

  quota[is.na(quota)] <- Inf
  quota
}

.owner_to_matrix <- function(owner, n, message) {
  if(missing(owner) || is.null(owner)){
    stop(message)
  }

  if(is.matrix(owner)){
    if(!isTRUE(all.equal(dim(owner), c(n,n)))){
      stop(message)
    }
    return(owner)
  }

  if(length(owner) != n){
    stop(message)
  }

  owner <- factor(owner, levels = unique(owner))
  owner <- stats::model.matrix(~-1+owner)
  tcrossprod(owner)
}

.tariff_mc_delta <- function(tariffPre, tariffPost) {
  (tariffPost - tariffPre)/(1 - tariffPost)
}
