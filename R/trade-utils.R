.normalize_tariff <- function(tariff, n, name) {
  if(length(tariff) != n){
    stop("'", name, "' must be a length-k vector")
  }

  tariff[is.na(tariff)] <- 0

  if(any(tariff >= 1, na.rm=TRUE)){
    stop("'", name, "' must be less than 1")
  }

  tariff
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

.apply_tariff_to_owner <- function(owner, tariff) {
  owner * matrix(1 - tariff, nrow = length(tariff), ncol = length(tariff))
}

.tariff_mc_delta <- function(tariffPre, tariffPost) {
  (tariffPost - tariffPre)/(1 - tariffPost)
}
