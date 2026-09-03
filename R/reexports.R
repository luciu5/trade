#' Re-exported antitrust diagnostics and accessors
#'
#' @description
#' These functions are implemented in \pkg{antitrust} and re-exported by
#' \pkg{trade} because they are useful for inspecting calibrated tariff and
#' quota simulations.
#'
#' @param object An instance of a supported simulation class.
#' @param levels If TRUE, report changes in levels. If FALSE, report
#'   proportional changes.
#' @param market If TRUE, report a market-level result when supported.
#' @param party If TRUE, report a party-level result when supported.
#' @param isMax If TRUE, run the underlying local maximum check when supported.
#' @param index Price index to use for aggregate price changes.
#' @param path A `CounterfactualPath` object.
#' @param i A single step index, for `result_at()`.
#' @param ... Additional arguments passed to the underlying \pkg{antitrust}
#'   method.
#'
#' @name antitrust-reexports
NULL

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust calcPriceDelta
antitrust::calcPriceDelta

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust calcShares
antitrust::calcShares

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust calcRevenues
antitrust::calcRevenues

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust calcMC
antitrust::calcMC

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust CV
antitrust::CV

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust elast
antitrust::elast

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust diversion
antitrust::diversion

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust calcDiagnostics
antitrust::calcDiagnostics

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust ownerToMatrix
antitrust::ownerToMatrix

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust ownerToVec
antitrust::ownerToVec

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust final_result
antitrust::final_result

#' @rdname antitrust-reexports
#' @export
#' @importFrom antitrust result_at
antitrust::result_at
