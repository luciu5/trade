# Test tiers are selected by the trade workflow. Local runs default to fast.
trade_test_tier <- function() {
  tier <- Sys.getenv("TRADE_TEST_TIER", unset = "fast")
  if (!tier %in% c("fast", "extended", "nightly")) {
    stop("TRADE_TEST_TIER must be fast, extended, or nightly")
  }
  tier
}

trade_skip_unless_tier <- function(required) {
  levels <- c("fast", "extended", "nightly")
  if (!required %in% levels) stop("unknown required test tier")
  testthat::skip_if(
    match(trade_test_tier(), levels) < match(required, levels),
    paste("requires trade", required, "test tier")
  )
}
