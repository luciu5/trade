# A permanent: prevents a requested launcher silently returning a warning string,
# using an obsolete UI, or making the optional GUI mandatory for ordinary trade.
test_that("deprecated launcher forwards the requested compatible GUI call", {
  received <- NULL
  launcher <- trade_shiny
  environment(launcher) <- list2env(list(.trade_toolbox_launcher = function() function(...) {
    received <<- list(...)
    "launched"
  }), parent = environment(trade_shiny))
  expect_warning(result <- launcher(example = TRUE), "deprecated")
  expect_identical(result, "launched")
  expect_identical(received, list(example = TRUE))
})

test_that("missing or old optional GUI gives the public installation command", {
  for (version in list(NULL, package_version("0.7.1"))) {
    error <- tryCatch(.trade_toolbox_launcher(available = !is.null(version),
      version = version), error = identity)
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), 'install_iovrse(packages = "competitiontoolbox")', fixed = TRUE)
    expect_match(conditionMessage(error), "competitiontoolbox >= 0.9.0", fixed = TRUE)
  }
})
