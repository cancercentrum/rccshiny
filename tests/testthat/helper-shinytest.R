file_remove_if_exists <- function(x) {
  if (file.exists(x)) file.remove(x)
}

shinytest_suffix <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])

  if (sysname == "windows") {
    # Use different expected shinytest results on Windows for different scenarios
    # - testthat::test_dir, use "windows" (rename to "wintest"?)
    # - devtools::test, use "windows" (rename to "wintest"?)
    # - devtools::check, use "wincheck"
    # - rcmdcheck::rcmdcheck, use "wincheck"

    if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      # Probably running devtools since NOT_CRAN is set to "true",
      if (identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"), "FALSE")) {
        # Probably running devtools::check since _R_CHECK_FORCE_SUGGESTS_ is set to FALSE
        suffix <- "wincheck"
      } else {
        # Probably running another devtools function, e.g. devtools::test()
        suffix <- "windows"
      }
    } else if (!identical(Sys.getenv("R_TESTS"), "")) {
      # Probably running R CMD check
      # When R CMD check runs tests, it sets R_TESTS.
      # see https://github.com/r-lib/devtools/blob/v2.3.2/R/R.R#L17
      suffix <- "wincheck"
    } else {
      suffix <- "windows"
    }
  } else if (sysname %in% c("darwin", "linux")) {
    suffix <- "mac"
  } else {
    suffix <- NULL
  }

  suffix
}
