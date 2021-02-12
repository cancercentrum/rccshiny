file_remove_if_exists <- function(x) {
  if (file.exists(x)) file.remove(x)
}

shinytest_suffix <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])

  if (sysname == "windows") {
    # Use different expected shinytest results on Windows for different scenarios
    if (identical(Sys.getenv("R_CMD"), "R CMD")) {
      # Probably running R CMD check
      suffix <- "wincheck"
    } else {
      suffix <- "wintest"
    }
  } else if (sysname %in% c("darwin", "linux")) {
    suffix <- "mac"
  } else {
    suffix <- NULL
  }

  suffix
}
