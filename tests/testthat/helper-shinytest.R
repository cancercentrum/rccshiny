file_remove_if_exists <- function(x) {
  if (file.exists(x)) file.remove(x)
}

shinytest_suffix <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])

  if (sysname == "windows") {
    if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "rccShiny")) {
      # Probably running R CMD check
      suffix <- "wincheck"
    } else {
      suffix <- "wintest"
    }
  } else if (sysname %in% c("darwin", "linux")) {
    if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "rccShiny")) {
      # Probably running R CMD check
      suffix <- "macheck"
    } else {
      suffix <- "mactest"
    }
  } else {
    suffix <- NULL
  }

  suffix
}
