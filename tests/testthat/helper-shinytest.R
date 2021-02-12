file_remove_if_exists <- function(x) {
  if (file.exists(x)) file.remove(x)
}

shinytest_suffix <- function() {

  sysname <- tolower(Sys.info()[["sysname"]])

  if (identical(Sys.getenv("APPVEYOR"), "True")) {
    suffix <- "wincheck"
  } else if (sysname == "windows") {
    if (identical(Sys.getenv("NOT_CRAN"), "true")) {
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
