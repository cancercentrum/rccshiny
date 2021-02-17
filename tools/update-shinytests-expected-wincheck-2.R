# Preparations
#
# 1. Run update-shinytests-expected-wintest.R
# 2. Run update-shinytests-expected-wincheck-1.R
# 3. Run Check Package in RStudio
# 4. Inspect differences between shinytest results when running tests during
#    R CMD check and expected shinytest results when testing the package in
#    RStudio (e.g. Ctrl+Shift+T, devtools::test, testthat::test_check or
#    testthat::test_dir) -- the differences should be minor

dirs_expected_wincheck <- stringr::str_subset(
  list.dirs("../rccShiny.Rcheck/tests/testthat/apps/"),
  pattern = "current"
)

for (dir_expected_wincheck in dirs_expected_wincheck) {
  for (file_expected_wincheck in list.files(dir_expected_wincheck, "*.json", full.names = TRUE)) {
    file.copy(
      from = file_expected_wincheck,
      to = stringr::str_replace(
        stringr::str_replace(
          file_expected_wincheck,
          pattern = "../rccShiny.Rcheck/tests/testthat/apps/",
          replacement = "tests/testthat/apps/"
        ),
        pattern = "current",
        replacement = "expected-wincheck"
      ),
      overwrite = TRUE
    )
  }
}
