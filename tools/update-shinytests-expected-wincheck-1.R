# Preparations
#
# Run update-shinytest-expected-wintest.R

dirs_expected_wintest <- stringr::str_subset(
  list.dirs("tests/testthat/apps/"),
  pattern = "expected-wintest"
)

for (dir_expected_wintest in dirs_expected_wintest) {
  for (file_expected_wintest in list.files(dir_expected_wintest, "*.json",  full.names = TRUE)) {
    file.copy(
      from = file_expected_wintest,
      to = stringr::str_replace(
        file_expected_wintest,
        pattern = "expected-wintest",
        replacement = "expected-wincheck"
      ),
      overwrite = TRUE
    )
  }
}
