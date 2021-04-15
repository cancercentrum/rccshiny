# Preparations
#
# Run update-shinytest-expected-mactest.R

dirs_expected_mactest <- stringr::str_subset(
  list.dirs("tests/testthat/apps/"),
  pattern = "expected-mactest"
)

for (dir_expected_mactest in dirs_expected_mactest) {
  for (file_expected_mactest in list.files(dir_expected_mactest, "*.json",  full.names = TRUE)) {
    file.copy(
      from = file_expected_mactest,
      to = stringr::str_replace(
        file_expected_mactest,
        pattern = "expected-mactest",
        replacement = "expected-maccheck"
      ),
      overwrite = TRUE
    )
  }
}
