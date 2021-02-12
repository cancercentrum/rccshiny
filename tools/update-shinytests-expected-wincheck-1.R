# Preparations
#
# Run update-shinytest-expected-windows.R

dirs_expected_windows <- stringr::str_subset(
  list.dirs("tests/testthat/apps/"),
  pattern = "expected-windows"
)

for (dir_expected_windows in dirs_expected_windows) {
  for (file_expected_windows in list.files(dir_expected_windows, "*.json",  full.names = TRUE)) {
    file.copy(
      from = file_expected_windows,
      to = stringr::str_replace(
        file_expected_windows,
        pattern = "expected-windows",
        replacement = "expected-wincheck"
      ),
      overwrite = TRUE
    )
  }
}
