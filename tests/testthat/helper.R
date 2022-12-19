file_remove_if_exists <- function(x) {
  if (file.exists(x)) file.remove(x)
}
