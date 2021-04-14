dirs_list <- stringr::str_subset(
  list.dirs("tests/testthat/apps/"),
  pattern = "-latest/tests$"
)

for (dir_selected in dirs_list) {
  cmd <- paste(
    "cp -R",
    dir_selected,
    stringr::str_replace(
      dir_selected,
      pattern = "latest",
      replacement = "1.7.1"
    )
  )
  shell(cmd, shell = "powershell")
}
