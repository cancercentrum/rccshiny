library(shinytest)

appdir <- file.path("apps", "sv", "app3")

# Initial checks
file_remove_if_exists(file.path(appdir, "global.R"))
file_remove_if_exists(file.path(appdir, "server.R"))
file_remove_if_exists(file.path(appdir, "ui.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))
file_remove_if_exists(file.path(appdir, "docs", "description.html"))

test_that("creating app3 works", {
  expect_invisible({
    # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-94
    rccShiny(
      data = rccShinyData,
      folder = "app3",
      path = ".",
      outcome = paste0("outcome", 1:3),
      outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
      description = c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
      varOther = list(
        list(
          var = "age",
          label = "Alder vid diagnos"
        ),
        list(
          var = "stage",
          label = "Stadium",
          choices = c("I", "II"),
          selected = "I",
          multiple = TRUE,
          showInTitle = TRUE
        )
      )
    )
  })
  expect_true(file.exists(file.path(appdir, "global.R")))
  expect_true(file.exists(file.path(appdir, "server.R")))
  expect_true(file.exists(file.path(appdir, "ui.R")))
  expect_true(file.exists(file.path(appdir, "data", "data.RData")))
  expect_true(file.exists(file.path(appdir, "docs", "description.html")))
})


test_that("loading app3 works", {
  expect_pass(testApp(appdir, "load-app", compareImages = FALSE, suffix = shinytest_suffix()))
})

# Clean up
file_remove_if_exists(file.path(appdir, "global.R"))
file_remove_if_exists(file.path(appdir, "server.R"))
file_remove_if_exists(file.path(appdir, "ui.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))
file_remove_if_exists(file.path(appdir, "docs", "description.html"))
