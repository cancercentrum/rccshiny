library(shinytest2)

appdir <- file.path("apps", "sv", "app1")

# Initial checks
file_remove_if_exists(file.path(appdir, "app.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))

test_that("creating app1 works", {
  expect_no_error({
    # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-94
    rccShiny2(
      data = rccShinyData,
      folder = "app1",
      path = "apps",
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
  expect_true(file.exists(file.path(appdir, "app.R")))
  expect_true(file.exists(file.path(appdir, "data", "data.RData")))
})


test_that("app1 works", {
  expect_no_error(test_app(appdir))
})

# Clean up
file_remove_if_exists(file.path(appdir, "app.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))
