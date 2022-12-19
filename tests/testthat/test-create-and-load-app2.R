library(shinytest2)

appdir_sv <- file.path("apps", "sv", "app2")
appdir_en <- file.path("apps", "en", "app2")

# Initial checks
file_remove_if_exists(file.path(appdir_sv, "app.R"))
file_remove_if_exists(file.path(appdir_sv, "data", "data.RData"))
file_remove_if_exists(file.path(appdir_en, "app.R"))
file_remove_if_exists(file.path(appdir_en, "data", "data.RData"))

test_that("creating app2 works", {
  expect_no_error({
    # Create app2
    # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-124
    rccShinyData$outcome1_en <- rccShinyData$outcome1
    rccShiny2(
      language = c("sv", "en"),
      data = rccShinyData,
      folder = "app2",
      path = "apps",
      outcome = "outcome1",
      outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
      textBeforeSubtitle = c("Nagot pa svenska", "Something in English"),
      description = list(
        c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
        c("Description of the indicator", "Important information!", "Information on variables etc.")
      ),
      varOther = list(
        list(
          var = "age",
          label = c("Alder vid diagnos", "Age at diagnosis"),
          choices = c(0, 120)
        )
      ),
      targetValues = c(95, 99)
    )
  })
  expect_true(file.exists(file.path(appdir_sv, "app.R")))
  expect_true(file.exists(file.path(appdir_sv, "data", "data.RData")))
  expect_true(file.exists(file.path(appdir_en, "app.R")))
  expect_true(file.exists(file.path(appdir_en, "data", "data.RData")))
})


test_that("app2 works", {
  expect_no_error(test_app(appdir_sv))
  expect_no_error(test_app(appdir_en))
})

# Clean up
file_remove_if_exists(file.path(appdir_sv, "app.R"))
file_remove_if_exists(file.path(appdir_sv, "data", "data.RData"))
file_remove_if_exists(file.path(appdir_en, "app.R"))
file_remove_if_exists(file.path(appdir_en, "data", "data.RData"))
