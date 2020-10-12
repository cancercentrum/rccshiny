library(shinytest)

appdir <- file.path("apps", "sv", "app1hc")

# Initial checks
file_remove_if_exists(file.path(appdir, "app.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))

test_that("creating app1hc works", {
  expect_invisible({
    # Adopted from https://bitbucket.org/cancercentrum/rccshiny/src/1.6.0/R/rccShiny2.R#lines-94
    # with outputHighcharts = TRUE
    rccShiny2(
      data = rccShinyData,
      folder = "app1hc",
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
      ),
      outputHighcharts = TRUE
    )
  })
  expect_true(file.exists(file.path(appdir, "app.R")))
  expect_true(file.exists(file.path(appdir, "data", "data.RData")))
})


test_that("loading app1hc works", {
  skip_on_os("linux")

  expect_pass(testApp(appdir, "load-app", compareImages = FALSE, suffix = shinytest_suffix()))
})

test_that("navigating app1hc works", {
  skip_on_os("linux")

  expect_pass(testApp(appdir, "nav-app1", compareImages = FALSE, suffix = shinytest_suffix()))
})

# Clean up
file_remove_if_exists(file.path(appdir, "app.R"))
file_remove_if_exists(file.path(appdir, "data", "data.RData"))
