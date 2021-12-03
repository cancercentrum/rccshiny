# TODO Change from sleep (bad) to waitForValue (good), or similar
# https://rstudio.github.io/shinytest/articles/in-depth.html#waiting-for-an-input-or-output-value

app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 60000)
app$snapshotInit("nav-app1", screenshot = FALSE)

app$snapshot()

# app$setInputs(tab = "table")
# # app$waitForValue()
# # Hope the newer report appears within 7 seconds
# Sys.sleep(7)
# app$snapshot()
app$setInputs(tab = "fig_map")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()
app$setInputs(tab = "fig_trend")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()
app$setInputs(tab = "description")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

app$setInputs(tab = "fig_compare")
app$setInputs(param_outcome = "Kontinuerlig")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

# app$setInputs(tab = "fig_compare")
app$setInputs(param_numerictype = "Andel inom ... dagar")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)

# app$setInputs(tab = "fig_compare")
# app$setInputs(param_numerictype = "Andel inom ... dagar")
app$setInputs(param_numerictype_prop = 60)
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

# app$setInputs(tab = "fig_compare")
app$setInputs(param_outcome = "Kategorisk")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

# app$setInputs(tab = "fig_compare")
app$setInputs(param_outcome = "Dikotom")
app$setInputs(param_region = "Stockholm-Gotland")
app$setInputs(param_levelpresent = "Sjukhus")
app$setInputs(param_ownhospital = "Sjukhus 10-1-a")
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

app$setInputs(param_period_year = c(2012, 2016))
app$setInputs(param_periodSplit = TRUE)
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()

app$setInputs(userInput_age = c(18, 80))
app$setInputs(userInput_stage = c("I", "II"))
# app$waitForValue()
# Hope the newer report appears within 7 seconds
Sys.sleep(7)
app$snapshot()
