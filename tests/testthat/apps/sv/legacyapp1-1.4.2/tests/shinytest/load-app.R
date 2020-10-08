app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 60000)
app$snapshotInit("load-app", screenshot = FALSE)

app$snapshot()
