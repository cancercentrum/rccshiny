app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 60000)
app$snapshotInit("mytest", screenshot = FALSE)

app$snapshot()
