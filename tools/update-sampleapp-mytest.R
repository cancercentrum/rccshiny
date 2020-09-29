library(shiny)
library(rccShiny)
library(shinytest)

testApp(file.path("inst", "sv", "sampleapp"), "mytest")
# snapshotUpdate(file.path("inst", "sampleapp"), "mytest")
