
library(shiny)
library(DT)
library(rccShiny)

shinyUI(fluidPage(

  if (!is.null(GLOBAL_gaPath)) { tags$head(tags$script(src = GLOBAL_gaPath)) },

  h2(textOutput("text0")),
  p(
    textOutput("text1"),
    textOutput("text2")
  ),

  fluidRow(
    column(
      width = 3,
      wellPanel(
        conditionalPanel(
          condition = ifelse(length(GLOBAL_outcomeTitle) > 1,"true","false"),
          selectInput(
            inputId = "param_outcome",
            label = rccShinyTXT(language = GLOBAL_language)$outcome,
            choices = GLOBAL_outcomeTitle,
            selected = GLOBAL_outcomeTitle[1]
          )
        ),
        uiOutput("numericTypeInput"),
        uiOutput("numericTypePropInput"),
        uiOutput("regionInput"),
        uiOutput("levelpresentInput"),
        uiOutput("ownhospitalInput"),
        uiOutput("periodInput"),
        uiOutput("periodSplitInput"),
        uiOutput("userInput"),
        uiOutput("funnelPlotInput")
      ),
      h4(ifelse(GLOBAL_comment == "", "", rccShinyTXT(language = GLOBAL_language)$comment)),
      p(GLOBAL_comment)
    ),
    column(
      width = 9,
      uiOutput("theTabs")
    )
  )

))
