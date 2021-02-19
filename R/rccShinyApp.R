#' Creates shiny app
#' @description internal function.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyApp <-
  function(
    optionsList = NULL,
    pageTitle = ""
  ) {

    shinyApp(
      ui = shinydashboard::dashboardPage(
        title = pageTitle,
        skin = "black",
        shinydashboard::dashboardHeader(disable = TRUE),
        shinydashboard::dashboardSidebar(disable = TRUE),
        shinydashboard::dashboardBody(
          tags$style(
            HTML("
              .content-wrapper, .right-side {
                background-color: #ffffff;
              }
              .btn-default {
                background-color: #ffffff
              }
              .d-flex { display: flex; }
              .flex-grow-1 { flex-grow: 1; }
              .ml-1 { margin-left: .7em; }
              .check-mark {
                color: #28a745
              }
              .dropdown {
                margin-top: 10px;
              }
              .glyphicon-user {
                color: #0172B7
              }
            ")
          ),
          tags$head(tags$style(HTML(".shiny-notification {position:fixed;top:0px;right:0px;width:300px;}"))),
          if (!is.null(optionsList$gaPath)) { tags$head(tags$script(src = optionsList$gaPath)) },
          h2(htmlOutput("text0")),
          p(
            htmlOutput("text1"),
            htmlOutput("text2")
          ),
          fluidRow(
            column(
              width = 3,
              shinydashboard::box(
                title = NULL,
                status = "primary",
                width = NULL,
                uiOutput("outcomeInput"),
                uiOutput("regionInput"),
                uiOutput("levelpresentInput"),
                uiOutput("ownhospitalInput"),
                uiOutput("numericTypeInput"),
                uiOutput("numericTypePropInput"),
                uiOutput("periodTypeInput"),
                uiOutput("periodInputYear"),
                uiOutput("periodInputQuarter"),
                uiOutput("periodSplitInput"),
                uiOutput("userInput"),
                uiOutput("funnelPlotInput")
              ),
              uiOutput("comment")
            ),
            uiOutput("theTabs")
          )
        )
      ),
      server = function(input, output, session) {

        if (optionsList$inca) {
          withProgress(
            message = "Laddar data och genererar rapport...",
            value = 0,
            {
              if (requireNamespace("INCA", quietly = TRUE)) {
                tryCatch(
                  expr = {
                    incaEnv <- INCA::getDataFrames(shinySession = session)
                    for (i in ls(envir = incaEnv)) {
                      assign(x = i, value = get(i, envir = incaEnv))
                    }
                  },
                  error = function (e) {
                    INCA::loadDataFrames(parseQueryString(isolate(session$clientData$url_search))[['token']])
                  }
                )
              } else {
                warning(
                  "inca = TRUE but INCA pkg not installed (you're probably not on INCA). ",
                  "OK during rccShiny pkg testing."
                )
              }
              if (exists("environmentVariables")) {
                if (!is.null(environmentVariables$UserParentUnitCode))
                  optionsList$incaUserHospital <- environmentVariables$UserParentUnitCode
              }
              incProgress(0.50)
              source(optionsList$incaScript, encoding = "UTF-8", local = TRUE)
              if (!exists("df")) {
                stop("The script '", optionsList$incaScript, "' did not produce a data.frame 'df'", call. = FALSE)
              }
              if (!is.data.frame(df)) {
                stop("The script '", optionsList$incaScript, "' did not produce a data.frame 'df'", call. = FALSE)
              }
              optionsList$data <- df
              incProgress(0.25)
              optionsList <-
                rccShinyCheckData(
                  optionsList = optionsList
                )
              incProgress(0.25)
              if (optionsList$error != "") {
                stop(optionsList$error, call. = FALSE)
              }
            }
          )
        }

        # Hotfix for backward compatibility with apps built before 1.5.0
        if (is.null(optionsList$prob)) {
          optionsList$prob <- c(0.25, 0.50, 0.75)
        }
        if (is.null(optionsList$prob_labels)) {
          optionsList$prob_labels <- c(
            rccShinyTXT(language = optionsList$language)$q1,
            rccShinyTXT(language = optionsList$language)$median,
            rccShinyTXT(language = optionsList$language)$q3
          )
          optionsList$iqrlab <- rccShinyTXT(language = optionsList$language)$iqr
          optionsList$medianiqrlab <- paste(
            optionsList$prob_labels[2],
            rccShinyTXT(language = optionsList$language)$iqr_and,
            optionsList$iqrlab
          )
        }
        if (is.null(optionsList$includeMissingColumn)) {
          optionsList$includeMissingColumn <- FALSE
        }
        if (!("varOtherComparison" %in% names(optionsList))) {
          optionsList["varOtherComparison"] <- list(NULL)
          optionsList$varOtherComparisonVariables <- vector()
          optionsList$varOtherComparisonLabels <- vector()
        }

        for (i in 1:length(optionsList)) {
          assign(x = paste0("GLOBAL_", names(optionsList)[i]), value = optionsList[[i]])
        }

        whichOutcome <-
          reactive({
            ifelse(
              is.null(input$param_outcome),
              1,
              which(GLOBAL_outcomeTitle == input$param_outcome)
            )
          })

        outcomeClassNumeric <-
          reactive({
            GLOBAL_outcomeClass[whichOutcome()] %in% "numeric"
          })

        output$outcomeInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = ifelse(length(GLOBAL_outcomeTitle) > 1, "true", "false"),
                selectInput(
                  inputId = "param_outcome",
                  label = rccShinyTXT(language = GLOBAL_language)$outcome,
                  choices = GLOBAL_outcomeTitle,
                  selected = GLOBAL_outcomeTitle[1],
                  width = "100%"
                )
              )
            )
          })

        output$numericTypeInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = ifelse(GLOBAL_propWithinShow & outcomeClassNumeric(), "true", "false"),
                radioButtons(
                  inputId = "param_numerictype",
                  label = rccShinyTXT(language = GLOBAL_language)$presentation,
                  choices = c(
                    paste0(
                      GLOBAL_prob_labels[2],
                      " (", GLOBAL_propWithinUnit, ")"
                    ),
                    paste(
                      rccShinyTXT(language = GLOBAL_language)$numericchoices_prop,
                      GLOBAL_propWithinUnit
                    )
                  ),
                  selected = paste0(
                    GLOBAL_prob_labels[2],
                    " (", GLOBAL_propWithinUnit, ")"
                  ),
                  width = "100%"
                )
              )
            )
          })

        numericTypeProp <-
          reactive({
            if (is.null(input$param_numerictype)) {
              FALSE
            } else {
              input$param_numerictype == paste(rccShinyTXT(language = GLOBAL_language)$numericchoices_prop, GLOBAL_propWithinUnit)
            }
          })

        output$numericTypePropInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = ifelse(
                  outcomeClassNumeric(),
                  paste0("input.param_numerictype == '", paste(rccShinyTXT(language = GLOBAL_language)$numericchoices_prop, GLOBAL_propWithinUnit), "'"),
                  "false"
                ),
                numericInput(
                  inputId = "param_numerictype_prop",
                  label = NULL,
                  value = GLOBAL_propWithinValue[whichOutcome()],
                  min = 0,
                  max = 1000,
                  step = 1,
                  width = "100%"
                )
              )
            )
          })

        output$regionInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.tab!='list' & ",
                  ifelse(GLOBAL_regionSelection, "true", "false"),
                  " & ",
                  ifelse(GLOBAL_geoUnitsRegionInclude, "true", "false")
                ),
                selectizeInput(
                  inputId = "param_region",
                  label = GLOBAL_regionLabel,
                  choices = c(rccShinyTXT(language = GLOBAL_language)$all,GLOBAL_regionChoices),
                  selected = GLOBAL_regionSelected,
                  multiple = FALSE,
                  width = "100%"
                )
              )
            )
          })

        output$levelpresentInput <-
          renderUI({
            tempChoices <- list()
            tempSumGeo <- sum(GLOBAL_geoUnitsHospitalInclude, GLOBAL_geoUnitsCountyInclude, GLOBAL_geoUnitsRegionInclude)
            if (tempSumGeo > 0) {
              tempChoices[[" "]] <- as.list(c(
                if (GLOBAL_geoUnitsRegionInclude) {rccShinyLevelNames("region", language = GLOBAL_language)},
                if (GLOBAL_geoUnitsCountyInclude) {rccShinyLevelNames(ifelse(GLOBAL_geoUnitsPatient, "county_lkf", "county"), language = GLOBAL_language)},
                if (GLOBAL_geoUnitsHospitalInclude) {rccShinyLevelNames("hospital", language = GLOBAL_language)}
              ))
            }
            tempSumOther <- length(GLOBAL_varOtherComparisonVariables)
            if (tempSumOther > 0) {
              tempChoices[["  "]] <- as.list(GLOBAL_varOtherComparisonLabels)
            }
            if (length(tempChoices) == 1) tempChoices <- unlist(tempChoices, use.names = FALSE)
            tagList(
              conditionalPanel(
                condition =
                  paste0(
                    "input.tab!='fig_trend' & input.tab!='fig_map' & input.tab!='list' & ",
                    ifelse(sum(tempSumGeo, tempSumOther) > 1, "true", "false")
                  ),
                selectInput(
                  inputId = "param_levelpresent",
                  label = rccShinyTXT(language = GLOBAL_language)$levelofcomparison,
                  choices = tempChoices,
                  selected =
                    if (GLOBAL_geoUnitsRegionInclude & GLOBAL_geoUnitsDefault %in% "region") {
                      rccShinyLevelNames("region", language = GLOBAL_language)
                    } else if (GLOBAL_geoUnitsHospitalInclude & GLOBAL_geoUnitsDefault %in% "hospital") {
                      rccShinyLevelNames("hospital", language = GLOBAL_language)
                    } else if (GLOBAL_geoUnitsDefault %in% GLOBAL_varOtherComparisonVariables) {
                      GLOBAL_varOtherComparisonLabels[which(GLOBAL_varOtherComparisonVariables == GLOBAL_geoUnitsDefault)]
                    } else {
                      rccShinyLevelNames(
                        ifelse(
                          GLOBAL_geoUnitsPatient,
                          "county_lkf",
                          "county"
                        ),
                        language = GLOBAL_language
                      )
                    },
                  width = "100%"
                )
              )
            )
          })

        varOtherComparisonChosen <-
          reactive({
            if (is.null(input[["param_levelpresent"]])) {
              FALSE
            } else {
              input[["param_levelpresent"]] %in% GLOBAL_varOtherComparisonLabels
            }
          })

        output$ownhospitalInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.tab!='fig_map' & input.tab!='table_num' & input.tab!='table_pct' & input.tab!='table' & input.tab!='list' & ",
                  ifelse(varOtherComparisonChosen(), "input.tab=='fig_trend'", "true"), " & ",
                  ifelse(GLOBAL_geoUnitsHospitalInclude, "true", "false"), " & ",
                  "!(", ifelse(GLOBAL_geoUnitsPatient, "true", "false"), " & input.param_levelpresent != '", rccShinyLevelNames("hospital", language = GLOBAL_language), "' & input.tab == 'fig_compare')"
                ),
                selectInput(
                  inputId = "param_ownhospital",
                  label = rccShinyTXT(language = GLOBAL_language)$hospitalinterest,
                  choices = hospitalChoices(),
                  selected = ifelse(!is.null(GLOBAL_geoUnitsHospitalSelected), GLOBAL_geoUnitsHospitalSelected, ""),
                  width = "100%"
                )
              )
            )
          })

        periodType <-
          reactive({
            if (GLOBAL_periodDate & length(GLOBAL_periodDateLevel) & !is.null(input[["param_periodtype"]])) {
              if (input[["param_periodtype"]] %in% rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelQuarter) {
                "quarter"
              } else {
                "year"
              }
            } else {
              "year"
            }
          })

        periodValues <-
          reactive({
            if (periodType() == "quarter") {
              GLOBAL_periodValues_quarters
            } else {
              GLOBAL_periodValues
            }
          })

        periodInput <-
          reactive({
            if (periodType() == "quarter") {
              input[["param_period_quarter"]]
            } else {
              input[["param_period_year"]]
            }
          })

        output$periodTypeInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = ifelse(GLOBAL_periodDate & length(GLOBAL_periodDateLevel) > 1, "true", "false"),
                radioButtons(
                  inputId = "param_periodtype",
                  label = if (GLOBAL_periodDate & length(GLOBAL_periodDateLevel) > 1) {GLOBAL_periodLabel} else {NULL},
                  choices = c(
                    if ("year" %in% GLOBAL_periodDateLevel) {rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelYear},
                    if ("quarter" %in% GLOBAL_periodDateLevel) {rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelQuarter}
                  ),
                  width = "100%"
                )
              )
            )
          })

        output$periodInputYear <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.tab!='fig_trend' & ",
                  ifelse(GLOBAL_periodStart == GLOBAL_periodEnd, "false", "true"),
                  " & (",
                  ifelse(
                    length(GLOBAL_periodDateLevel) == 1 & GLOBAL_periodDateLevel[1] == "year",
                    "true",
                    "false"
                  ),
                  " | input.param_periodtype=='",
                  ifelse(
                    rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelYear %in% "\u00c5r", # Fullösning eftersom åäö verkar krångla i JavaScriptkoden för condition
                    "\u00c5r",
                    rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelYear
                  ),
                  "')"
                ),
                sliderInput(
                  inputId = "param_period_year",
                  label = if (GLOBAL_periodDate & length(GLOBAL_periodDateLevel) > 1) {NULL} else {GLOBAL_periodLabel},
                  min = GLOBAL_periodStart,
                  max = GLOBAL_periodEnd,
                  step = 1,
                  ticks = FALSE,
                  value = c(GLOBAL_periodDefaultStart, GLOBAL_periodDefaultEnd),
                  sep = "",
                  width = "100%"
                )
              )
            )
          })
        output$periodInputQuarter <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.tab!='fig_trend' & ",
                  ifelse(utils::head(GLOBAL_periodValues_quarters, 1) == utils::tail(GLOBAL_periodValues_quarters, 1), "false", "true"),
                  " & (",
                  ifelse(
                    length(GLOBAL_periodDateLevel) == 1 & GLOBAL_periodDateLevel[1] == "quarter",
                    "true",
                    "false"
                  ),
                  " | input.param_periodtype=='", rccShinyTXT(language = GLOBAL_language)$periodTypeInputLabelQuarter, "')"
                ),
                shinyWidgets::sliderTextInput(
                  inputId = "param_period_quarter",
                  label = if (GLOBAL_periodDate & length(GLOBAL_periodDateLevel) > 1) {NULL} else {GLOBAL_periodLabel},
                  choices = GLOBAL_periodValues_quarters,
                  selected = c(GLOBAL_periodDefaultStart_quarters, GLOBAL_periodDefaultEnd_quarters),
                  width = "100%"
                )
              )
            )
          })

        output$periodSplitInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "!input.param_funnelplot & input.tab!='fig_trend' & input.tab!='fig_map' & input.tab!='list' & ",
                  if (is.null(periodInput())) {
                    "false"
                  } else if (periodInput()[1] != periodInput()[2]) {
                    "true"
                  } else {
                    "false"
                  }
                ),
                checkboxInput(
                  inputId = "param_periodSplit",
                  label = paste(
                    rccShinyTXT(language = GLOBAL_language)$periodSplit1,
                    tolower(GLOBAL_periodLabel),
                    rccShinyTXT(language = GLOBAL_language)$periodSplit2
                  ),
                  value = FALSE,
                  width = "100%"
                )
              )
            )
          })

        output$userInput <-
          renderUI({
            if (!is.null(GLOBAL_varOther)) {
              varOther <-
                lapply(
                  1:length(GLOBAL_varOther),
                  function(i) {
                    tempList <- GLOBAL_varOther[[i]]
                    if (tempList$classNumeric) {
                      sliderInput(
                        inputId = paste0("userInputId",i),
                        label = tempList$label,
                        min = min(tempList$choices, na.rm = TRUE),
                        max = max(tempList$choices, na.rm = TRUE),
                        step = 1,
                        ticks = FALSE,
                        value = c(
                          min(tempList$selected, na.rm = TRUE),
                          max(tempList$selected, na.rm = TRUE)
                        ),
                        sep = "",
                        width = "100%"
                      )
                    } else {
                      shinyWidgets::pickerInput(
                        inputId = paste0("userInputId", i),
                        label = tempList$label,
                        choices = tempList$choices,
                        selected = tempList$selected,
                        multiple = tempList$multiple,
                        options = list(
                          'actions-box' = TRUE,
                          'deselect-all-text' = rccShinyTXT(language = GLOBAL_language)$deselectAll,
                          'select-all-text' = rccShinyTXT(language = GLOBAL_language)$selectAll,
                          'none-selected-text' = "-"
                        ),
                        width = "100%"
                      )
                    }
                  }
                )
            } else {
              varOther <- list()
            }
            do.call(tagList,varOther)
          })

        output$funnelPlotInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0("input.tab=='fig_compare' & ", if (outcomeClassNumeric()) {ifelse(numericTypeProp() & GLOBAL_funnelplot == TRUE, "true", "false")} else if (GLOBAL_outcomeClass[whichOutcome()] == "factor"  | GLOBAL_funnelplot == FALSE) {"false"} else {"true"}),
                checkboxInput(
                  inputId = "param_funnelplot",
                  label = rccShinyTXT(language = GLOBAL_language)$funnelplot,
                  value = FALSE,
                  width = "100%"
                )
              )
            )
          })

        hospitalChoices <- reactive({
          tempHospitalsVar <- GLOBAL_data$sjukhus
          tempHospitals <- sort(unique(tempHospitalsVar))
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
              if ("sjukhus_alt" %in% colnames(GLOBAL_data)) {
                tempHospitalsVar <- GLOBAL_data$sjukhus_alt
                tempHospitals <- sort(unique(tempHospitalsVar))
              }
              tempHospitals <- tempHospitals[tempHospitals %in% tempHospitalsVar[GLOBAL_data$region %in% input[["param_region"]]]]
            }
          }
          tempHospitals <- c("",tempHospitals)
          tempHospitals
        })

        indTitle <- reactive({
          if (outcomeClassNumeric()) {
            if (numericTypeProp()) {
              paste0(
                GLOBAL_outcomeTitle[whichOutcome()],
                ", ",
                rccShinyTXT(language = GLOBAL_language)$numeric_proportionwithin,
                input$param_numerictype_prop,
                " ",
                GLOBAL_propWithinUnit
              )
            } else {
              GLOBAL_outcomeTitle[whichOutcome()]
            }
          } else {
            GLOBAL_outcomeTitle[whichOutcome()]
          }
        })

        indSubtitlePeriod <- reactive({
          if (GLOBAL_periodInclude){
            tempPeriodInput <- periodInput()
            paste0(
              GLOBAL_periodLabel,
              ": ",
              ifelse(
                tempPeriodInput[1] == tempPeriodInput[2],
                as.character(strong(tempPeriodInput[1])),
                as.character(strong(
                  paste0(
                    tempPeriodInput[1],
                    "-",
                    tempPeriodInput[2]
                  )
                ))
              ),
              ". "
            )
          } else {
            ""
          }
        })

        indSubtitle <-
          function(period=TRUE) {
            paste0(
              ifelse(
                GLOBAL_textBeforeSubtitle != "",
                paste0(GLOBAL_textBeforeSubtitle," "),
                ""
              ),
              ifelse(
                period,
                indSubtitlePeriod(),
                ""
              ),
              ifelse(
                GLOBAL_textAfterSubtitle != "",
                paste0(GLOBAL_textAfterSubtitle," "),
                ""
              )
            )
          }

        indSubtitleUserInput <- reactive({
          tempText <- ""
          if (!is.null(GLOBAL_varOther)) {
            for (i in 1:length(GLOBAL_varOther)) {
              tempList <- GLOBAL_varOther[[i]]
              tempValues <- input[[paste0("userInputId",i)]]
              if (tempList$showInTitle) {
                if (tempList$classNumeric) {
                  if (!(min(tempList$choices) %in% tempValues[1] &
                        max(tempList$choices) %in% tempValues[2])){
                    tempText <-
                      paste0(
                        tempText,
                        tempList$label,
                        ": ",
                        ifelse(
                          tempValues[1] == tempValues[2],
                          as.character(strong(tempValues[1])),
                          as.character(strong(
                            paste0(
                              tempValues[1],
                              "-",
                              tempValues[2]
                            )
                          ))
                        ),
                        ". "
                      )
                  } else {
                    tempText <- ""
                  }
                } else if (!(all(tempList$choices %in% tempValues))) {
                  tempText <-
                    paste0(
                      tempText,
                      tempList$label,
                      ": ",
                      as.character(strong(paste(tempValues,collapse = " / "))),
                      ". "
                    )
                }
              }
            }
          }
          tempText
        })

        output$text0 <- renderText({
          indTitle()
        })

        output$text1 <- renderText({
          indSubtitle(period = !(input$tab == "fig_trend"))
        })

        output$text2 <- renderText({
          indSubtitleUserInput()
        })

        output$comment <- renderUI({
          if (GLOBAL_comment != "") {
            shinydashboard::box(
              title = NULL,
              status = "primary",
              width = "100%",
              p(GLOBAL_comment)
            )
          }
        })

        output$tableTitle <- renderText({
          indTitle()
        })

        output$theTabs <-
          renderUI({
            theTabs <- list()

            if ("compare" %in% GLOBAL_includeTabs) {
              if (GLOBAL_outputHighcharts) {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_compare, value = "fig_compare", highcharter::highchartOutput("indPlot", height = "980px"), icon = icon("bar-chart"))
              } else {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_compare, value = "fig_compare", plotOutput("indPlot", height = "auto"), icon = icon("bar-chart"))
              }
            }
            if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {
              if ("table" %in% GLOBAL_includeTabs) {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_n, value = "table_num", DT::dataTableOutput("indTableNum"), icon = icon("table"))
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_p, value = "table_pct", DT::dataTableOutput("indTablePct"), icon = icon("table"))
              }
            } else {
              if ("table" %in% GLOBAL_includeTabs) {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab, value = "table", DT::dataTableOutput("indTable"), icon = icon("table"))
              }
              if (GLOBAL_geoUnitsCountyInclude & "map" %in% GLOBAL_includeTabs) {
                if (GLOBAL_outputHighcharts) {
                  theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$map, value = "fig_map", highcharter::highchartOutput("indMap", height = "980px"), icon = icon("map-marker"))
                } else {
                  theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$map, value = "fig_map", plotOutput("indMap", height = "auto"), icon = icon("map-marker"))
                }
              }
            }
            if (GLOBAL_periodInclude & "trend" %in% GLOBAL_includeTabs) {
              if (GLOBAL_outputHighcharts) {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_trend, value = "fig_trend", highcharter::highchartOutput("indPlotTrend", height = "630px"), icon = icon("line-chart"))
              } else {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_trend, value = "fig_trend", plotOutput("indPlotTrend", height = "auto"), icon = icon("line-chart"))
              }
            }
            if (GLOBAL_inca & GLOBAL_incaIncludeList) {
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$list, value = "list", DT::dataTableOutput("indList"), icon = icon("list"))
            }
            if ("description" %in% GLOBAL_includeTabs) {
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$description, value = "description", htmlOutput("description"), icon = icon("info-circle"))
            }
            do.call(shinydashboard::tabBox, c(theTabs, id = "tab", width = 9))
          })

        dfInput <- reactive({

          dftemp <- GLOBAL_data

          dftemp$outcome <- dftemp[,GLOBAL_outcome[whichOutcome()]]
          if (outcomeClassNumeric()) {
            if (GLOBAL_outcomeNumericExcludeNeg)
              dftemp$outcome[!is.na(dftemp$outcome) & dftemp$outcome < 0] <- NA
            if (numericTypeProp())
              dftemp$outcome <- dftemp$outcome <= input$param_numerictype_prop
          }
          if (!GLOBAL_includeMissingColumn){
            dftemp <- subset(dftemp, !is.na(outcome))
          }


          selectionPeriods <- periodValues()
          if (GLOBAL_periodDate & periodType() == "quarter") {
            selectionPeriods <- selectionPeriods[which(selectionPeriods == input[["param_period_quarter"]][1]):which(selectionPeriods == input[["param_period_quarter"]][2])]
          } else {
            dftemp$period <- as.numeric(substr(dftemp$period, 1, 4))
            selectionPeriods <- selectionPeriods[which(selectionPeriods == input[["param_period_year"]][1]):which(selectionPeriods == input[["param_period_year"]][2])]
          }

          if (input$tab != "fig_trend") {
            dftemp <-
              subset(
                dftemp,
                !is.na(period) & period %in% selectionPeriods
              )
          }

          if (input$tab == "list") {
            dftemp$group <- dftemp[, "sjukhus"]
            dftemp$groupCode <- dftemp[, "sjukhuskod"]
          } else {
            if (!(all(rccShinyRegionNames(language = GLOBAL_language)[4:5] %in% input[["param_region"]])) & (rccShinyRegionNames(language = GLOBAL_language)[4] %in% input[["param_region"]] | rccShinyRegionNames(language = GLOBAL_language)[5] %in% input[["param_region"]])) {
              dftemp$landsting[dftemp$landsting == "Halland" & dftemp$region == rccShinyRegionNames(language = GLOBAL_language)[4]] <- "S\u00f6dra Halland"
              dftemp$landsting[dftemp$landsting == "Halland" & dftemp$region == rccShinyRegionNames(language = GLOBAL_language)[5]] <- "Norra Halland"
            }

            if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
              if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                if ("sjukhus_alt" %in% colnames(GLOBAL_data)) {
                  dftemp$sjukhus <- dftemp$sjukhus_alt
                }
              }
            }

            dftemp$group <- dftemp[,rccShinyGroupVariable(label = input$param_levelpresent, otherVariables = GLOBAL_varOtherComparisonVariables, otherLabels = GLOBAL_varOtherComparisonLabels)]
            dftemp$group_ownhospital <- dftemp[,"sjukhus"] == input$param_ownhospital
            dftemp$groupCode <- rep(NA, nrow(dftemp))
          }

          if (!is.null(GLOBAL_varOther)) {
            for (i in 1:length(GLOBAL_varOther)) {
              if (GLOBAL_varOther[[i]]$classNumeric) {
                dftemp <- dftemp[!is.na(dftemp[,GLOBAL_varOther[[i]]$var]) & dftemp[,GLOBAL_varOther[[i]]$var] %in% input[[paste0("userInputId",i)]][1]:input[[paste0("userInputId",i)]][2],]
              } else {
                dftemp <- dftemp[!is.na(dftemp[,GLOBAL_varOther[[i]]$var]) & dftemp[,GLOBAL_varOther[[i]]$var] %in% input[[paste0("userInputId",i)]],]
              }
            }
          }

          dftemp

        })

        hallandLabel <- reactive({
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (
              rccShinyRegionNames(language = GLOBAL_language)[4] %in% input[["param_region"]] &
              !(rccShinyRegionNames(language = GLOBAL_language)[5] %in% input[["param_region"]])
            ) {
              hallandLabel <- "S\u00f6dra Halland"
            } else if (
              rccShinyRegionNames(language = GLOBAL_language)[5] %in% input[["param_region"]] &
              !(rccShinyRegionNames(language = GLOBAL_language)[4] %in% input[["param_region"]])
            ) {
              hallandLabel <- "Norra Halland"
            } else {
              hallandLabel <- "Halland"
            }
          } else {
            hallandLabel <- "Halland"
          }
        })

        emphLabelReactive <- reactive({
          data.frame(
            param_ownhospital=input$param_ownhospital,
            param_levelpresent=input$param_levelpresent
          )
        })

        emphLabel <-
          function(data) {
            tempEmphLabelReactive <- emphLabelReactive()
            if (tempEmphLabelReactive$param_levelpresent == rccShinyLevelNames("hospital",language = GLOBAL_language)) {
              emph_lab <- tempEmphLabelReactive$param_ownhospital
            } else if (GLOBAL_geoUnitsPatient) {
              emph_lab <- ""
            } else if (tempEmphLabelReactive$param_levelpresent == rccShinyLevelNames("county",language = GLOBAL_language) & nrow(data) > 0) {
              emph_lab <- data$landsting[data$sjukhus == tempEmphLabelReactive$param_ownhospital][1]
              if (!is.na(emph_lab) & emph_lab == "Halland") {
                emph_lab <- hallandLabel()
              }
            } else if (tempEmphLabelReactive$param_levelpresent == rccShinyLevelNames("region",language = GLOBAL_language) & nrow(data) > 0) {
              emph_lab <- data$region[data$sjukhus == tempEmphLabelReactive$param_ownhospital][1]
            } else {
              emph_lab <- ""
            }
            emph_lab
          }

        output$indPlot <-

          if (GLOBAL_outputHighcharts) {

            highcharter::renderHighchart({

              dfuse <- dfInput()

              tempSubset <- NULL
              if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
                if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                  tempSubset <- dfuse$region %in% input[["param_region"]]
                }
              }

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {

                rcc2PlotInd(
                  group = dfuse$group,
                  groupHideLessThan = GLOBAL_hideLessThan,
                  groupHideLessThanLabel = rccShinyTXT(language = GLOBAL_language)$grouphidelessthan,
                  groupHideLessThanCell = GLOBAL_hideLessThanCell,
                  allLab = GLOBAL_allLabel,
                  emphLab = emphLabel(dfuse),
                  ind = dfuse$outcome,
                  indNumericExcludeNeg = FALSE,
                  indNumericPercentiles = GLOBAL_prob,
                  indTitle = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    GLOBAL_prob_labels[2],
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  indNCasesTxt = rccShinyTXT(language = GLOBAL_language)$noofcases,
                  indNCasesOfTxt = rccShinyTXT(language = GLOBAL_language)$noofcases_nOfN,
                  period = if (input$param_periodSplit) {dfuse$period} else {NULL},
                  xLab = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    paste0(
                      GLOBAL_medianiqrlab,
                      " (", GLOBAL_propWithinUnit, ")"),
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  legendFixedTextWidth = TRUE,
                  cexText = 1 - 0.2 * min(max((length(unique(dfuse$group)) - 30) / 30, 0), 1),
                  cexPoint = 3 - 1.2 * min(max((length(unique(dfuse$group)) - 30) / 30, 0), 1),
                  targetValues = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                     GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                     numericTypeProp() &
                                     input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                    GLOBAL_targetValues[[whichOutcome()]]} else {
                      NULL
                    },
                  targetValuesHigh = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                         GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                         numericTypeProp() &
                                         input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                    GLOBAL_sortDescending[whichOutcome()]} else {
                      NULL
                    },
                  targetValuesLabels = c(
                    rccShinyTXT(language = GLOBAL_language)$targetValuesLabelIntermediate,
                    rccShinyTXT(language = GLOBAL_language)$targetValuesLabelHigh
                  ),
                  funnelplot = input$param_funnelplot,
                  sort = GLOBAL_sort,
                  subset = tempSubset,
                  subsetLab = paste(input[["param_region"]], collapse = "/"),
                  outputHighchart = GLOBAL_outputHighcharts
                )

              }

            })

          } else {

            renderImage({

              x_width <- min(session$clientData$output_indPlot_width,700)
              yx_ratio <- 1.4

              dfuse <- dfInput()

              tempSubset <- NULL
              if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
                if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                  tempSubset <- dfuse$region %in% input[["param_region"]]
                }
              }

              outfile <- tempfile(fileext = ".png")

              grDevices::png(filename = outfile, width = 9,height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {
                rcc2PlotInd(
                  group = dfuse$group,
                  groupHideLessThan = GLOBAL_hideLessThan,
                  groupHideLessThanLabel = rccShinyTXT(language = GLOBAL_language)$grouphidelessthan,
                  groupHideLessThanCell = GLOBAL_hideLessThanCell,
                  allLab = GLOBAL_allLabel,
                  emphLab = emphLabel(dfuse),
                  ind = dfuse$outcome,
                  indNumericExcludeNeg = FALSE,
                  indNumericPercentiles = GLOBAL_prob,
                  indTitle = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    GLOBAL_prob_labels[2],
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  indNCasesTxt = rccShinyTXT(language = GLOBAL_language)$noofcases,
                  indNCasesOfTxt = rccShinyTXT(language = GLOBAL_language)$noofcases_nOfN,
                  period = if (input$param_periodSplit) {dfuse$period} else {NULL},
                  xLab = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    paste0(
                      GLOBAL_medianiqrlab,
                      " (", GLOBAL_propWithinUnit, ")"),
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  legendFixedTextWidth = TRUE,
                  cexText = 1 - 0.2 * min(max((length(unique(dfuse$group)) - 30) / 30, 0), 1),
                  cexPoint = 3 - 1.2 * min(max((length(unique(dfuse$group)) - 30) / 30, 0), 1),
                  targetValues = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                     GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                     numericTypeProp() &
                                     input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                    GLOBAL_targetValues[[whichOutcome()]]} else {
                      NULL
                    },
                  targetValuesHigh = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                         GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                         numericTypeProp() &
                                         input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                    GLOBAL_sortDescending[whichOutcome()]} else {
                      NULL
                    },
                  targetValuesLabels = c(
                    rccShinyTXT(language = GLOBAL_language)$targetValuesLabelIntermediate,
                    rccShinyTXT(language = GLOBAL_language)$targetValuesLabelHigh
                  ),
                  funnelplot = input$param_funnelplot,
                  sort = GLOBAL_sort,
                  subset = tempSubset,
                  subsetLab = paste(input[["param_region"]], collapse = "/")
                )
              } else {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)
                text(1, 1, rccShinyNoObservationsText(language = GLOBAL_language))
              }

              grDevices::dev.off()

              list(src = outfile,
                   contentType = "image/png",
                   width = x_width,
                   height = x_width * yx_ratio)

            }, deleteFile = TRUE)

          }

        output$indPlotTrend <-

          if (GLOBAL_outputHighcharts) {

            highcharter::renderHighchart({

              dfuse <- dfInput()

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {

                tab <-
                  rccShinyIndTable(
                    group = dfuse[, rccShinyGroupVariable("hospital")],
                    group_hide_less_than = GLOBAL_hideLessThan,
                    all_lab = GLOBAL_allLabel,
                    ind = dfuse$outcome,
                    ind_factor_pct = GLOBAL_outcomeClass[whichOutcome()] == "factor",
                    ind_numeric_percentiles = GLOBAL_prob,
                    lab_percentiles = GLOBAL_prob_labels,
                    period = dfuse$period,
                    period_factors = periodValues(),
                    period_alwaysinclude = TRUE
                  )

                tab_group <- subset(tab,group == input$param_ownhospital)
                tab_total <- subset(tab,group == GLOBAL_allLabel)

                tab <- rbind(tab_total, tab_group)

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  if (nrow(tab_group) > 0) {
                    yx_ratio <- 1.8
                  }

                } else if (GLOBAL_geoUnitsRegionInclude) {

                  tab_region <-
                    rccShinyIndTable(
                      group = dfuse[,rccShinyGroupVariable("sjukv\u00e5rdsregion")],
                      group_hide_less_than = GLOBAL_hideLessThan,
                      all_lab = NULL,
                      ind = dfuse$outcome,
                      ind_numeric_percentiles = GLOBAL_prob,
                      lab_percentiles = GLOBAL_prob_labels,
                      period = dfuse$period,
                      period_factors = periodValues(),
                      period_alwaysinclude = TRUE
                    )
                  if (!(rccShinyTXT(language = optionsList$language)$all %in% input[["param_region"]])) {
                    tab_region <-
                      subset(
                        tab_region,
                        group %in% input[["param_region"]]
                      )
                  }
                  tab <- rbind(tab_region, tab)

                }

              }

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  x <- list()
                  y <- list()
                  legend <- vector()

                  for (i in levels(dfuse$outcome)) {
                    x <- append(x, list(tab_total$Period))
                    y <- append(y, list(as.numeric(tab_total[,i])))
                    legend <- c(legend, i)
                  }

                  rcc2PlotLine(
                    x = x,
                    y = y,
                    legend = legend,
                    xLim = range(tab_total$Period),
                    xBy = 1,
                    yLim = range(pretty(c(0, max(unlist(y), na.rm = TRUE)))),
                    title = GLOBAL_allLabel,
                    subtitle1 = NULL,
                    subtitle2 = NULL,
                    xLab = GLOBAL_periodLabel,
                    yLab = rccShinyTXT(language = GLOBAL_language)$percent,
                    outputHighchart = TRUE
                  )

                } else {

                  x <- list()
                  y <- list()
                  legend <- vector()

                  if (outcomeClassNumeric() & !numericTypeProp()) {
                    y_varinterest <- GLOBAL_prob_labels[2]
                    y_varinterest_txt <- paste0(
                      GLOBAL_prob_labels[2],
                      " (", GLOBAL_propWithinUnit, ")")

                  } else {
                    y_varinterest <- "Procent"
                    y_varinterest_txt <- rccShinyTXT(language = GLOBAL_language)$percent
                  }

                  for (i in unique(tab$group)) {
                    x <- append(x, list(tab$Period[tab$group == i]))
                    y <- append(y, list(as.numeric(tab[tab$group == i, y_varinterest])))
                    legend <- c(legend, i)
                  }

                  master_colshade <- 1.6
                  master_col <- c(
                    rcc2ColShade("#005092", master_colshade),
                    rcc2ColShade("#e56284", master_colshade),
                    rcc2ColShade("#66cccc", master_colshade),
                    rcc2ColShade("#7f3705", master_colshade),
                    rcc2ColShade("#7c458a", master_colshade),
                    rcc2ColShade("#95bf5d", master_colshade),
                    "#ffb117",
                    "#db5524",
                    "#19975d"
                  )

                  col <- rep("#000000", length(legend))
                  tempRegionNames <- sort(rccShinyRegionNames(language = GLOBAL_language)[1:6])
                  col[legend == tempRegionNames[1]] <- master_col[1]
                  col[legend == tempRegionNames[2]] <- master_col[2]
                  col[legend == tempRegionNames[3]] <- master_col[3]
                  col[legend == tempRegionNames[4]] <- master_col[4]
                  col[legend == tempRegionNames[5]] <- master_col[5]
                  col[legend == tempRegionNames[6]] <- master_col[6]
                  col[legend == GLOBAL_allLabel] <- master_col[7]
                  col[legend == input$param_ownhospital] <- master_col[8]
                  col[legend %in% input[["param_region"]]] <- master_col[9]

                  rcc2PlotLine(
                    x = x,
                    y = y,
                    legend = legend,
                    legendTextWidth = 15,
                    xLim = range(tab$Period),
                    xBy = 1,
                    yLim = range(
                      pretty(
                        c(0,
                          ifelse(
                            y_varinterest == GLOBAL_prob_labels[2],
                            max(unlist(y),na.rm = TRUE),
                            100
                          )
                        )
                      )
                    ),
                    xLab = GLOBAL_periodLabel,
                    yLab = y_varinterest_txt,
                    targetValues = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                       GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                       numericTypeProp() &
                                       input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                      GLOBAL_targetValues[[whichOutcome()]]} else {
                        NULL
                      },
                    targetValuesHigh = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                           GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                           numericTypeProp() &
                                           input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                      GLOBAL_sortDescending[whichOutcome()]} else {
                        NULL
                      },
                    targetValuesLabels = c(
                      rccShinyTXT(language = GLOBAL_language)$targetValuesLabelIntermediate,
                      rccShinyTXT(language = GLOBAL_language)$targetValuesLabelHigh
                    ),
                    col = col,
                    outputHighchart = TRUE
                  )

                }

              }

            })

          } else {

            renderImage({

              x_width <- min(session$clientData$output_indPlotTrend_width, 700)
              yx_ratio <- 0.9

              dfuse <- dfInput()

              outfile <- tempfile(fileext = ".png")

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {

                tab <-
                  rccShinyIndTable(
                    group = dfuse[, rccShinyGroupVariable("hospital")],
                    group_hide_less_than = GLOBAL_hideLessThan,
                    all_lab = GLOBAL_allLabel,
                    ind = dfuse$outcome,
                    ind_factor_pct = GLOBAL_outcomeClass[whichOutcome()] == "factor",
                    ind_numeric_percentiles = GLOBAL_prob,
                    lab_percentiles = GLOBAL_prob_labels,
                    period = dfuse$period,
                    period_factors = periodValues(),
                    period_alwaysinclude = TRUE
                  )

                tab_group <- subset(tab,group == input$param_ownhospital)
                tab_total <- subset(tab,group == GLOBAL_allLabel)

                tab <- rbind(tab_total, tab_group)

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  if (nrow(tab_group) > 0) {
                    yx_ratio <- 1.8
                  }

                } else if (GLOBAL_geoUnitsRegionInclude) {

                  tab_region <-
                    rccShinyIndTable(
                      group = dfuse[,rccShinyGroupVariable("sjukv\u00e5rdsregion")],
                      group_hide_less_than = GLOBAL_hideLessThan,
                      all_lab = NULL,
                      ind = dfuse$outcome,
                      ind_numeric_percentiles = GLOBAL_prob,
                      lab_percentiles = GLOBAL_prob_labels,
                      period = dfuse$period,
                      period_factors = periodValues(),
                      period_alwaysinclude = TRUE
                    )
                  if (!(rccShinyTXT(language = optionsList$language)$all %in% input[["param_region"]])) {
                    tab_region <-
                      subset(
                        tab_region,
                        group %in% input[["param_region"]]
                      )
                  }
                  tab <- rbind(tab_region, tab)

                }

              }

              grDevices::png(filename = outfile, width = 9, height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  if (nrow(tab_group) > 0) {

                    par(mfrow = c(2, 1))

                    x <- list()
                    y <- list()
                    legend <- vector()

                    for (i in levels(dfuse$outcome)) {
                      x <- append(x, list(tab_group$Period))
                      y <- append(y, list(as.numeric(tab_group[,i])))
                      legend <- c(legend, i)
                    }

                    rcc2PlotLine(
                      x = x,
                      y = y,
                      legend = legend,
                      xLim = range(tab_group$Period),
                      xBy = 1,
                      yLim = range(pretty(c(0, max(unlist(y), na.rm = TRUE)))),
                      title = input$param_ownhospital,
                      subtitle1 = NULL,
                      subtitle2 = NULL,
                      xLab = GLOBAL_periodLabel,
                      yLab = rccShinyTXT(language = GLOBAL_language)$percent
                    )

                  }

                  x <- list()
                  y <- list()
                  legend <- vector()

                  for (i in levels(dfuse$outcome)) {
                    x <- append(x, list(tab_total$Period))
                    y <- append(y, list(as.numeric(tab_total[,i])))
                    legend <- c(legend, i)
                  }

                  rcc2PlotLine(
                    x = x,
                    y = y,
                    legend = legend,
                    xLim = range(tab_total$Period),
                    xBy = 1,
                    yLim = range(pretty(c(0, max(unlist(y), na.rm = TRUE)))),
                    title = GLOBAL_allLabel,
                    subtitle1 = NULL,
                    subtitle2 = NULL,
                    xLab = GLOBAL_periodLabel,
                    yLab = rccShinyTXT(language = GLOBAL_language)$percent
                  )

                } else {

                  x <- list()
                  y <- list()
                  legend <- vector()

                  if (outcomeClassNumeric() & !numericTypeProp()) {
                    y_varinterest <- GLOBAL_prob_labels[2]
                    y_varinterest_txt <- paste0(
                      GLOBAL_prob_labels[2],
                      " (", GLOBAL_propWithinUnit, ")")

                  } else {
                    y_varinterest <- "Procent"
                    y_varinterest_txt <- rccShinyTXT(language = GLOBAL_language)$percent
                  }

                  for (i in unique(tab$group)) {
                    x <- append(x, list(tab$Period[tab$group == i]))
                    y <- append(y, list(as.numeric(tab[tab$group == i, y_varinterest])))
                    legend <- c(legend, i)
                  }

                  master_colshade <- 1.6
                  master_col <- c(
                    rcc2ColShade("#005092", master_colshade),
                    rcc2ColShade("#e56284", master_colshade),
                    rcc2ColShade("#66cccc", master_colshade),
                    rcc2ColShade("#7f3705", master_colshade),
                    rcc2ColShade("#7c458a", master_colshade),
                    rcc2ColShade("#95bf5d", master_colshade),
                    "#ffb117",
                    "#db5524",
                    "#19975d"
                  )

                  col <- rep("#000000", length(legend))
                  tempRegionNames <- sort(rccShinyRegionNames(language = GLOBAL_language)[1:6])
                  col[legend == tempRegionNames[1]] <- master_col[1]
                  col[legend == tempRegionNames[2]] <- master_col[2]
                  col[legend == tempRegionNames[3]] <- master_col[3]
                  col[legend == tempRegionNames[4]] <- master_col[4]
                  col[legend == tempRegionNames[5]] <- master_col[5]
                  col[legend == tempRegionNames[6]] <- master_col[6]
                  col[legend == GLOBAL_allLabel] <- master_col[7]
                  col[legend == input$param_ownhospital] <- master_col[8]
                  col[legend %in% input[["param_region"]]] <- master_col[9]

                  rcc2PlotLine(
                    x = x,
                    y = y,
                    legend = legend,
                    legendTextWidth = 15,
                    xLim = range(tab$Period),
                    xBy = 1,
                    yLim = range(
                      pretty(
                        c(0,
                          ifelse(
                            y_varinterest == GLOBAL_prob_labels[2],
                            max(unlist(y),na.rm = TRUE),
                            100
                          )
                        )
                      )
                    ),
                    xLab = GLOBAL_periodLabel,
                    yLab = y_varinterest_txt,
                    targetValues = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                       GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                       numericTypeProp() &
                                       input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                      GLOBAL_targetValues[[whichOutcome()]]} else {
                        NULL
                      },
                    targetValuesHigh = if (GLOBAL_outcomeClass[whichOutcome()] == "logical" |
                                           GLOBAL_outcomeClass[whichOutcome()] == "numeric" &
                                           numericTypeProp() &
                                           input$param_numerictype_prop == GLOBAL_propWithinValue[whichOutcome()]) {
                      GLOBAL_sortDescending[whichOutcome()]} else {
                        NULL
                      },
                    targetValuesLabels = c(
                      rccShinyTXT(language = GLOBAL_language)$targetValuesLabelIntermediate,
                      rccShinyTXT(language = GLOBAL_language)$targetValuesLabelHigh
                    ),
                    col = col
                  )

                }

              } else {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)
                text(1, 1, rccShinyNoObservationsText(language = GLOBAL_language))
              }

              grDevices::dev.off()

              list(src = outfile,
                   contentType = "image/png",
                   width = x_width,
                   height = x_width * yx_ratio)

            }, deleteFile = TRUE)

          }

        output$indTableNum <-
          DT::renderDataTable({

            dfuse <- dfInput()

            tempSubset <- NULL
            if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
              if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                tempSubset <- dfuse$region %in% input[["param_region"]]
              }
            }

            if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] == "factor") {
              tempPeriodInput <- periodInput()
              if (!input$param_periodSplit & tempPeriodInput[1] != tempPeriodInput[2]) {
                dfuse$period <-
                  paste0(
                    tempPeriodInput[1],
                    "-",
                    tempPeriodInput[2]
                  )
              }

              tab <-
                rccShinyIndTable(
                  language = GLOBAL_language,
                  group = dfuse$group,
                  group_hide_less_than = GLOBAL_hideLessThan,
                  group_hide_less_than_cell = GLOBAL_hideLessThanCell,
                  all_lab = GLOBAL_allLabel,
                  ind_numeric_percentiles = GLOBAL_prob,
                  lab_percentiles = GLOBAL_prob_labels,
                  ind = dfuse$outcome,
                  period = dfuse$period,
                  period_alwaysinclude = GLOBAL_periodInclude,
                  lab_period = GLOBAL_periodLabel,
                  subset = tempSubset,
                  subset_lab = paste(input[["param_region"]], collapse = "/"),
                  include_missing_column = GLOBAL_includeMissingColumn
                )

              colnames(tab)[1] <- input$param_levelpresent
            } else {
              tab <-
                subset(
                  data.frame(
                    rccShinyNoObservationsText(language = GLOBAL_language)
                  ),
                  FALSE
                )
              colnames(tab) <- rccShinyTXT(language = GLOBAL_language)$message
            }

            tempColumnDefs <-
              list(
                list(
                  className = 'dt-left',
                  targets = 0
                )
              )
            if (ncol(tab) > 1) {
              tempColumnDefs[[2]] <-
                list(
                  className = 'dt-right',
                  targets = 1:(ncol(tab)-1)
                )
            }

            tab <-
              DT::datatable(
                tab,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  columnDefs = tempColumnDefs,
                  language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
                  searching = TRUE,
                  paging = FALSE,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = list(
                    list(extend = 'excel', filename = indTitle(), title = indTitle()),
                    list(extend = 'pdf', filename = indTitle(), title = indTitle()),
                    list(extend = 'print', title = indTitle())
                  )
                )
              )

            tab
          })

        output$indTablePct <-
          DT::renderDataTable({

            dfuse <- dfInput()

            tempSubset <- NULL
            if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
              if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                tempSubset <- dfuse$region %in% input[["param_region"]]
              }
            }

            if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] == "factor") {
              tempPeriodInput <- periodInput()
              if (!input$param_periodSplit & tempPeriodInput[1] != tempPeriodInput[2]) {
                dfuse$period <-
                  paste0(
                    tempPeriodInput[1],
                    "-",
                    tempPeriodInput[2]
                  )
              }

              tab <-
                rccShinyIndTable(
                  language = GLOBAL_language,
                  group = dfuse$group,
                  group_hide_less_than = GLOBAL_hideLessThan,
                  group_hide_less_than_cell = GLOBAL_hideLessThanCell,
                  all_lab = GLOBAL_allLabel,
                  ind = dfuse$outcome,
                  ind_factor_pct = TRUE,
                  ind_numeric_percentiles = GLOBAL_prob,
                  lab_percentiles = GLOBAL_prob_labels,
                  period = dfuse$period,
                  period_alwaysinclude = GLOBAL_periodInclude,
                  lab_period = GLOBAL_periodLabel,
                  subset = tempSubset,
                  subset_lab = paste(input[["param_region"]], collapse = "/")
                )

              colnames(tab)[1] <- input$param_levelpresent
              colnames(tab)[3:ncol(tab)] <- paste(colnames(tab)[3:ncol(tab)],"(%)")
            } else {
              tab <-
                subset(
                  data.frame(
                    rccShinyNoObservationsText(language = GLOBAL_language)
                  ),
                  FALSE
                )
              colnames(tab) <- rccShinyTXT(language = GLOBAL_language)$message
            }

            tempColumnDefs <-
              list(
                list(
                  className = 'dt-left',
                  targets = 0
                )
              )
            if (ncol(tab) > 1) {
              tempColumnDefs[[2]] <-
                list(
                  className = 'dt-right',
                  targets = 1:(ncol(tab)-1)
                )
            }

            tab <-
              DT::datatable(
                tab,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  columnDefs = tempColumnDefs,
                  language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
                  searching = TRUE,
                  paging = FALSE,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = list(
                    list(extend = 'excel', filename = indTitle(), title = indTitle()),
                    list(extend = 'pdf', filename = indTitle(), title = indTitle()),
                    list(extend = 'print', title = indTitle())
                  )
                )
              )

            tab
          })

        output$indTable <-
          DT::renderDataTable({

            dfuse <- dfInput()

            tempSubset <- NULL
            if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
              if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                tempSubset <- dfuse$region %in% input[["param_region"]]
              }
            }

            if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] != "factor") {
              tempPeriodInput <- periodInput()
              if (!input$param_periodSplit & tempPeriodInput[1] != tempPeriodInput[2]) {
                dfuse$period <-
                  paste0(
                    tempPeriodInput[1],
                    "-",
                    tempPeriodInput[2]
                  )
              }

              tab <-
                rccShinyIndTable(
                  language = GLOBAL_language,
                  group = dfuse$group,
                  group_hide_less_than = GLOBAL_hideLessThan,
                  group_hide_less_than_cell = GLOBAL_hideLessThanCell,
                  all_lab = GLOBAL_allLabel,
                  ind = dfuse$outcome,
                  ind_numeric_percentiles = GLOBAL_prob,
                  lab_percentiles = GLOBAL_prob_labels,
                  period = dfuse$period,
                  period_alwaysinclude = GLOBAL_periodInclude,
                  lab_period = GLOBAL_periodLabel,
                  subset = tempSubset,
                  subset_lab = paste(input[["param_region"]], collapse = "/"),
                  include_missing_column = GLOBAL_includeMissingColumn
                )

              colnames(tab)[1] <- input$param_levelpresent
            } else {
              tab <-
                subset(
                  data.frame(
                    rccShinyNoObservationsText(language = GLOBAL_language)
                  ),
                  FALSE
                )
              colnames(tab) <- rccShinyTXT(language = GLOBAL_language)$message
            }

            tempColumnDefs <-
              list(
                list(
                  className = 'dt-left',
                  targets = 0
                )
              )
            if (ncol(tab) > 1) {
              tempColumnDefs[[2]] <-
                list(
                  className = 'dt-right',
                  targets = 1:(ncol(tab)-1)
                )
            }

            tab <-
              DT::datatable(
                tab,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  columnDefs = tempColumnDefs,
                  language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
                  searching = TRUE,
                  paging = FALSE,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = list(
                    list(extend = 'excel', filename = indTitle(), title = indTitle()),
                    list(extend = 'pdf', filename = indTitle(), title = indTitle()),
                    list(extend = 'print', title = indTitle())
                  )
                )
              )

            tab
          })

        output$indMap <-

          if (GLOBAL_outputHighcharts) {

            highcharter::renderHighchart({

              tab_order <- rcc2PlotMap(valueOrderReturn = TRUE)

              tab_order[tab_order == "Halland"] <- hallandLabel()

              dfuse <- dfInput()

              if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
                if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                  dfuse <- subset(dfuse, region %in% input[["param_region"]])
                }
              }

              dfuse$group <- dfuse[, rccShinyGroupVariable(label = "region")]

              dfuse <- subset(dfuse,group %in% tab_order)

              if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] != "factor") {

                showPercentage <-
                  if (outcomeClassNumeric()) {
                    numericTypeProp()
                  } else {
                    TRUE
                  }

                tab <-
                  rccShinyIndTable(
                    group = dfuse$group,
                    group_hide_less_than = GLOBAL_hideLessThan,
                    group_factors = tab_order,
                    all_lab = GLOBAL_allLabel,
                    ind_numeric_percentiles = GLOBAL_prob,
                    lab_percentiles = GLOBAL_prob_labels,
                    ind = dfuse$outcome
                  )

                tab <- tab[match(tab_order, tab$group),]

                rcc2PlotMap(
                  value = if (showPercentage) {as.numeric(tab$Procent)} else {as.numeric(tab[[GLOBAL_prob_labels[2]]])},
                  valueLim = if (showPercentage) {c(0,100)} else {NULL},
                  legend = ifelse(
                    showPercentage,
                    rccShinyTXT(language = GLOBAL_language)$percent,
                    paste0(
                      GLOBAL_prob_labels[2],
                      " (", GLOBAL_propWithinUnit, ")")
                  ),
                  col = if (showPercentage){
                    if (ifelse(is.null(GLOBAL_sortDescending[whichOutcome()]), TRUE, GLOBAL_sortDescending[whichOutcome()])){
                      "#00b3f6"
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  },
                  nDec = ifelse(showPercentage, 0, 1),
                  outputHighchart = TRUE
                )

              }

            })

          } else {

            renderImage({

              x_width <- min(session$clientData$output_indMap_width, 700)
              yx_ratio <- 1.4

              tab_order <- rcc2PlotMap(valueOrderReturn = TRUE)

              tab_order[tab_order == "Halland"] <- hallandLabel()

              dfuse <- dfInput()

              if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
                if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
                  dfuse <- subset(dfuse, region %in% input[["param_region"]])
                }
              }

              dfuse$group <- dfuse[, rccShinyGroupVariable(label = "region")]

              dfuse <- subset(dfuse,group %in% tab_order)

              outfile <- tempfile(fileext = ".png")

              grDevices::png(filename = outfile, width = 9, height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

              if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] != "factor") {

                showPercentage <-
                  if (outcomeClassNumeric()) {
                    numericTypeProp()
                  } else {
                    TRUE
                  }

                tab <-
                  rccShinyIndTable(
                    group = dfuse$group,
                    group_hide_less_than = GLOBAL_hideLessThan,
                    group_factors = tab_order,
                    all_lab = GLOBAL_allLabel,
                    ind_numeric_percentiles = GLOBAL_prob,
                    lab_percentiles = GLOBAL_prob_labels,
                    ind = dfuse$outcome
                  )

                tab <- tab[match(tab_order, tab$group),]

                rcc2PlotMap(
                  value = if (showPercentage) {as.numeric(tab$Procent)} else {as.numeric(tab[[GLOBAL_prob_labels[2]]])},
                  valueLim = if (showPercentage) {c(0,100)} else {NULL},
                  legend = ifelse(
                    showPercentage,
                    rccShinyTXT(language = GLOBAL_language)$percent,
                    paste0(
                      GLOBAL_prob_labels[2],
                      " (", GLOBAL_propWithinUnit, ")")
                  ),
                  col = if (showPercentage){
                    if (ifelse(is.null(GLOBAL_sortDescending[whichOutcome()]), TRUE, GLOBAL_sortDescending[whichOutcome()])){
                      "#00b3f6"
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  },
                  nDec = ifelse(showPercentage, 0, 1)
                )

              } else {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)
                text(1, 1, rccShinyNoObservationsText(language = GLOBAL_language))
              }

              grDevices::dev.off()

              list(src = outfile,
                   contentType = "image/png",
                   width = x_width,
                   height = x_width * yx_ratio)

            }, deleteFile = TRUE)

          }

        output$indList <-
          DT::renderDataTable({

            dfuse <- dfInput()

            if (!GLOBAL_geoUnitsHospitalInclude) {
              dfuse$group <- dfuse$groupCode
            }

            listIncludeVariables <- vector()
            if (GLOBAL_idInclude) {
              listIncludeVariables <- c(listIncludeVariables, GLOBAL_id)
              dfuse[, GLOBAL_id] <- as.character(dfuse[, GLOBAL_id])
              dfuse[!(dfuse[, GLOBAL_idAuthorisedToView] %in% 1), GLOBAL_id] <- ""
            }
            if (GLOBAL_idOverviewLinkInclude) {
              listIncludeVariables <- c(listIncludeVariables, GLOBAL_idOverviewLink)
              dfuse[, GLOBAL_idOverviewLink] <- as.character(dfuse[, GLOBAL_idOverviewLink])
              dfuse[!(dfuse[, GLOBAL_idAuthorisedToView] %in% 1), GLOBAL_idOverviewLink] <- ""
            }
            listIncludeVariables <- c(
              listIncludeVariables,
              "group",
              if (GLOBAL_periodInclude) {"period"},
              "outcome"
            )
            listIncludeVariablesTxt <- c(
              if (GLOBAL_idInclude) {"ID"},
              if (GLOBAL_idOverviewLinkInclude) {rccShinyTXT(language = GLOBAL_language)$idOverviewLink},
              rccShinyLevelNames("hospital", language = GLOBAL_language),
              if (GLOBAL_periodInclude) {GLOBAL_periodLabel},
              rccShinyTXT(language = GLOBAL_language)$outcome
            )

            tab <-
              subset(
                dfuse,
                !is.na(groupCode) & groupCode %in% GLOBAL_incaUserHospital,
                select = listIncludeVariables
              )

            if (class(tab$outcome) %in% "logical") {
              tab$outcome <-
                factor(
                  tab$outcome,
                  levels = c(TRUE, FALSE),
                  labels = c(
                    rccShinyTXT(language = GLOBAL_language)$yes,
                    rccShinyTXT(language = GLOBAL_language)$no
                  )
                )
            }

            colnames(tab) <- listIncludeVariablesTxt

            if (nrow(tab) == 0) {
              tab <-
                subset(
                  data.frame(
                    rccShinyNoObservationsText(language = GLOBAL_language)
                  ),
                  FALSE
                )
              colnames(tab) <- rccShinyTXT(language = GLOBAL_language)$message
            }

            tempColumnDefs <-
              list(
                list(
                  className = 'dt-left',
                  targets = 0
                )
              )
            if (ncol(tab) > 1) {
              tempColumnDefs[[2]] <-
                list(
                  className = 'dt-right',
                  targets = 1:(ncol(tab)-1)
                )
            }

            tab <-
              DT::datatable(
                tab,
                escape = FALSE,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  columnDefs = tempColumnDefs,
                  language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
                  searching = TRUE,
                  paging = FALSE,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = list(
                    list(extend = 'excel', filename = indTitle(), title = indTitle()),
                    list(extend = 'pdf', filename = indTitle(), title = indTitle()),
                    list(extend = 'print', title = indTitle())
                  )
                )
              )

            tab
          })

        output$description <-
          renderUI({
            HTML(
              paste0(
                "<p></p>",
                if (!is.na(GLOBAL_description[1])){
                  paste0(
                    "<p><b>", rccShinyTXT(language = GLOBAL_language)$descriptionAbout, "</b></p>",
                    "<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>",
                    GLOBAL_description[1],
                    "</div>"
                  )
                },
                if (!is.na(GLOBAL_description[2]) | GLOBAL_hideLessThan > 1 | GLOBAL_hideLessThanCell > 1){
                  paste0(
                    "<p><b>", rccShinyTXT(language = GLOBAL_language)$descriptionInterpretation, "</b></p>",
                    "<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>",
                    if (!is.na(GLOBAL_description[2])){
                      paste0(
                        GLOBAL_description[2],
                        "<p></p>"
                      )
                    },
                    if (GLOBAL_hideLessThan > 1) {
                      paste0(
                        rccShinyTXT(language = GLOBAL_language)$fewcases1,
                        " ",
                        GLOBAL_hideLessThan,
                        " ",
                        rccShinyTXT(language = GLOBAL_language)$fewcases2,
                        ". "
                      )
                    },
                    if (GLOBAL_hideLessThanCell > 1) {
                      paste0(
                        rccShinyTXT(language = GLOBAL_language)$fewcases1cell,
                        " ",
                        GLOBAL_hideLessThanCell,
                        " ",
                        rccShinyTXT(language = GLOBAL_language)$fewcases2cell,
                        ". "
                      )
                    },
                    "</div>"
                  )
                },
                if (!is.na(GLOBAL_description[3])){
                  paste0(
                    "<p><b>", rccShinyTXT(language = GLOBAL_language)$descriptionTechnical, "</b></p>",
                    "<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>",
                    GLOBAL_description[3],
                    "</div>"
                  )
                }
              )
            )
          })
      }
    )

  }
#' Checks input to rccShiny
#' @description internal function.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyCheckData <-
  function(
    optionsList = NULL
  ) {

    optionsList$error <- ""

    # id
    optionsList$idInclude <- TRUE
    if (is.null(optionsList$id)) {
      optionsList$idInclude <- FALSE
    } else if (!(optionsList$id %in% colnames(optionsList$data))) {
      optionsList$idInclude <- FALSE
    }

    # idOverviewLink
    optionsList$idOverviewLinkInclude <- TRUE
    if (is.null(optionsList$idOverviewLink)) {
      optionsList$idOverviewLinkInclude <- FALSE
    } else if (!(optionsList$idOverviewLink %in% colnames(optionsList$data))) {
      optionsList$idOverviewLinkInclude <- FALSE
    }

    # idAuthorisedToView
    optionsList$idAuthorisedToViewInclude <- TRUE
    if (is.null(optionsList$idAuthorisedToView)) {
      optionsList$idAuthorisedToViewInclude <- FALSE
      optionsList$idInclude <- FALSE
      optionsList$idOverviewLinkInclude <- FALSE
    } else if (!(optionsList$idAuthorisedToView %in% colnames(optionsList$data))) {
      optionsList$idAuthorisedToViewInclude <- FALSE
      optionsList$idInclude <- FALSE
      optionsList$idOverviewLinkInclude <- FALSE
    } else if (!all(optionsList$data[, optionsList$idAuthorisedToView] %in% 0:1)) {
      optionsList$error <- paste0("Column '", optionsList$idAuthorisedToView, "' must contain only the values 0 and 1.")
      return(optionsList)
    }

    # outcome
    for (i in 1:length(optionsList$outcome)) {
      if (paste0(optionsList$outcome[i], "_", optionsList$language) %in% colnames(optionsList$data)) {
        optionsList$data[, optionsList$outcome[i]] <- optionsList$data[, paste0(optionsList$outcome[i], "_", optionsList$language)]
      } else if (!(optionsList$outcome[i] %in% colnames(optionsList$data))) {
        optionsList$error <- paste0("Column '", optionsList$outcome[i], "' not found in 'data'")
        return(optionsList)
      }
    }

    # outcomeClass
    optionsList$outcomeClass <- vector()
    for (i in 1:length(optionsList$outcome)) {
      optionsList$outcomeClass[i] <- class(optionsList$data[, optionsList$outcome[i]])
      if (!(optionsList$outcomeClass[i]) %in% c("logical", "factor", "numeric")) {
        optionsList$error <- paste0("Column '", optionsList$outcome[i], "' in 'data' is not of type logical, factor or numeric")
        return(optionsList)
      }
    }

    # outcomeTitle
    optionsList$outcomeTitle <-
      if (length(optionsList$outcomeTitle) >= optionsList$whichLanguage) {
        optionsList$outcomeTitle[[optionsList$whichLanguage]]
      } else {
        optionsList$outcomeTitle[[1]]
      }

    # textBeforeSubtitle
    optionsList$textBeforeSubtitle <-
      if (length(optionsList$textBeforeSubtitle) >= optionsList$whichLanguage) {
        optionsList$textBeforeSubtitle[optionsList$whichLanguage]
      } else {
        optionsList$textBeforeSubtitle[1]
      }

    # textAfterSubtitle
    optionsList$textAfterSubtitle <-
      if (length(optionsList$textAfterSubtitle) >= optionsList$whichLanguage) {
        optionsList$textAfterSubtitle[optionsList$whichLanguage]
      } else {
        optionsList$textAfterSubtitle[1]
      }

    # comment
    optionsList$comment <-
      enc2utf8(
        ifelse(
          length(optionsList$comment) >= optionsList$whichLanguage,
          optionsList$comment[optionsList$whichLanguage],
          optionsList$comment[1]
        )
      )

    # description
    optionsList$description <-
      if (length(optionsList$description) >= optionsList$whichLanguage) {
        optionsList$description[[optionsList$whichLanguage]]
      } else {
        optionsList$description[[1]]
      }
    if (length(optionsList$description) < 3)
      optionsList$description <- c(
        optionsList$description,
        rep(NA, 3 - length(optionsList$description))
      )

    # geoUnitsHospital
    optionsList$geoUnitsHospitalInclude <- TRUE
    if (is.null(optionsList$geoUnitsHospital)) {
      optionsList$geoUnitsHospitalInclude <- FALSE
      optionsList$geoUnitsHospital <- "sjukhus"
    } else if (!(optionsList$geoUnitsHospital %in% colnames(optionsList$data))) {
      optionsList$geoUnitsHospitalInclude <- FALSE
    } else if (!class(optionsList$data[, optionsList$geoUnitsHospital]) %in% c("character", "numeric", "integer")) {
      optionsList$error <- paste0("The data in the variable '", optionsList$geoUnitsHospital, "' should be one of the following classes: 'character', 'numeric' or 'integer'")
      return(optionsList)
    }

    # sjukhus
    if (optionsList$geoUnitsHospitalInclude) {
      optionsList$data$sjukhus <-
        if (paste0(optionsList$geoUnitsHospital, "_", optionsList$language) %in% colnames(optionsList$data)) {
          optionsList$data[, paste0(optionsList$geoUnitsHospital, "_", optionsList$language)]
        } else {
          optionsList$data[, optionsList$geoUnitsHospital]
        }
      # Fix missing in hospital variable
      optionsList$data$sjukhus[is.na(optionsList$data$sjukhus) | optionsList$data$sjukhus %in% ""] <- rccShinyTXT(language = optionsList$language)$missing
    } else {
      optionsList$data$sjukhus <- rep("(not displayed)", nrow(optionsList$data))
    }

    # geoUnitsHospitalAlt
    optionsList$geoUnitsHospitalAltInclude <- TRUE
    if (is.null(optionsList$geoUnitsHospitalAlt)) {
      optionsList$geoUnitsHospitalAltInclude <- FALSE
      optionsList$geoUnitsHospitalAlt <- "sjukhus_alt"
    } else if (!(optionsList$geoUnitsHospitalAlt %in% colnames(optionsList$data))) {
      optionsList$geoUnitsHospitalAltInclude <- FALSE
    } else if (!class(optionsList$data[, optionsList$geoUnitsHospitalAlt]) %in% c("character", "numeric", "integer")) {
      optionsList$error <- paste0("The data in the variable '", optionsList$geoUnitsHospitalAlt, "' should be one of the following classes: 'character', 'numeric' or 'integer'")
      return(optionsList)
    }

    # sjukhus_alt
    if (optionsList$geoUnitsHospitalAltInclude) {
      optionsList$data$sjukhus_alt <-
        if (paste0(optionsList$geoUnitsHospitalAlt, "_", optionsList$language) %in% colnames(optionsList$data)) {
          optionsList$data[, paste0(optionsList$geoUnitsHospitalAlt, "_", optionsList$language)]
        } else {
          optionsList$data[, optionsList$geoUnitsHospitalAlt]
        }
      # Fix missing in hospital variable
      optionsList$data$sjukhus_alt[is.na(optionsList$data$sjukhus_alt) | optionsList$data$sjukhus_alt %in% ""] <- rccShinyTXT(language = optionsList$language)$missing
    } else {
      optionsList$data$sjukhus_alt <- optionsList$data$sjukhus
    }

    # geoUnitsHospitalCode
    if (is.null(optionsList$geoUnitsHospitalCode)) {
      optionsList$incaIncludeList <- FALSE
      optionsList$geoUnitsHospitalCode <- "sjukhuskod"
    } else if (!(optionsList$geoUnitsHospitalCode %in% colnames(optionsList$data))) {
      optionsList$incaIncludeList <- FALSE
    } else if (!class(optionsList$data[, optionsList$geoUnitsHospitalCode]) %in% c("numeric","integer")) {
      optionsList$error <- paste0("The data in the variable '", optionsList$geoUnitsHospitalCode, "' should be one of the following classes: 'numeric' or 'integer'")
      return(optionsList)
    }

    # sjukhuskod
    if (optionsList$incaIncludeList) {
      optionsList$data$sjukhuskod <- optionsList$data[, optionsList$geoUnitsHospitalCode]
    } else {
      optionsList$data$sjukhuskod <- rep(NA, nrow(optionsList$data))
    }

    # geoUnitsCounty
    optionsList$geoUnitsCountyInclude <- TRUE
    if (is.null(optionsList$geoUnitsCounty)) {
      optionsList$geoUnitsCountyInclude <- FALSE
      optionsList$geoUnitsCounty <- "landsting"
    } else if (!(optionsList$geoUnitsCounty %in% colnames(optionsList$data))) {
      optionsList$geoUnitsCountyInclude <- FALSE
    } else if (!class(optionsList$data[, optionsList$geoUnitsCounty]) %in% c("numeric","integer")) {
      optionsList$error <- paste0("The data in the variable '", optionsList$geoUnitsCounty,"' should be one of the following classes: 'numeric' or 'integer'")
      return(optionsList)
    } else {
      optionsList$data$landstingCode <- suppressWarnings(as.numeric(as.character(optionsList$data[, optionsList$geoUnitsCounty])))
      if (!(all(optionsList$data$landstingCode %in% rccShinyCounties(lkf = optionsList$geoUnitsPatient)$landstingCode))) {
        optionsList$error <-
          paste0(
            "'", optionsList$geoUnitsCounty, "' contains invalid values. When 'geoUnitsPatient' = ", optionsList$geoUnitsPatient, ", '", optionsList$geoUnitsCounty, "' should only contain the values (",
            paste(rccShinyCounties(lkf = optionsList$geoUnitsPatient)$landstingCode, collapse = ", "),
            ")"
          )
        return(optionsList)
      }
    }

    # landsting
    if (optionsList$geoUnitsCountyInclude) {
      optionsList$data <- optionsList$data[, colnames(optionsList$data) != "landsting"]
      optionsList$data <-
        merge(
          optionsList$data,
          rccShinyCounties(language = optionsList$language, lkf = optionsList$geoUnitsPatient),
          by = "landstingCode",
          all.x = TRUE
        )
    } else {
      optionsList$data$landsting <- rep("(not displayed)", nrow(optionsList$data))
    }

    # geoUnitsRegion
    optionsList$geoUnitsRegionInclude <- TRUE
    if (is.null(optionsList$geoUnitsRegion)) {
      optionsList$geoUnitsRegionInclude <- FALSE
      optionsList$geoUnitsRegion <- "region"
    } else if (!(optionsList$geoUnitsRegion %in% colnames(optionsList$data))) {
      optionsList$geoUnitsRegionInclude <- FALSE
    } else if (!class(optionsList$data[, optionsList$geoUnitsRegion]) %in% c("numeric","integer")) {
      optionsList$error <- paste0("The data in the variable '", optionsList$geoUnitsRegion, "' should be one of the following classes: 'numeric' or 'integer'")
      return(optionsList)
    } else {
      optionsList$data$regionCode <- suppressWarnings(as.numeric(as.character(optionsList$data[, optionsList$geoUnitsRegion])))
      if (!(all(optionsList$data$regionCode %in% c(1:6, NA)))) {
        optionsList$error <- paste0("'", geoUnitsRegion, "' contains invalid values. '", geoUnitsRegion, "' should only contain the values (", paste(c(1:6, NA), collapse = ", "), ").")
        return(optionsList)
      }
    }

    # region
    if (optionsList$geoUnitsRegionInclude) {
      optionsList$data$region <-
        factor(
          optionsList$data$regionCode,
          levels = c(1:6, NA),
          labels = rccShinyRegionNames(language = optionsList$language),
          exclude = NULL
        )
    } else {
      optionsList$data$region <- rep("(not displayed)", nrow(optionsList$data))
    }

    # regionLabel
    optionsList$regionLabel <-
      ifelse(
        length(optionsList$regionLabel) >= optionsList$whichLanguage,
        optionsList$regionLabel[optionsList$whichLanguage],
        optionsList$regionLabel[1]
      )

    # regionChoices
    optionsList$regionChoices <- levels(factor(optionsList$data$region))[!(levels(factor(optionsList$data$region)) %in% rccShinyTXT(language = optionsList$language)$missing)]

    # regionSelected
    if (optionsList$geoUnitsRegionInclude & !is.null(optionsList$regionSelectionDefault)) {
      optionsList$regionSelected <- levels(optionsList$data$region)[optionsList$regionSelectionDefault]
    } else {
      optionsList$regionSelected <- rccShinyTXT(language = optionsList$language)$all
    }

    # period
    optionsList$periodInclude <- TRUE
    if (is.null(optionsList$period)) {
      optionsList$periodInclude <- FALSE
      optionsList$period <- "period"
      optionsList$data$period <- rep(1, nrow(optionsList$data))
    } else if (optionsList$period %in% colnames(optionsList$data)) {
      optionsList$data$period <- optionsList$data[, optionsList$period]
      if (!class(optionsList$data[, optionsList$period]) %in% c("numeric", "integer", "Date")) {
        optionsList$error <- paste0("The data in the variable '", optionsList$period,"' should be one of the following classes: 'numeric', 'integer' or 'Date'")
        return(optionsList)
      }
    } else {
      optionsList$error <- paste0("Column '", optionsList$period, "' not found in 'data'")
      return(optionsList)
    }

    # periodDate
    if (class(optionsList$data$period) == "Date") {
      optionsList$periodDate <- TRUE

      tempNonEmpty <- !is.na(optionsList$data$period)
      tempYear <- as.numeric(format(optionsList$data$period, "%Y"))
      tempQuarter <- quarters(optionsList$data$period)
      tempQuarter[!tempNonEmpty] <- NA
      tempPeriod <- rep(NA, nrow(optionsList$data))
      tempPeriod[tempNonEmpty] <-
        paste0(
          tempYear[tempNonEmpty],
          tempQuarter[tempNonEmpty]
        )
      optionsList$data$period <- tempPeriod

      tempYearsUnique <- sort(unique(tempYear))
      optionsList$periodStart <- utils::head(sort(unique(tempPeriod)), 1)
      optionsList$periodEnd <- utils::tail(sort(unique(tempPeriod)), 1)
      optionsList$periodValues <-
        paste0(
          rep(min(tempYearsUnique):max(tempYearsUnique), each = 4),
          rep(paste0("Q", 1:4), rep = length(tempYearsUnique))
        )

      optionsList$periodValues_quarters <- optionsList$periodValues[which(optionsList$periodValues == optionsList$periodStart):which(optionsList$periodValues == optionsList$periodEnd)]

      optionsList$periodStart <- min(tempYear, na.rm = TRUE)
      optionsList$periodEnd <- max(tempYear, na.rm = TRUE)
      optionsList$periodValues <- optionsList$periodStart:optionsList$periodEnd
    } else {
      optionsList$periodDate <- FALSE
      optionsList$periodStart <- min(optionsList$data$period, na.rm = TRUE)
      optionsList$periodEnd <- max(optionsList$data$period, na.rm = TRUE)
      optionsList$periodValues <- optionsList$periodStart:optionsList$periodEnd
      optionsList$periodValues_quarters <- optionsList$periodValues
    }

    # periodDefaultStart, periodDefaultEnd
    if (is.null(optionsList$periodDefaultEnd)) {
      optionsList$periodDefaultEnd <- optionsList$periodEnd
    } else if (!(optionsList$periodDefaultEnd %in% optionsList$periodValues)) {
      optionsList$periodDefaultEnd <- optionsList$periodEnd
    }
    if (is.null(optionsList$periodDefaultStart)) {
      optionsList$periodDefaultStart <- optionsList$periodDefaultEnd
    } else if (!(optionsList$periodDefaultStart %in% optionsList$periodValues)) {
      optionsList$periodDefaultStart <- optionsList$periodDefaultEnd
    } else if (which(optionsList$periodValues == optionsList$periodDefaultStart) > which(optionsList$periodValues == optionsList$periodDefaultEnd)) {
      optionsList$periodDefaultStart <- optionsList$periodDefaultEnd
    }
    optionsList$periodDefaultStart_quarters <- paste0(optionsList$periodDefaultStart, "Q1")
    if (!(optionsList$periodDefaultStart_quarters %in% optionsList$periodValues_quarters)) {
      optionsList$periodDefaultStart_quarters <- utils::head(optionsList$periodValues_quarters, 1)
    }
    optionsList$periodDefaultEnd_quarters <- paste0(optionsList$periodDefaultEnd, "Q4")
    if (!(optionsList$periodDefaultEnd_quarters %in% optionsList$periodValues_quarters)) {
      optionsList$periodDefaultEnd_quarters <- utils::tail(optionsList$periodValues_quarters, 1)
    }

    # periodLabel
    optionsList$periodLabel <-
      ifelse(
        length(optionsList$periodLabel) >= optionsList$whichLanguage,
        optionsList$periodLabel[optionsList$whichLanguage],
        optionsList$periodLabel[1]
      )

    # includeVariables
    includeVariables <- c(optionsList$outcome, "region", "landsting", "sjukhus", "sjukhus_alt", "sjukhuskod", "period")

    if (optionsList$idInclude)
      includeVariables <- c(includeVariables, optionsList$id)

    if (optionsList$idOverviewLinkInclude)
      includeVariables <- c(includeVariables, optionsList$idOverviewLink)

    if (optionsList$idAuthorisedToViewInclude)
      includeVariables <- c(includeVariables, optionsList$idAuthorisedToView)

    # varOtherComparison
    varOtherComparisonVariables <- vector()
    varOtherComparisonLabels <- vector()
    if (!is.null(optionsList$varOtherComparison)) {
      for (i in 1:length(optionsList$varOtherComparison)) {
        # varOtherComparison[[i]]$var
        tempVar <- optionsList$varOtherComparison[[i]]$var
        if (!(tempVar %in% colnames(optionsList$data))) {
          optionsList$error <- paste0("The variable '", tempVar, "' from varOtherComparison[[", i, "]] is missing in 'data'")
          return(optionsList)
        }
        if (paste0(tempVar, "_", optionsList$language) %in% colnames(optionsList$data))
          optionsList$data[, tempVar] <- optionsList$data[, paste0(tempVar, "_", optionsList$language)]
        varOtherComparisonVariables <- c(varOtherComparisonVariables, tempVar)
        # varOtherComparison[[i]]$label
        if (!("label" %in% names(optionsList$varOtherComparison[[i]])) | is.null(optionsList$varOtherComparison[[i]]$label))
          optionsList$varOtherComparison[[i]]$label <- tempVar
        optionsList$varOtherComparison[[i]]$label <-
          ifelse(
            length(optionsList$varOtherComparison[[i]]$label) >= optionsList$whichLanguage,
            optionsList$varOtherComparison[[i]]$label[optionsList$whichLanguage],
            optionsList$varOtherComparison[[i]]$label[1]
          )
        varOtherComparisonLabels <- c(varOtherComparisonLabels, optionsList$varOtherComparison[[i]]$label)
      }
      if (anyDuplicated(varOtherComparisonVariables) > 0) {
        optionsList$error <- paste0("'varOtherComparison' contains duplicate variable names")
        return(optionsList)
      }
      if (anyDuplicated(varOtherComparisonLabels) > 0) {
        optionsList$error <- paste0("'varOtherComparison' contains duplicate variable labels")
        return(optionsList)
      }
      includeVariables <- c(includeVariables, varOtherComparisonVariables)
    }
    optionsList$varOtherComparisonVariables <- varOtherComparisonVariables
    optionsList$varOtherComparisonLabels <- varOtherComparisonLabels

    # Check for conflicts in labels (to be fixed in future version)
    tempReservedLabels <- c(
      rccShinyLevelNames("region", language = optionsList$language),
      rccShinyLevelNames("county_lkf", language = optionsList$language),
      rccShinyLevelNames("county", language = optionsList$language),
      rccShinyLevelNames("hospital", language = optionsList$language)
    )
    if (any(tolower(optionsList$varOtherComparisonLabels) %in% tolower(tempReservedLabels))) {
      optionsList$error <- paste0(paste(paste0("'", tempReservedLabels, "'"), collapse = "/"), " are currently not allowed as labels in varOtherComparison when language = '", optionsList$language, "'. This will be fixed in a future version. For now, try adding a whitespace after the label in order for it to be distinct.")
      return(optionsList)
    }

    # geoUnitsHospitalInclude, geoUnitsCountyInclude, geoUnitsRegionInclude, varOtherComparison
    if (sum(optionsList$geoUnitsHospitalInclude, optionsList$geoUnitsCountyInclude, optionsList$geoUnitsRegionInclude, length(optionsList$varOtherComparisonVariables)) < 1) {
      optionsList$error <- paste0("At least one level of comparison (hospital/county/region/varOtherComparison) must be available")
      return(optionsList)
    }

    # geoUnitsDefault
    if (!(optionsList$geoUnitsDefault %in% c("region", "county", "hospital", optionsList$varOtherComparisonVariables)))
      stop("Valid values for 'geoUnitsDefault' are 'region', 'county', 'hospital' or any of the variable names ('var') in 'varOtherComparison'", call. = FALSE)

    # varOther
    if (!is.null(optionsList$varOther)) {
      varOtherVariables <- vector()
      for (i in 1:length(optionsList$varOther)) {
        # varOther[[i]]$var
        tempVar <- optionsList$varOther[[i]]$var
        if (!(tempVar %in% colnames(optionsList$data))) {
          optionsList$error <- paste0("The variable '", tempVar, "' from varOther[[", i, "]] is missing in 'data'")
          return(optionsList)
        }
        if (paste0(tempVar, "_", optionsList$language) %in% colnames(optionsList$data))
          optionsList$data[, tempVar] <- optionsList$data[, paste0(tempVar, "_", optionsList$language)]
        varOtherVariables <- c(varOtherVariables, tempVar)
        # varOther[[i]]$label
        if (!("label" %in% names(optionsList$varOther[[i]])) | is.null(optionsList$varOther[[i]]$label))
          optionsList$varOther[[i]]$label <- tempVar
        optionsList$varOther[[i]]$label <-
          ifelse(
            length(optionsList$varOther[[i]]$label) >= optionsList$whichLanguage,
            optionsList$varOther[[i]]$label[optionsList$whichLanguage],
            optionsList$varOther[[i]]$label[1]
          )
        # varOther[[i]]$classNumeric
        optionsList$varOther[[i]]$classNumeric <- class(optionsList$data[, tempVar]) %in% c("difftime", "numeric", "integer")
        # varOther[[i]]$choices
        if (!("choices" %in% names(optionsList$varOther[[i]])) | is.null(optionsList$varOther[[i]]$choices)) {
          if (optionsList$varOther[[i]]$classNumeric) {
            optionsList$varOther[[i]]$choices <- range(optionsList$data[, tempVar], na.rm = TRUE)
          } else {
            optionsList$varOther[[i]]$choices <- levels(factor(optionsList$data[, tempVar]))
          }
        }
        if (is.list(optionsList$varOther[[i]]$choices)) {
          optionsList$varOther[[i]]$choices <- optionsList$varOther[[i]]$choices[[ifelse(length(optionsList$varOther[[i]]$choices) >= optionsList$whichLanguage, optionsList$whichLanguage, 1)]]
        }
        # varOther[[i]]$selected
        if (!("selected" %in% names(optionsList$varOther[[i]])) | is.null(optionsList$varOther[[i]]$selected))
          optionsList$varOther[[i]]$selected <- optionsList$varOther[[i]]$choices
        if (is.list(optionsList$varOther[[i]]$selected)) {
          optionsList$varOther[[i]]$selected <- optionsList$varOther[[i]]$selected[[ifelse(length(optionsList$varOther[[i]]$selected) >= optionsList$whichLanguage, optionsList$whichLanguage, 1)]]
        }
        # varOther[[i]]$multiple
        if (!("multiple" %in% names(optionsList$varOther[[i]])) | is.null(optionsList$varOther[[i]]$multiple))
          optionsList$varOther[[i]]$multiple <- TRUE
        # varOther[[i]]$showInTitle
        if (!("showInTitle" %in% names(optionsList$varOther[[i]])) | is.null(optionsList$varOther[[i]]$showInTitle))
          optionsList$varOther[[i]]$showInTitle <- TRUE
      }
      includeVariables <- c(includeVariables, varOtherVariables)
    }

    # allLabel
    optionsList$allLabel <-
      ifelse(
        length(optionsList$allLabel) >= optionsList$whichLanguage,
        optionsList$allLabel[optionsList$whichLanguage],
        optionsList$allLabel[1]
      )

    # propWithinUnit
    optionsList$propWithinUnit <-
      ifelse(
        length(optionsList$propWithinUnit) >= optionsList$whichLanguage,
        optionsList$propWithinUnit[optionsList$whichLanguage],
        optionsList$propWithinUnit[1]
      )

    # propWithinValue
    if (length(optionsList$propWithinValue) == 1) {
      optionsList$propWithinValue <- rep(optionsList$propWithinValue, length(optionsList$outcome))
    }

    # prop
    if (is.null(optionsList$prob)) {
      optionsList$prob <- c(0.25, 0.50, 0.75)
    }

    # prob_labels
    optionsList$prob_labels <- c(
      rccShinyTXT(language = optionsList$language)$q1,
      rccShinyTXT(language = optionsList$language)$median,
      rccShinyTXT(language = optionsList$language)$q3
    )
    optionsList$iqrlab <- rccShinyTXT(language = optionsList$language)$iqr

    if (optionsList$prob[1] != 0.25){
      optionsList$prob_labels[1] <- paste0(optionsList$prob[1] * 100, "-", rccShinyTXT(language = optionsList$language)$percentile)
      optionsList$iqrlab <- rccShinyTXT(language = optionsList$language)$interquantilerange
    }
    if (optionsList$prob[2] != 0.5){
      optionsList$prob_labels[2] <- paste0(optionsList$prob[2] * 100, "-", rccShinyTXT(language = optionsList$language)$percentile)
    }
    if (optionsList$prob[3] != 0.75){
      optionsList$prob_labels[3] <- paste0(optionsList$prob[3] * 100, "-", rccShinyTXT(language = optionsList$language)$percentile)
      optionsList$iqrlab <- rccShinyTXT(language = optionsList$language)$interquantilerange
    }
    optionsList$medianiqrlab <- paste(
      optionsList$prob_labels[2],
      rccShinyTXT(language = optionsList$language)$iqr_and,
      optionsList$iqrlab
    )

    # hideLessThan
    optionsList$hideLessThan <-
      ifelse(
        !optionsList$inca & optionsList$hideLessThan < 5,
        5,
        optionsList$hideLessThan
      )
    optionsList$hideLessThan <-
      ifelse(
        optionsList$hideLessThan == 0,
        1,
        optionsList$hideLessThan
      )

    # data
    optionsList$data <-
      fixEncoding(
        subset(
          optionsList$data,
          select = unique(includeVariables)
        )
      )

    return(optionsList)
  }
