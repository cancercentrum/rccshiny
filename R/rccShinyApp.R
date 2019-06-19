#' Creates shiny app
#' @description internal function.
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#' @export
rccShinyApp <-
  function(
    optionsList = NULL
  ) {

    options(spinner.type = 3, spinner.color.background = "#ffffff")

    shinyApp(
      ui = dashboardPage(
        skin = "black",
        dashboardHeader(disable = TRUE),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
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
              box(
                title = NULL,
                status = "primary",
                width = NULL,
                uiOutput("outcomeInput"),
                uiOutput("regionInput"),
                uiOutput("levelpresentInput"),
                uiOutput("ownhospitalInput"),
                uiOutput("numericTypeInput"),
                uiOutput("numericTypePropInput"),
                uiOutput("periodInput"),
                uiOutput("periodSplitInput"),
                uiOutput("userInput"),
                uiOutput("funnelPlotInput")
              ),
              uiOutput("comment")
            ),
            shinycssloaders::withSpinner(uiOutput("theTabs"))
          )
        )
      ),
      server = function(input, output, session) {

        if (optionsList$inca) {
          withProgress(
            message = "Laddar data och genererar rapport...",
            value = 0,
            {
              INCA::loadDataFrames(parseQueryString(isolate(session$clientData$url_search))[['token']])
              if (exists("environmentVariables")) {
                if (!is.null(environmentVariables$UserParentUnitCode))
                  optionsList$incaUserHospital <- environmentVariables$UserParentUnitCode
              }
              incProgress(0.50)
              source(optionsList$incaScript, encoding = "UTF-8")
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
        for (i in 1:length(optionsList)) {
          assign(x = paste0("GLOBAL_", names(optionsList)[i]), value = optionsList[[i]], envir = .GlobalEnv)
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
                      rccShinyTXT(language = GLOBAL_language)$median,
                      " (", GLOBAL_propWithinUnit, ")"
                    ),
                    paste(
                      rccShinyTXT(language = GLOBAL_language)$numericchoices_prop,
                      GLOBAL_propWithinUnit
                    )
                  ),
                  selected = paste0(
                    rccShinyTXT(language = GLOBAL_language)$median,
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
                  paste0("input.param_numerictype == '", paste(rccShinyTXT(language = GLOBAL_language)$numericchoices_prop, GLOBAL_propWithinUnit ), "'"),
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
            tagList(
              conditionalPanel(
                condition =
                  paste0(
                    "input.tab!='fig_trend' & input.tab!='fig_map' & input.tab!='list' & ",
                    ifelse(GLOBAL_outcomeClass[whichOutcome()] == "factor", "true", "input.tab!='fig_trend'"),
                    " & ",
                    ifelse(sum(GLOBAL_geoUnitsHospitalInclude, GLOBAL_geoUnitsCountyInclude, GLOBAL_geoUnitsRegionInclude) > 1, "true", "false")
                  ),
                selectInput(
                  inputId = "param_levelpresent",
                  label = rccShinyTXT(language = GLOBAL_language)$levelofcomparison,
                  choices = c(
                    if (GLOBAL_geoUnitsRegionInclude) { rccShinyLevelNames("region", language = GLOBAL_language) },
                    if (GLOBAL_geoUnitsCountyInclude) {
                      rccShinyLevelNames(
                        ifelse(
                          GLOBAL_geoUnitsPatient,
                          "county_lkf",
                          "county"
                        ),
                        language = GLOBAL_language
                      )
                    },
                    if (GLOBAL_geoUnitsHospitalInclude) { rccShinyLevelNames("hospital", language = GLOBAL_language) }
                  ),
                  selected =
                    if (GLOBAL_geoUnitsRegionInclude & GLOBAL_geoUnitsDefault %in% "region") {
                      rccShinyLevelNames("region", language = GLOBAL_language)
                    } else if (GLOBAL_geoUnitsHospitalInclude & GLOBAL_geoUnitsDefault %in% "hospital") {
                      rccShinyLevelNames("hospital", language = GLOBAL_language)
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

        output$ownhospitalInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "input.tab!='fig_map' & input.tab!='table_num' & input.tab!='table_pct' & input.tab!='table' & input.tab!='list' & ",
                  ifelse(GLOBAL_geoUnitsHospitalInclude, "true", "false"),
                  " & !(",
                  ifelse(GLOBAL_geoUnitsPatient, "true", "false"),
                  " & input.param_levelpresent != '", rccShinyLevelNames("hospital", language = GLOBAL_language), "' & input.tab == 'fig_compare')"
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

        output$periodInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition =
                  paste0(
                    "input.tab!='fig_trend' & ",
                    ifelse(GLOBAL_periodStart == GLOBAL_periodEnd, "false", "true")
                  ),
                if (!(GLOBAL_periodDate & GLOBAL_periodDateLevel != "year")) {
                  sliderInput(
                    inputId = "param_period",
                    label = GLOBAL_periodLabel,
                    min = GLOBAL_periodStart,
                    max = GLOBAL_periodEnd,
                    step = 1,
                    ticks = FALSE,
                    value = rep(GLOBAL_periodEnd, 2),
                    sep = "",
                    width = "100%"
                  )
                } else {
                  sliderTextInput(
                    inputId = "param_period",
                    label = GLOBAL_periodLabel,
                    choices = GLOBAL_periodValues,
                    selected = rep(GLOBAL_periodEnd, 2),
                    width = "100%"
                  )
                }
              )
            )
          })

        output$periodSplitInput <-
          renderUI({
            tagList(
              conditionalPanel(
                condition = paste0(
                  "!input.param_funnelplot & input.tab!='fig_trend' & input.tab!='fig_map' & input.tab!='list' & ",
                  ifelse(
                    !is.null(input[["param_period"]]),
                    "input.param_period[0]!=input.param_period[1]",
                    "false"
                  )
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
                      pickerInput(
                        inputId = paste0("userInputId", i),
                        label = tempList$label,
                        choices = tempList$choices,
                        selected = tempList$selected,
                        multiple = tempList$multiple,
                        options = list('none-selected-text' = ""),
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
          tempHospitals <- sort(unique(GLOBAL_data$sjukhus))
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
              tempHospitals <- tempHospitals[tempHospitals %in% GLOBAL_data$sjukhus[GLOBAL_data$region %in% input[["param_region"]]]]
            }
          }

          # Speciallösning för NPCR
          # -----------------------
          if (GLOBAL_npcrGroupPrivateOthers) {
            showPrivateHospitals <- TRUE
            if (!GLOBAL_regionSelection | is.null(input[["param_region"]])) {
              showPrivateHospitals <- FALSE
            } else {
              if (rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]]) {
                showPrivateHospitals <- FALSE
              }
            }

            if (!showPrivateHospitals) {
              npcrListPrivateAlwaysShow <- c(
                "Capio Lundby Närsjukhus",
                "Carlanderska sjukhuset",
                "Sophiahemmet",
                "Capio S:t Görans sjukhus",
                "Capio S:t Görans sjukhus - UroClinic"
              )
              privateOthersName <- npcrPreparePeriodRegionCountyHospitalVariables(language = GLOBAL_language,returnPrivateOthersNames = TRUE)
              landstingName <- privateOthersName[[GLOBAL_language]]$landsting
              sjukhusName <- privateOthersName[[GLOBAL_language]]$sjukhus_privatovriga
              tempHospitals <- tempHospitals[!(tempHospitals %in% GLOBAL_data$sjukhus[substr(GLOBAL_data$landsting,1,nchar(landstingName)) == landstingName]) | tempHospitals %in% npcrListPrivateAlwaysShow]
              tempHospitals <- c(
                tempHospitals,
                paste0(sjukhusName," - ",rccShinyRegionNames(language = GLOBAL_language))
              )
              tempHospitals <- sort(tempHospitals)
            }
          }
          # -----------------------

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
            paste0(
              GLOBAL_periodLabel,
              ": ",
              ifelse(
                input[["param_period"]][1] == input[["param_period"]][2],
                as.character(strong(input[["param_period"]][1])),
                as.character(strong(
                  paste0(
                    input[["param_period"]][1],
                    "-",
                    input[["param_period"]][2]
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
          function(
            period=TRUE,
            hideLessThan=FALSE
          ) {
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
          indSubtitle(
            period = !(input$tab == "fig_trend"),
            hideLessThan = GLOBAL_hideLessThan
          )
        })

        output$text2 <- renderText({
          indSubtitleUserInput()
        })

        output$comment <- renderUI({
          if (GLOBAL_comment != "") {
            box(
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
            theTabs <-
              list(
                if (GLOBAL_outputHighcharts) {
                  tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_compare, value = "fig_compare", highcharter::highchartOutput("indPlot", height = "980px"), icon = icon("chart-bar"))
                } else {
                  tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_compare, value = "fig_compare", plotOutput("indPlot", height = "auto"), icon = icon("chart-bar"))
                }
              )
            if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_n, value = "table_num", DT::dataTableOutput("indTableNum"), icon = icon("table"))
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_p, value = "table_pct", DT::dataTableOutput("indTablePct"), icon = icon("table"))
            } else {
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab, value = "table", DT::dataTableOutput("indTable"), icon = icon("table"))
              if (GLOBAL_geoUnitsCountyInclude) {
                if (GLOBAL_outputHighcharts) {
                  theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$map, value = "fig_map", highcharter::highchartOutput("indMap", height = "980px"), icon = icon("map-marked-alt"))
                } else {
                  theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$map, value = "fig_map", plotOutput("indMap", height = "auto"), icon = icon("map-marked-alt"))
                }
              }
            }
            if (GLOBAL_periodInclude) {
              if (GLOBAL_outputHighcharts) {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_trend, value = "fig_trend", highcharter::highchartOutput("indPlotTrend", height = "630px"), icon = icon("chart-line"))
              } else {
                theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_trend, value = "fig_trend", plotOutput("indPlotTrend", height = "auto"), icon = icon("chart-line"))
              }
            }
            if (GLOBAL_inca & GLOBAL_incaIncludeList) {
              theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$list, value = "list", DT::dataTableOutput("indList"), icon = icon("list"))
            }
            theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$description, value = "description", htmlOutput("description"), icon = icon("info-circle"))
            do.call(tabBox, c(theTabs, id = "tab", width = 9))
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
          dftemp <- subset(dftemp, !is.na(outcome))

          if (input$tab != "fig_trend") {
            if (!(GLOBAL_periodDate & GLOBAL_periodDateLevel != "year")) {
              selectionPeriods <- input[["param_period"]][1]:input[["param_period"]][2]
            } else if (GLOBAL_periodDateLevel == "quarter") {
              selectionPeriods <- GLOBAL_periodValues
              selectionPeriods <- selectionPeriods[which(selectionPeriods == input[["param_period"]][1]):which(selectionPeriods == input[["param_period"]][2])]
            }

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
              dftemp$landsting[dftemp$landsting == "Halland" & dftemp$region == rccShinyRegionNames(language = GLOBAL_language)[4]] <- "Södra Halland"
              dftemp$landsting[dftemp$landsting == "Halland" & dftemp$region == rccShinyRegionNames(language = GLOBAL_language)[5]] <- "Norra Halland"
            }

            # Speciallösning för NPCR
            # -----------------------
            if (GLOBAL_npcrGroupPrivateOthers) {
              showPrivateHospitals <- TRUE
              if (!GLOBAL_regionSelection | is.null(input[["param_region"]])) {
                showPrivateHospitals <- FALSE
              } else {
                if (rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]]) {
                  showPrivateHospitals <- FALSE
                }
              }

              if (!showPrivateHospitals) {
                npcrListPrivateAlwaysShow <- c(
                  "Capio Lundby Närsjukhus",
                  "Carlanderska sjukhuset",
                  "Sophiahemmet",
                  "Capio S:t Görans sjukhus",
                  "Capio S:t Görans sjukhus - UroClinic"
                )
                privateOthersName <- npcrPreparePeriodRegionCountyHospitalVariables(language = GLOBAL_language,returnPrivateOthersNames = TRUE)
                landstingName <- privateOthersName[[GLOBAL_language]]$landsting
                sjukhusName <- privateOthersName[[GLOBAL_language]]$sjukhus_privatovriga
                changeName <- substr(dftemp$landsting, 1, nchar(landstingName)) == landstingName & !(dftemp$sjukhus %in% npcrListPrivateAlwaysShow)
                dftemp$sjukhus[changeName] <- paste0(sjukhusName," - ",dftemp$region[changeName])
              }
            }
            # -----------------------

            dftemp$group <- dftemp[,rccShinyGroupVariable(label = input$param_levelpresent)]
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
              hallandLabel <- "Södra Halland"
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
                  allLab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                  emphLab = emphLabel(dfuse),
                  ind = dfuse$outcome,
                  indNumericExcludeNeg = FALSE,
                  indTitle = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    rccShinyTXT(language = GLOBAL_language)$median,
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  indNCasesTxt = rccShinyTXT(language = GLOBAL_language)$noofcases,
                  indNCasesOfTxt = rccShinyTXT(language = GLOBAL_language)$noofcases_nOfN,
                  period = if (input$param_periodSplit) {dfuse$period} else {NULL},
                  xLab = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    paste0(
                      rccShinyTXT(language = GLOBAL_language)$medianiqr,
                      " (", GLOBAL_propWithinUnit, ")"),
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  legendFixedTextWidth = TRUE,
                  cexText = ifelse(
                    input$param_levelpresent == rccShinyLevelNames("hospital",language = GLOBAL_language),
                    0.8,
                    1
                  ),
                  cexPoint = ifelse(
                    input$param_levelpresent == rccShinyLevelNames("hospital", language = GLOBAL_language),
                    1.8,
                    3
                  ),
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

              png(filename = outfile, width = 9,height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

              if (nrow(dfuse) >= GLOBAL_hideLessThan) {
                rcc2PlotInd(
                  group = dfuse$group,
                  groupHideLessThan = GLOBAL_hideLessThan,
                  groupHideLessThanLabel = rccShinyTXT(language = GLOBAL_language)$grouphidelessthan,
                  allLab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                  emphLab = emphLabel(dfuse),
                  ind = dfuse$outcome,
                  indNumericExcludeNeg = FALSE,
                  indTitle = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    rccShinyTXT(language = GLOBAL_language)$median,
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  indNCasesTxt = rccShinyTXT(language = GLOBAL_language)$noofcases,
                  indNCasesOfTxt = rccShinyTXT(language = GLOBAL_language)$noofcases_nOfN,
                  period = if (input$param_periodSplit) {dfuse$period} else {NULL},
                  xLab = ifelse(
                    class(dfuse$outcome) %in% "numeric",
                    paste0(
                      rccShinyTXT(language = GLOBAL_language)$medianiqr,
                      " (", GLOBAL_propWithinUnit, ")"),
                    rccShinyTXT(language = GLOBAL_language)$percent
                  ),
                  legendFixedTextWidth = TRUE,
                  cexText = ifelse(
                    input$param_levelpresent == rccShinyLevelNames("hospital",language = GLOBAL_language),
                    0.8,
                    1
                  ),
                  cexPoint = ifelse(
                    input$param_levelpresent == rccShinyLevelNames("hospital", language = GLOBAL_language),
                    1.8,
                    3
                  ),
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

              dev.off()

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
                    all_lab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                    ind = dfuse$outcome,
                    ind_factor_pct = GLOBAL_outcomeClass[whichOutcome()] == "factor",
                    period = dfuse$period,
                    period_factors = GLOBAL_periodValues,
                    period_alwaysinclude = TRUE
                  )

                tab_group <- subset(tab,group == input$param_ownhospital)
                tab_total <- subset(tab,group == rccShinyTXT(language = GLOBAL_language)$RIKET)

                tab <- rbind(tab_total, tab_group)

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  if (nrow(tab_group) > 0) {
                    yx_ratio <- 1.8
                  }

                } else if (GLOBAL_geoUnitsRegionInclude) {

                  tab_region <-
                    rccShinyIndTable(
                      group = dfuse[,rccShinyGroupVariable("region")],
                      group_hide_less_than = GLOBAL_hideLessThan,
                      all_lab = NULL,
                      ind = dfuse$outcome,
                      period = dfuse$period,
                      period_factors = GLOBAL_periodValues,
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
                    title = rccShinyTXT(language = GLOBAL_language)$RIKET,
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
                    y_varinterest <- "Median"
                    y_varinterest_txt <- paste0(
                      rccShinyTXT(language = GLOBAL_language)$median,
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
                  col[legend == rccShinyTXT(language = GLOBAL_language)$RIKET] <- master_col[7]
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
                            y_varinterest == rccShinyTXT(language = GLOBAL_language)$median,
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
                    all_lab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                    ind = dfuse$outcome,
                    ind_factor_pct = GLOBAL_outcomeClass[whichOutcome()] == "factor",
                    period = dfuse$period,
                    period_factors = GLOBAL_periodValues,
                    period_alwaysinclude = TRUE
                  )

                tab_group <- subset(tab,group == input$param_ownhospital)
                tab_total <- subset(tab,group == rccShinyTXT(language = GLOBAL_language)$RIKET)

                tab <- rbind(tab_total, tab_group)

                if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {

                  if (nrow(tab_group) > 0) {
                    yx_ratio <- 1.8
                  }

                } else if (GLOBAL_geoUnitsRegionInclude) {

                  tab_region <-
                    rccShinyIndTable(
                      group = dfuse[,rccShinyGroupVariable("region")],
                      group_hide_less_than = GLOBAL_hideLessThan,
                      all_lab = NULL,
                      ind = dfuse$outcome,
                      period = dfuse$period,
                      period_factors = GLOBAL_periodValues,
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

              png(filename = outfile, width = 9, height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

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
                    title = rccShinyTXT(language = GLOBAL_language)$RIKET,
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
                    y_varinterest <- "Median"
                    y_varinterest_txt <- paste0(
                      rccShinyTXT(language = GLOBAL_language)$median,
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
                  col[legend == rccShinyTXT(language = GLOBAL_language)$RIKET] <- master_col[7]
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
                            y_varinterest == rccShinyTXT(language = GLOBAL_language)$median,
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

              dev.off()

              list(src = outfile,
                   contentType = "image/png",
                   width = x_width,
                   height = x_width * yx_ratio)

            }, deleteFile = TRUE)

          }

        output$indTableNum <- DT::renderDataTable({

          dfuse <- dfInput()

          tempSubset <- NULL
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
              tempSubset <- dfuse$region %in% input[["param_region"]]
            }
          }

          if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] == "factor") {
            if (!input$param_periodSplit & input[["param_period"]][1] != input[["param_period"]][2]) {
              dfuse$period <-
                paste0(
                  input[["param_period"]][1],
                  "-",
                  input[["param_period"]][2]
                )
            }

            tab <-
              rccShinyIndTable(
                language = GLOBAL_language,
                group = dfuse$group,
                group_hide_less_than = GLOBAL_hideLessThan,
                ind = dfuse$outcome,
                period = dfuse$period,
                period_alwaysinclude = GLOBAL_periodInclude,
                lab_period = GLOBAL_periodLabel,
                subset = tempSubset,
                subset_lab = paste(input[["param_region"]], collapse = "/")
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

          assign("nColTab", ncol(tab), envir = .GlobalEnv)

          tab

        },
        extensions = 'Buttons',
        options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = 1:(nColTab-1))),
          language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
          searching = TRUE,
          paging = FALSE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          buttons = list(
            list(extend = 'excel', filename = 'tableExport'),
            list(extend = 'pdf', filename = 'tableExport'),
            list(extend = 'print')
          )
        ),
        rownames = FALSE
        )

        output$indTablePct <- DT::renderDataTable({

          dfuse <- dfInput()

          tempSubset <- NULL
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
              tempSubset <- dfuse$region %in% input[["param_region"]]
            }
          }

          if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] == "factor") {
            if (!input$param_periodSplit & input[["param_period"]][1] != input[["param_period"]][2]) {
              dfuse$period <-
                paste0(
                  input[["param_period"]][1],
                  "-",
                  input[["param_period"]][2]
                )
            }

            tab <-
              rccShinyIndTable(
                language = GLOBAL_language,
                group = dfuse$group,
                group_hide_less_than = GLOBAL_hideLessThan,
                ind = dfuse$outcome,
                ind_factor_pct = TRUE,
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

          assign("nColTab", ncol(tab), envir = .GlobalEnv)

          tab

        },
        extensions = 'Buttons',
        options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = 1:(nColTab-1))),
          language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
          searching = TRUE,
          paging = FALSE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          buttons = list(
            list(extend = 'excel', filename = 'tableExport'),
            list(extend = 'pdf', filename = 'tableExport'),
            list(extend = 'print')
          )
        ),
        rownames = FALSE
        )

        output$indTable <- DT::renderDataTable({

          dfuse <- dfInput()

          tempSubset <- NULL
          if (GLOBAL_regionSelection & !is.null(input[["param_region"]])) {
            if (!(rccShinyTXT(language = GLOBAL_language)$all %in% input[["param_region"]])) {
              tempSubset <- dfuse$region %in% input[["param_region"]]
            }
          }

          if (nrow(dfuse) >= GLOBAL_hideLessThan & GLOBAL_outcomeClass[whichOutcome()] != "factor") {
            if (!input$param_periodSplit & input[["param_period"]][1] != input[["param_period"]][2]) {
              dfuse$period <-
                paste0(
                  input[["param_period"]][1],
                  "-",
                  input[["param_period"]][2]
                )
            }

            tab <-
              rccShinyIndTable(
                language = GLOBAL_language,
                group = dfuse$group,
                group_hide_less_than = GLOBAL_hideLessThan,
                ind = dfuse$outcome,
                period = dfuse$period,
                period_alwaysinclude = GLOBAL_periodInclude,
                lab_period = GLOBAL_periodLabel,
                subset = tempSubset,
                subset_lab = paste(input[["param_region"]], collapse = "/")
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

          assign("nColTab", ncol(tab), envir = .GlobalEnv)

          tab

        },
        extensions = 'Buttons',
        options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = 1:(nColTab-1))),
          language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
          searching = TRUE,
          paging = FALSE,
          dom = 'Bfrtip',
          scrollX = TRUE,
          buttons = list(
            list(extend = 'excel', filename = 'tableExport'),
            list(extend = 'pdf', filename = 'tableExport'),
            list(extend = 'print')
          )
        ),
        rownames = FALSE
        )

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

              dfuse$group <- dfuse[, rccShinyGroupVariable(label = "landsting")]

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
                    all_lab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                    ind = dfuse$outcome
                  )

                tab <- tab[match(tab_order, tab$group),]

                rcc2PlotMap(
                  value = if (showPercentage) {as.numeric(tab$Procent)} else {as.numeric(tab$Median)},
                  valueLim = if (showPercentage) {c(0,100)} else {NULL},
                  legend = ifelse(
                    showPercentage,
                    rccShinyTXT(language = GLOBAL_language)$percent,
                    paste0(
                      rccShinyTXT(language = GLOBAL_language)$median,
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

              dfuse$group <- dfuse[, rccShinyGroupVariable(label = "landsting")]

              dfuse <- subset(dfuse,group %in% tab_order)

              outfile <- tempfile(fileext = ".png")

              png(filename = outfile, width = 9, height = 9 * yx_ratio, units = "in", res = 2*x_width/9)

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
                    all_lab = rccShinyTXT(language = GLOBAL_language)$RIKET,
                    ind = dfuse$outcome
                  )

                tab <- tab[match(tab_order, tab$group),]

                rcc2PlotMap(
                  value = if (showPercentage) {as.numeric(tab$Procent)} else {as.numeric(tab$Median)},
                  valueLim = if (showPercentage) {c(0,100)} else {NULL},
                  legend = ifelse(
                    showPercentage,
                    rccShinyTXT(language = GLOBAL_language)$percent,
                    paste0(
                      rccShinyTXT(language = GLOBAL_language)$median,
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

              dev.off()

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
                    list(extend = 'excel', filename = 'tableExport'),
                    list(extend = 'pdf', filename = 'tableExport'),
                    list(extend = 'print')
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
                "<p><b>", rccShinyTXT(language = GLOBAL_language)$descriptionInterpretation, "</b></p>",
                "<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>",
                if (!is.na(GLOBAL_description[2])){
                  paste0(
                    GLOBAL_description[2],
                    "<p></p>"
                  )
                },
                rccShinyTXT(language = GLOBAL_language)$fewcases1,
                " ",
                GLOBAL_hideLessThan,
                " ",
                rccShinyTXT(language = GLOBAL_language)$fewcases2,
                ".",
                "</div>",
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
#' @author Fredrik Sandin, RCC Uppsala-Örebro
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
        optionsList$error <- paste0("Column '", optionsList$outcome[i], "' in 'data' if not of type logical, factor or numeric")
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

    # geoUnitsHospitalInclude, geoUnitsCountyInclude, geoUnitsRegionInclude
    if (sum(optionsList$geoUnitsHospitalInclude, optionsList$geoUnitsCountyInclude, optionsList$geoUnitsRegionInclude) < 1) {
      optionsList$error <- paste0("At least one level of comparison (hospital/county/region) must be available")
      return(optionsList)
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
      if (optionsList$periodDateLevel == "quarter") {
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
        optionsList$periodStart <- head(sort(unique(tempPeriod)), 1)
        optionsList$periodEnd <- tail(sort(unique(tempPeriod)), 1)
        optionsList$periodValues <-
          paste0(
            rep(min(tempYearsUnique):max(tempYearsUnique), each = 4),
            rep(paste0("Q", 1:4), rep = length(tempYearsUnique))
          )
        optionsList$periodValues <- optionsList$periodValues[which(optionsList$periodValues == optionsList$periodStart):which(optionsList$periodValues == optionsList$periodEnd)]
      } else {
        optionsList$data$period <- as.numeric(format(optionsList$data$period, "%Y"))
        optionsList$periodStart <- min(optionsList$data$period, na.rm = TRUE)
        optionsList$periodEnd <- max(optionsList$data$period, na.rm = TRUE)
        optionsList$periodValues <- optionsList$periodStart:optionsList$periodEnd
      }
    } else {
      optionsList$periodDate <- FALSE
      optionsList$periodStart <- min(optionsList$data$period, na.rm = TRUE)
      optionsList$periodEnd <- max(optionsList$data$period, na.rm = TRUE)
      optionsList$periodValues <- optionsList$periodStart:optionsList$periodEnd
    }

    # periodLabel
    optionsList$periodLabel <-
      ifelse(
        length(optionsList$periodLabel) >= optionsList$whichLanguage,
        optionsList$periodLabel[optionsList$whichLanguage],
        optionsList$periodLabel[1]
      )

    # includeVariables
    includeVariables <- c(optionsList$outcome, "region", "landsting", "sjukhus", "sjukhuskod", "period")

    if (optionsList$idInclude)
      includeVariables <- c(includeVariables, optionsList$id)

    if (optionsList$idOverviewLinkInclude)
      includeVariables <- c(includeVariables, optionsList$idOverviewLink)

    if (optionsList$idAuthorisedToViewInclude)
      includeVariables <- c(includeVariables, optionsList$idAuthorisedToView)

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

    # hideLessThan
    optionsList$hideLessThan <-
      ifelse(
        optionsList$hideLessThan < 5,
        5,
        optionsList$hideLessThan
      )

    # npcrGroupPrivateOthers
    if (optionsList$npcrGroupPrivateOthers & sum(optionsList$geoUnitsHospitalInclude, optionsList$geoUnitsCountyInclude, optionsList$geoUnitsRegionInclude) < 3) {
      optionsList$npcrGroupPrivateOthers <- FALSE
      warning("'npcrGroupPrivateOthers' = TRUE can only be used when all levels of comparison (geoUnitsHospital, geoUnitsCounty and geoUnitsRegion) are active. 'npcrGroupPrivateOthers' set to FALSE.", call. = FALSE)
    }

    # data
    optionsList$data <-
      fixEncoding(
        subset(
          optionsList$data,
          select = includeVariables
        )
      )

    return(optionsList)
  }
