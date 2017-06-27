
library(shiny)
library(DT)
library(rccShiny)

shinyServer(function(input, output, clientData) {

  whichOutcome <-
    reactive({
      which(GLOBAL_outcomeTitle == input$param_outcome)
    })

  outcomeClassNumeric <-
    reactive({
      GLOBAL_outcomeClass[whichOutcome()] %in% c("difftime", "numeric", "integer")
    })

  output$numericTypeInput <-
    renderUI({
      tagList(
        conditionalPanel(
          condition = ifelse(outcomeClassNumeric(), "true", "false"),
          radioButtons(
            inputId = "param_numerictype",
            label = rccShinyTXT(language = GLOBAL_language)$presentation,
            choices = c(rccShinyTXT(language = GLOBAL_language)$median, rccShinyTXT(language = GLOBAL_language)$numericchoices_prop),
            selected = rccShinyTXT(language = GLOBAL_language)$median
          )
        )
      )
    })

  numericTypeProp <-
    reactive({
      input$param_numerictype == rccShinyTXT(language = GLOBAL_language)$numericchoices_prop
    })

  output$numericTypePropInput <-
    renderUI({
      tagList(
        conditionalPanel(
          condition = ifelse(outcomeClassNumeric(), paste0("input.param_numerictype == '",rccShinyTXT(language = GLOBAL_language)$numericchoices_prop, "'"), "false"),
          numericInput(
            inputId = "param_numerictype_prop",
            label = NULL,
            value = 30,
            min = 0,
            max = 1000,
            step = 1
          )
        )
      )
    })

  output$regionInput <-
    renderUI({
      tagList(
        conditionalPanel(
          condition = paste0(
            ifelse(GLOBAL_regionSelection, "true", "false"),
            " & ",
            ifelse(GLOBAL_geoUnitsRegionInclude, "true", "false")
          ),
          selectizeInput(
            inputId = "param_region",
            label = GLOBAL_regionLabel,
            choices = c(rccShinyTXT(language = GLOBAL_language)$all,GLOBAL_regionChoices),
            selected = GLOBAL_regionSelected,
            multiple = FALSE
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
              "input.tab!='fig_trend' & input.tab!='fig_map' & ",
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
              rccShinyLevelNames(
                ifelse(
                  GLOBAL_geoUnitsPatient,
                  "county_lkf",
                  "county"
                ),
                language = GLOBAL_language
              )
          )
        )
      )
    })

  output$ownhospitalInput <-
    renderUI({
      tagList(
        conditionalPanel(
          condition = paste0(
            "input.tab!='fig_map' & input.tab!='table_num' & input.tab!='table_pct' & input.tab!='table' & ",
            ifelse(GLOBAL_geoUnitsHospitalInclude, "true", "false")
          ),
          selectInput(
            inputId = "param_ownhospital",
            label = rccShinyTXT(language = GLOBAL_language)$hospitalinterest,
            choices = hospitalChoices(),
            selected = ""
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
          sliderInput(
            inputId = "param_period",
            label = GLOBAL_periodLabel,
            min = GLOBAL_periodStart,
            max = GLOBAL_periodEnd,
            step = 1,
            ticks = FALSE,
            value = rep(GLOBAL_periodEnd, 2),
            sep = ""
          )
        )
      )
    })

  output$periodSplitInput <-
    renderUI({
      tagList(
        conditionalPanel(
          condition = "!input.param_funnelplot & input.tab!='fig_trend' & input.tab!='fig_map' & input.param_period[0]!=input.param_period[1]",
          checkboxInput(
            inputId = "param_periodSplit",
            label = paste(
              rccShinyTXT(language = GLOBAL_language)$periodSplit1,
              tolower(GLOBAL_periodLabel),
              rccShinyTXT(language = GLOBAL_language)$periodSplit2
            ),
            value = FALSE
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
                  min = min(tempList$choices[1], na.rm = TRUE),
                  max = max(tempList$choices[2], na.rm = TRUE),
                  step = 1,
                  ticks = FALSE,
                  value = range(tempList$choices, na.rm = TRUE)
                )
              } else {
                selectizeInput(
                  inputId = paste0("userInputId", i),
                  label = tempList$label,
                  choices = tempList$choices,
                  selected = tempList$selected,
                  multiple = tempList$multiple
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
            value = FALSE
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
        privateOthersName <- npcrPreparePeriodRegionCountyHospitalVariables(language = GLOBAL_language,returnPrivateOthersNames = TRUE)
        landstingName <- privateOthersName[[GLOBAL_language]]$landsting
        sjukhusName <- privateOthersName[[GLOBAL_language]]$sjukhus_privatovriga
        tempHospitals <- tempHospitals[!(tempHospitals %in% GLOBAL_data$sjukhus[substr(GLOBAL_data$landsting,1,nchar(landstingName)) == landstingName])]
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
          input$param_numerictype_prop
        )
      } else {
        GLOBAL_outcomeTitle[whichOutcome()]
      }
    } else {
      GLOBAL_outcomeTitle[whichOutcome()]
    }
  })

  indSubtitlePeriod <- reactive({
    paste0(
      GLOBAL_periodLabel,
      ": ",
      ifelse(
        input[["param_period"]][1] == input[["param_period"]][2],
        input[["param_period"]][1],
        paste0(
          input[["param_period"]][1],
          "-",
          input[["param_period"]][2]
        )
      ),
      ". "
    )
  })

  indSubtitleGroupLessThan <- reactive({
    paste0(
      rccShinyTXT(language = GLOBAL_language)$fewcases1,
      " ",
      GLOBAL_hideLessThan,
      " ",
      rccShinyTXT(language = GLOBAL_language)$fewcases2,
      "."
    )
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
          hideLessThan,
          indSubtitleGroupLessThan(),
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
            tempText <-
              paste0(
                tempText,
                tempList$label,
                ": ",
                ifelse(
                  tempValues[1] == tempValues[2],
                  tempValues[1],
                  paste0(
                    tempValues[1],
                    "-",
                    tempValues[2]
                  )
                ),
                ". "
              )
          } else if (!(all(tempList$choices %in% tempValues))) {
            tempText <-
              paste0(
                tempText,
                tempList$label,
                ": ",
                paste(tempValues,collapse = "/"),
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
    indSubtitle(hideLessThan = GLOBAL_hideLessThan)
  })

  output$text2 <- renderText({
    indSubtitleUserInput()
  })

  output$tableTitle <- renderText({
    indTitle()
  })

  output$tableSubtitle1 <- renderText({
    indSubtitle(hideLessThan = GLOBAL_hideLessThan)
  })

  output$tableSubtitle2 <- renderText({
    indSubtitleUserInput()
  })

  output$theTabs <-
    renderUI({
      theTabs <-
        list(
          tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_compare, value = "fig_compare", plotOutput("indPlot"))
        )
      if (GLOBAL_outcomeClass[whichOutcome()] == "factor") {
        theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_n, value = "table_num", dataTableOutput("indTableNum"))
        theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$tab_p, value = "table_pct", dataTableOutput("indTablePct"))
      } else {
        theTabs[[length(theTabs) + 1]] <-
          tabPanel(
            title = rccShinyTabsNames(language = GLOBAL_language)$tab,
            value = "table",
            h2(textOutput("tableTitle")),
            p(textOutput("tableSubtitle1")),
            p(textOutput("tableSubtitle2")),
            dataTableOutput("indTable")
          )
        if (GLOBAL_geoUnitsCountyInclude) {
          theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$map, value = "fig_map", plotOutput("indMap"))
        }
      }
      theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$fig_trend, value = "fig_trend", plotOutput("indPlotTrend"))
      theTabs[[length(theTabs) + 1]] <- tabPanel(rccShinyTabsNames(language = GLOBAL_language)$description, includeHTML("./docs/description.html"))
      do.call(tabsetPanel,c(theTabs,id = "tab"))
    })

  dfInput <- reactive({

    dftemp <- GLOBAL_data

    dftemp$outcome <- dftemp[,GLOBAL_outcome[whichOutcome()]]
    if (outcomeClassNumeric() & numericTypeProp()) {
      dftemp$outcome <- dftemp$outcome <= input$param_numerictype_prop
    }

    if (input$tab != "fig_trend") {
      dftemp <- subset(dftemp, !is.na(period) & period %in% input[["param_period"]][1]:input[["param_period"]][2])
    }

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
        privateOthersName <- npcrPreparePeriodRegionCountyHospitalVariables(language = GLOBAL_language,returnPrivateOthersNames = TRUE)
        landstingName <- privateOthersName[[GLOBAL_language]]$landsting
        sjukhusName <- privateOthersName[[GLOBAL_language]]$sjukhus_privatovriga
        changeName <- substr(dftemp$landsting, 1, nchar(landstingName)) == landstingName
        dftemp$sjukhus[changeName] <- paste0(sjukhusName," - ",dftemp$region[changeName])
      }
    }
    # -----------------------

    dftemp$group <- dftemp[,rccShinyGroupVariable(label = input$param_levelpresent)]
    dftemp$group_ownhospital <- dftemp[,"sjukhus"] == input$param_ownhospital

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
    renderImage({

      x_width <- min(clientData$output_indPlot_width,700)
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
        fIndPlot(
          group = dfuse$group,
          group_hide_less_than = GLOBAL_hideLessThan,
          all_lab = rccShinyTXT(language = GLOBAL_language)$RIKET,
          emph_lab = emphLabel(dfuse),
          ind = dfuse$outcome,
          ind_title = ifelse(
            class(dfuse$outcome) %in% c("difftime", "numeric", "integer"),
            rccShinyTXT(language = GLOBAL_language)$median,
            rccShinyTXT(language = GLOBAL_language)$percent
          ),
          ind_noofcasestxt = rccShinyTXT(language = GLOBAL_language)$noofcases,
          period = if (input$param_periodSplit) {dfuse$period} else {NULL},
          x_lab = ifelse(
            class(dfuse$outcome) %in% c("difftime", "numeric", "integer"),
            rccShinyTXT(language = GLOBAL_language)$medianiqr,
            rccShinyTXT(language = GLOBAL_language)$percent
          ),
          legend_fixedtextwidth = TRUE,
          title = indTitle(),
          subtitle = if (indSubtitle() == "") {NULL} else {indSubtitle()},
          subtitle2 = if (indSubtitleUserInput() == "") {NULL} else {indSubtitleUserInput()},
          text_cex = ifelse(
            input$param_levelpresent == rccShinyLevelNames("hospital",language = GLOBAL_language),
            0.8,
            1
          ),
          point_cex = ifelse(
            input$param_levelpresent == rccShinyLevelNames("hospital", language = GLOBAL_language),
            1.8,
            3
          ),
          target_values = GLOBAL_targetValues,
          target_values_high = GLOBAL_sortDescending[whichOutcome()],
          funnelplot = input$param_funnelplot,
          subset = tempSubset,
          subset_lab = paste(input[["param_region"]], collapse = "/")
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

  output$indPlotTrend <-
    renderImage({

      x_width <- min(clientData$output_indPlotTrend_width, 700)
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
            period_factors = GLOBAL_periodStart:GLOBAL_periodEnd,
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
              period_factors = GLOBAL_periodStart:GLOBAL_periodEnd,
              period_alwaysinclude = TRUE
            )
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
              x <- append(x, list(as.numeric(tab_group$Period)))
              y <- append(y, list(as.numeric(tab_group[,i])))
              legend <- c(legend, i)
            }

            fLinePlot(
              x = x,
              y = y,
              legend = legend,
              #legend_textwidth=15,
              x_lim = c(GLOBAL_periodStart, GLOBAL_periodEnd),
              x_by = 1,
              y_lim = range(pretty(c(0, max(unlist(y), na.rm = TRUE)))),
              title = indTitle(),
              subtitle = paste0(
                input$param_ownhospital,
                ". ",
                indSubtitle(period = FALSE)
              ),
              subtitle2 = if (indSubtitleUserInput() == "") {NULL} else {indSubtitleUserInput()},
              x_lab = GLOBAL_periodLabel,
              y_lab = rccShinyTXT(language = GLOBAL_language)$percent,
              target_values = GLOBAL_targetValues,
              target_values_high = GLOBAL_sortDescending[whichOutcome()]
            )

          }

          x <- list()
          y <- list()
          legend <- vector()

          for (i in levels(dfuse$outcome)) {
            x <- append(x, list(as.numeric(tab_total$Period)))
            y <- append(y, list(as.numeric(tab_total[,i])))
            legend <- c(legend, i)
          }

          fLinePlot(
            x = x,
            y = y,
            legend = legend,
            #legend_textwidth=15,
            x_lim = c(GLOBAL_periodStart, GLOBAL_periodEnd),
            x_by = 1,
            y_lim = range(pretty(c(0, max(unlist(y), na.rm = TRUE)))),
            title = indTitle(),
            subtitle = paste0(
              rccShinyTXT(language = GLOBAL_language)$RIKET,
              ". ",
              indSubtitle(period = FALSE)
            ),
            subtitle2 = if (indSubtitleUserInput() == "") {NULL} else {indSubtitleUserInput()},
            x_lab = GLOBAL_periodLabel,
            y_lab = rccShinyTXT(language = GLOBAL_language)$percent,
            target_values = GLOBAL_targetValues,
            target_values_high = GLOBAL_sortDescending[whichOutcome()]
          )

        } else {

          x <- list()
          y <- list()
          legend <- vector()

          if (outcomeClassNumeric() & !numericTypeProp()) {
            y_varinterest <- "Median"
            y_varinterest_txt <- rccShinyTXT(language = GLOBAL_language)$median
          } else {
            y_varinterest <- "Procent"
            y_varinterest_txt <- rccShinyTXT(language = GLOBAL_language)$percent
          }

          for (i in unique(tab$group)) {
            x <- append(x, list(as.numeric(tab$Period[tab$group == i])))
            y <- append(y, list(as.numeric(tab[tab$group == i, y_varinterest])))
            legend <- c(legend, i)
          }

          master_col <- c("#e5e5e5","#cccccc","#b2b2b2","#999999","#7f7f7f","#666666","#ffb117","#db5524","#19975d")

          col <- rep("#000000", length(legend))
          tempRegionNames <- rccShinyRegionNames(language = GLOBAL_language, sort = TRUE)
          col[legend == tempRegionNames[1]] <- master_col[1]
          col[legend == tempRegionNames[2]] <- master_col[2]
          col[legend == tempRegionNames[3]] <- master_col[3]
          col[legend == tempRegionNames[4]] <- master_col[4]
          col[legend == tempRegionNames[5]] <- master_col[5]
          col[legend == tempRegionNames[6]] <- master_col[6]
          col[legend == rccShinyTXT(language = GLOBAL_language)$RIKET] <- master_col[7]
          col[legend == input$param_ownhospital] <- master_col[8]
          col[legend %in% input[["param_region"]]] <- master_col[9]

          fLinePlot(
            x = x,
            y = y,
            legend = legend,
            legend_textwidth = 15,
            x_lim = c(GLOBAL_periodStart, GLOBAL_periodEnd),
            x_by = 1,
            y_lim = range(
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
            title = indTitle(),
            subtitle = if (indSubtitle(period = FALSE) == "") {NULL} else {indSubtitle(period = FALSE)},
            subtitle2 = if (indSubtitleUserInput() == "") {NULL} else {indSubtitleUserInput()},
            x_lab = GLOBAL_periodLabel,
            y_lab = y_varinterest_txt,
            target_values = GLOBAL_targetValues,
            target_values_high = GLOBAL_sortDescending[whichOutcome()],
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

    tab

  },
  extensions = 'Buttons',
  options = list(
    columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = '_all')),
    language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
    searching = TRUE,
    paging = FALSE,
    dom = 'Bfrtip',
    buttons = list('excel', 'pdf', 'print')
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
          lab_period = GLOBAL_periodLabel,
          subset = tempSubset,
          subset_lab = paste(input[["param_region"]], collapse = "/")
        )

      colnames(tab)[1] <- input$param_levelpresent
      colnames(tab)[2:ncol(tab)] <- paste(colnames(tab)[2:ncol(tab)],"(%)")
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

    tab

  },
  extensions = 'Buttons',
  options = list(
    columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = '_all')),
    language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
    searching = TRUE,
    paging = FALSE,
    dom = 'Bfrtip',
    buttons = list('excel', 'pdf', 'print')
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

    tab

  },
  extensions = 'Buttons',
  options = list(
    columnDefs = list(list(className = 'dt-left', targets = 0),list(className = 'dt-right', targets = '_all')),
    language = list(emptyTable = rccShinyNoObservationsText(language = GLOBAL_language)),
    searching = TRUE,
    paging = FALSE,
    dom = 'Bfrtip',
    buttons = list('excel', 'pdf', 'print')
  ),
  rownames = FALSE
  )

  output$indMap <-
    renderImage({

      x_width <- min(clientData$output_indMap_width, 700)
      yx_ratio <- 1.4

      tab_order <- fMapPlot(value_order_return = TRUE)

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

        fMapPlot(
          value = if (showPercentage) {as.numeric(tab$Procent)} else {as.numeric(tab$Median)},
          value_lim = if (showPercentage) {c(0,100)} else {NULL},
          legend = ifelse(
            showPercentage,
            rccShinyTXT(language = GLOBAL_language)$percent,
            rccShinyTXT(language = GLOBAL_language)$median
          ),
          title = indTitle(),
          subtitle = if (indSubtitle() == "") {NULL} else {indSubtitle()},
          subtitle2 = if (indSubtitleUserInput() == "") {NULL} else {indSubtitleUserInput()},
          col = if (GLOBAL_sortDescending[whichOutcome()]){
            "#00b3f6"
          } else {
            NULL
          },
          ndec = ifelse(showPercentage, 0, 1),
          rds_path = "../../_data/"
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

})
