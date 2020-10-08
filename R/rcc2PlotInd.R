#' rcc2PlotInd
#' @description internal function.
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#' @keywords internal
#' @import highcharter
#' @export
rcc2PlotInd <-
  function(
    group = NULL,
    groupHideLessThan = FALSE,
    groupHideLessThanLabel = "(otillräcklig data)",
    groupHideLessThanCell = FALSE,
    groupMaxChars = NULL,
    ind = NULL,
    period = NULL,
    periodMaxN = 99,
    periodIncludeGroupsNotInLatestPeriod = TRUE,
    indType = class(ind),
    indNumeric = indType %in% c("difftime", "numeric", "integer"),
    indNumericExcludeNeg = TRUE,
    indNumericPercentiles = c(0.25, 0.50, 0.75),
    indFactorHide = NULL,
    indFactorSortByCols = NULL,
    indFactorShowN = ifelse(!is.null(indFactorHide), TRUE, FALSE),
    indShowPct = ifelse(indType == "factor", FALSE, TRUE),
    indTitle = ifelse(indNumeric, "Median", "Procent"),
    indNCasesTxt = "Antal fall",
    indNCasesOfTxt = "av",
    legendNCol = NULL,
    legendFixedTextWidth = TRUE,
    col = NULL,
    border = TRUE,
    xMax = if (indNumeric) {NULL} else {100},
    xBy = NULL,
    xLab = ifelse(indNumeric, "Median samt kvartilavstånd", "Procent"),
    allLab = "RIKET",
    emphLab = NULL,
    title = NULL,
    subtitle1 = NULL,
    subtitle2 = NULL,
    cexText = 1,
    cexPoint = 2.25,
    targetValues = NULL,
    targetValuesHigh = NULL,
    targetValuesLabels = c("Mellannivå av måluppfyllelse", "Hög nivå av måluppfyllelse"),
    funnelplot = FALSE,
    funnelplotProbs = c(0.05, 0.01),
    sort = TRUE,
    subset = NULL,
    subsetLab = "SUBSET",
    extra = NULL,
    extraLab = "EXTRA",
    outputHighchart = FALSE
  ) {

    # requireNamespace("gplots", quietly = TRUE)
    # requireNamespace("plyr", quietly = TRUE)
    # requireNamespace("Hmisc", quietly = TRUE)
    requireNamespace("highcharter", quietly = TRUE)

    rcc2LightenCol <-
      function(
        col = "#000000",
        factor = 0.8,
        bg = "#ffffff"
      ) {
        # Check
        if (length(factor) > 1) col <- col[1]
        factor[factor < 0] <- 0
        factor[factor > 1] <- 1

        R <- strtoi(substr(col, 2, 3), 16)
        G <- strtoi(substr(col, 4, 5), 16)
        B <- strtoi(substr(col, 6, 7), 16)

        R_bg <- strtoi(substr(bg[1], 2, 3), 16)
        G_bg <- strtoi(substr(bg[1], 2, 3), 16)
        B_bg <- strtoi(substr(bg[1], 2, 3), 16)

        RR <- R * factor + R_bg * (1 - factor)
        GG <- G * factor + G_bg * (1 - factor)
        BB <- B * factor + B_bg * (1 - factor)

        RR <- as.character(as.hexmode(round(RR)))
        GG <- as.character(as.hexmode(round(GG)))
        BB <- as.character(as.hexmode(round(BB)))

        RR_lengtone <- nchar(RR) == 1
        GG_lengtone <- nchar(GG) == 1
        BB_lengtone <- nchar(BB) == 1
        RR[RR_lengtone] <- paste0("0", RR[RR_lengtone])
        GG[GG_lengtone] <- paste0("0", GG[GG_lengtone])
        BB[BB_lengtone] <- paste0("0", BB[BB_lengtone])

        return(
          paste0(
            "#",
            RR,
            GG,
            BB
          )
        )
      }

    if (is.null(subset)) {
      subset <- rep(TRUE, length(group))
    }

    if (is.null(extra)) {
      extra <- rep(FALSE, length(group))
    }

    if (is.null(subtitle1) & !is.null(subtitle2)) {
      subtitle1 <- subtitle2
      subtitle2 <- NULL
    }

    if (is.null(targetValuesHigh)) {
      targetValuesHigh <-
        ifelse(
          indNumeric,
          FALSE,
          TRUE
        )
    }

    if (!(indType %in% c("difftime", "numeric", "integer", "logical", "factor"))) {
      stop(
        paste0(
          "Variable of class ",
          class(ind),
          " is not supported."
        )
      )
    }

    if (funnelplot) {
      if (indType != "logical") {
        funnelplot <- FALSE
        warning("funnelplot = TRUE is only allowed for ind of type logical => funnelplot set to FALSE in analysis")
      } else if (outputHighchart) {
        funnelplot <- FALSE
        warning("funnelplot = TRUE not allowed with outputHighchart = TRUE => funnelplot set to FALSE in analysis")
      } else {
        if (!is.null(period)) {
          period <- NULL
          warning("funnelplot = TRUE is not allowed together with period => period set to NULL in analysis")
        }
        if (!is.null(targetValues)) {
          targetValues <- NULL
          warning("funnelplot = TRUE is not allowed together with targetValues => targetValues set to NULL in analysis")
        }
      }
    }

    if (funnelplot) {
      funnelplotProbs <- sort(funnelplotProbs)
      sort <- TRUE
      targetValuesHigh <- TRUE
    }

    if (is.null(period)) {
      period <- rep(1, length(group))
    }
    show_periods <- utils::tail(sort(unique(period)), periodMaxN)
    num_periods <- length(show_periods)
    act_period <- utils::tail(show_periods, 1)

    # Handle missing values
    include <- !is.na(ind) & !is.na(period)
    if (indNumeric) {
      ind <- as.numeric(ind)
      if (indNumericExcludeNeg) include <- include & ind >= 0
    }

    if (!is.factor(group)) group <- factor(as.character(group))
    if (any(is.na(group))) {
      group <-
        factor(
          group,
          levels = c(levels(group), NA),
          exclude = NULL,
          labels = c(levels(group), "Uppgift saknas")
        )
    }

    group <- group[include]
    ind <- ind[include]
    period <- period[include]
    subset <- subset[include]
    extra <- extra[include]

    tabdata <-
      data.frame(
        group,
        ind,
        period,
        subset,
        extra,
        stringsAsFactors = FALSE
      )
    tabdata <-
      subset(
        tabdata,
        period %in% show_periods
      )
    byvars <- c("group", "period")

    if (indType == "factor") {
      factor_legend <- levels(ind)[!(levels(ind) %in% indFactorHide)]
    } else {
      factor_legend <- NULL
    }
    if (is.null(legendNCol)) {
      legendNCol <-
        ifelse(
          indType == "factor",
          ifelse(
            length(factor_legend) <= 3,
            length(factor_legend),
            ifelse(
              length(factor_legend) == 4,
              2,
              3
            )
          ),
          0
        )
    }
    legend_nrow <-
      ifelse(
        indType == "factor",
        ceiling(length(factor_legend) / legendNCol),
        0
      )

    # Tabulate
    summaryFunction <-
      function(x){
        if (indNumeric) {
          hide <-
            ifelse(
              hideLowVolume,
              sum(!is.na(x$ind),na.rm = TRUE) < groupHideLessThan,
              FALSE
            )
          hideCellLessThan <- FALSE
          if (hide) {
            measurements <- data.frame(NA, NA, NA, NA)
          } else {
            measurements <-
              data.frame(
                rbind(
                  stats::quantile(
                    as.numeric(x$ind),
                    probs = indNumericPercentiles,
                    na.rm = TRUE
                  )
                )
              )
            measurements <- cbind(
              measurements,
              sum(!is.na(x$ind), na.rm = TRUE)
            )
          }
          measurements <- cbind(measurements, hide, hideCellLessThan)
          names(measurements) <- c(
            "lower",
            "ind",
            "upper",
            "n",
            "hide",
            "hideCellLessThan"
          )
        } else if (indType == "logical") {
          hide <-
            ifelse(
              hideLowVolume,
              sum(!is.na(x$ind), na.rm = TRUE) < groupHideLessThan,
              FALSE
            )
          hideCellLessThan <-
            ifelse(
              hideLowVolume,
              (sum(x$ind, na.rm = TRUE) > 0 & sum(x$ind, na.rm = TRUE) < groupHideLessThanCell) |
                (sum(!x$ind, na.rm = TRUE) > 0 & sum(!x$ind, na.rm = TRUE) < groupHideLessThanCell),
              FALSE
            )
          if (hide) {
            measurements <- data.frame(NA, NA, NA)
          } else {
            measurements <-
              data.frame(
                rbind(
                  100 * Hmisc::binconf(
                    sum(x$ind, na.rm = TRUE),
                    sum(!is.na(x$ind), na.rm = TRUE),
                    method = "exact"
                  )
                )
              )
          }
          if (hide | hideCellLessThan) {
            measurements <- cbind(measurements, NA)
          } else {
            measurements <- cbind(
              measurements,
              paste0(
                sum(x$ind, na.rm = TRUE),
                " ",
                indNCasesOfTxt,
                " ",
                sum(!is.na(x$ind), na.rm = TRUE)
              ),
              stringsAsFactors = FALSE
            )
          }
          if (hide) {
            measurements <- cbind(measurements, NA)
          } else {
            measurements <- cbind(
              measurements,
              sum(!is.na(x$ind))
            )
          }
          measurements <- cbind(measurements, hide, hideCellLessThan)
          names(measurements) <- c(
            "ind",
            "lower",
            "upper",
            "n",
            "N",
            "hide",
            "hideCellLessThan"
          )
        } else if (indType == "factor") {
          hide <-
            ifelse(
              hideLowVolume,
              sum(!is.na(x$ind), na.rm = TRUE) < groupHideLessThan,
              FALSE
            )
          tempXInd <- factor(x$ind, levels = factor_legend)
          hideCellLessThan <-
            ifelse(
              hideLowVolume,
              any(table(tempXInd) > 0 & table(tempXInd) < groupHideLessThanCell),
              FALSE
            )
          measurements <- vector()
          if (hide) {
            for (i in factor_legend) {
              measurements <- c(measurements, NA)
            }
          } else {
            for (i in factor_legend) {
              measurements <- c(
                measurements,
                100*(sum(x$ind %in% i) / sum(!is.na(x$ind)))
              )
            }
          }
          measurements <- data.frame(rbind(measurements))
          if (hide) {
            measurements <- cbind(measurements, NA)
          } else {
            measurements <- cbind(
              measurements,
              sum(measurements)
            )
          }
          if (hide | hideCellLessThan) {
            measurements <- cbind(measurements, NA)
          } else {
            if (indFactorShowN & !is.null(indFactorHide)) {
              measurements <- cbind(
                measurements,
                paste0(
                  sum(x$ind %in% factor_legend),
                  " ",
                  indNCasesOfTxt,
                  " ",
                  sum(!is.na(x$ind), na.rm = TRUE)
                ),
                stringsAsFactors = FALSE
              )
            } else {
              measurements <- cbind(
                measurements,
                sum(!is.na(x$ind), na.rm = TRUE)
              )
            }
          }
          measurements <- cbind(measurements, hide, hideCellLessThan)
          names(measurements) <- c(
            paste0(
              "factor",
              1:(length(measurements) - 4)
            ),
            "ind",
            "n",
            "hide",
            "hideCellLessThan"
          )
        }
        return(
          measurements
        )
      }

    if (sum(subset) == 0) {

      if (!outputHighchart) {
        plot(
          x = 1,
          y = 1,
          type = "n",
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
      }
      warning("Nothing plotted because subset is empty.")

    } else {

      hideLowVolume <- as.logical(groupHideLessThan)

      tabdata_subset <-
        subset(
          tabdata,
          subset
        )
      tabdata_subset$group <- factor(tabdata_subset$group)
      tab <-
        plyr::ddply(
          .data = tabdata_subset,
          .variables = byvars,
          .fun = summaryFunction,
          .drop = FALSE
        )

      if (!periodIncludeGroupsNotInLatestPeriod) {
        include_groups <- sort(unique(subset(tabdata,period == act_period)$group))
        tab <-
          subset(
            tab,
            group %in% include_groups
          )
      }

      subsetUniqueGroups <- unique(tabdata$group[tabdata$subset & tabdata$period == act_period])
      if (!all(tabdata$subset) & !(length(subsetUniqueGroups) == 1 & all(subsetUniqueGroups %in% subsetLab))) {
        tab_subset <-
          plyr::ddply(
            .data = subset(tabdata, subset),
            .variables = byvars[byvars != "group"],
            .fun = summaryFunction,
            .drop = FALSE
          )
        tab_subset <- tab_subset[intersect(names(tab_subset), names(tab))]
        tab_subset$group <- subsetLab

        tab <-
          rbind(
            tab,
            tab_subset[, names(tab)]
          )
      }

      if (any(extra)) {
        tab_extra <-
          plyr::ddply(
            .data = subset(tabdata, extra),
            .variables = byvars[byvars != "group"],
            .fun = summaryFunction,
            .drop = FALSE
          )
        tab_extra <- tab_extra[intersect(names(tab_extra), names(tab))]
        tab_extra$group <- extraLab

        tab <-
          rbind(
            tab,
            tab_extra[, names(tab)]
          )
      }

      if (!is.null(allLab)) {
        tab_all <-
          plyr::ddply(
            .data = tabdata,
            .variables = byvars[byvars != "group"],
            .fun=summaryFunction,
            .drop=FALSE
          )
        tab_all <- tab_all[intersect(names(tab_all), names(tab))]
        tab_all$group <- allLab

        tab <-
          rbind(
            tab,
            tab_all[, names(tab)]
          )
      }

      # Funnelplot
      if (funnelplot) {
        temp_ind_all <-
          as.numeric(
            subset(
              tab,
              group == allLab &
                period == act_period
            )$ind
          ) / 100
        temp_x <- temp_ind_all * as.numeric(tab$N)
        temp_n <- as.numeric(tab$N)
        # Fix for hide
        temp_x[is.na(temp_x)] <- 0
        temp_n[is.na(temp_n)] <- 0
        for (i in 1:length(funnelplotProbs)) {
          temp_binconf <-
            Hmisc::binconf(
              x = temp_x,
              n = temp_n,
              method = "exact",
              alpha = funnelplotProbs[i]
            )
          colnames(temp_binconf) <-
            paste0(
              "funnelplot_p",
              i,
              "_",
              c("est", "lo", "hi")
            )
          tab <-
            cbind(
              tab,
              100*temp_binconf
            )
        }
      }

      # Determine sorting variable
      if (indType == "factor") {
        tab$ind_sort <-
          round(
            rowSums(
              apply(
                cbind(
                  tab[
                    ,
                    if (!is.null(indFactorSortByCols)) {
                      paste0(
                        "factor",
                        1:indFactorSortByCols
                      )
                    } else {
                      substr(names(tab), 1, 6) == "factor"
                    }
                    ]
                ),
                2,
                as.numeric
              )
            ),
            6
          )
      } else if (funnelplot) {
        tab$ind_sort <- tab$funnelplot_p1_lo
      } else {
        tab$ind_sort <- tab$ind
        tab$lower <- as.numeric(tab$lower)
        tab$upper <- as.numeric(tab$upper)
      }
      tab$ind <- as.numeric(tab$ind)
      tab$ind_sort <- as.numeric(tab$ind_sort)

      act <- tab$period == act_period

      # Colors
      col_ind_periods <- "#A29B96"
      col_ind_act <- "#00b3f6"
      col_ind_all <- "#ffb117"
      col_ind_emph <- "#db5524"
      col_ind_subset <- "#19975d"

      col_target_1 <- rcc2LightenCol("#ffb117", factor = 0.4)
      col_target_2 <- rcc2LightenCol("#19975d", factor = 0.4)

      col_factors <- c(
        "#00b3f6","#ffb117","#005092","#19975d","#e56284","#66cccc","#db5524","#7f3705","#7c458a","#95bf5d",
        "#7f7f7f","#8c8c8c","#999999","#a6a6a6","#b2b2b2","#bfbfbf","#cccccc","#d9d9d9","#e5e5e5","#f2f2f2"
      )

      if (is.null(col)) {
        if (indType == "factor") {
          col <- col_factors
        } else {
          tab$col <- rep(col_ind_periods, nrow(tab))
          tab$col[act] <- col_ind_act
          tab$col[act & tab$group %in% allLab] <- col_ind_all
          tab$col[act & tab$group %in% emphLab] <- col_ind_emph
          tab$col[act & tab$group %in% subsetLab] <- col_ind_subset
        }
      }

      tab$col_border <- "#7f7f7f"
      tab$col_border[tab$hide] <- "transparent"

      tab$col_text <- "#000000"
      tab$col_text[tab$hide] <- "#7f7f7f"

      # Order
      if (sort) {
        if (targetValuesHigh) {
          o <-
            order(
              tab[act,]$ind_sort,
              decreasing = TRUE
            )
        } else {
          o <-
            order(
              tab[act,]$ind_sort,
              tab[act,]$group
            )
        }
      } else {
        o <- 1:nrow(tab[act,])
      }

      # Shorten group names
      if (!is.null(groupMaxChars)) {
        tab$group[nchar(tab$group) > groupMaxChars] <-
          paste0(
            substr(
              tab$group[nchar(tab$group)>groupMaxChars],
              1,
              groupMaxChars - 3
            ),
            "..."
          )
      }

      alphacol <-
        utils::tail(
          255 * seq(0.25, 1, length.out = max(2, min(periodMaxN, num_periods))),
          min(periodMaxN,num_periods)
        )

      tab_list <- list()
      for (i in 1:num_periods) {
        tab_list[[i]] <-
          subset(
            tab,
            period == show_periods[i]
          )
        tab_list[[i]] <- tab_list[[i]][o,]

        if (indType != "factor") {
          tab_list[[i]]$col <-
            rcc2LightenCol(
              col = tab_list[[i]]$col,
              factor = i / num_periods
            )
        }
      }

      # x-axis label and ticks
      barheight <- 1
      barheight_factor <- 1.4
      if (is.null(xMax)) {
        xMax <-
          max(
            pretty(
              c(
                0,
                ifelse(
                  indNumeric,
                  max(tab$upper, na.rm = TRUE),
                  max(tab$ind, na.rm = TRUE)
                )
              )
            )
          )
      }
      x_lim <- c(0, xMax)

      if (is.null(xBy)) {
        x_ticks <- pretty(x_lim)
      } else {
        x_ticks <-
          seq(
            x_lim[1],
            x_lim[2],
            xBy
          )
      }

      # Output Highchart
      if (outputHighchart) {

        # Ugly solution in order to change color of individual labels
        tempHideLabels <- as.character(tab_list[[num_periods]]$group[tab_list[[num_periods]]$hide])
        for (i in 1:length(tab_list)) {
          tab_list[[i]]$groupOriginal <- tab_list[[i]]$group
          levels(tab_list[[i]]$group)[levels(tab_list[[i]]$group) %in% tempHideLabels] <-
            paste0(
              "_(hide)_",
              levels(tab_list[[i]]$group)[levels(tab_list[[i]]$group) %in% tempHideLabels],
              " ",
              groupHideLessThanLabel
            )

          tab_list[[i]]$n[tab_list[[i]]$hideCellLessThan] <- "-"
        }


        tempPlot <-
          highchart() %>%
          hc_boost(
            enabled = FALSE
          ) %>%
          hc_xAxis(
            categories = tab_list[[num_periods]]$group,
            labels = list(
              formatter = JS(
                "function() {",
                "var tempColor = '#000000';",
                "var tempString = this.value.toString();",
                "if (tempString.substr(0, 8) == '_(hide)_') {tempColor = '#7f7f7f'; tempString = tempString.replace('_(hide)_', '');}",
                paste0("return '<span style = \"color: ' + tempColor + '; font-size: ", round(12 * cexText), "px;\">' + tempString + '</span>';"),
                "}"
              ),
              useHTML = TRUE
            ),
            tickWidth = 0,
            floor = 0,
            ceiling = length(tab_list[[num_periods]]$group) - 1,
            lineColor = "#bdbdbd"
          ) %>%
          hc_yAxis(
            reversedStacks = FALSE,
            min = 0,
            max = xMax,
            tickInterval = x_ticks[2] - x_ticks[1],
            labels = list(
              style = list(
                color = "#000000",
                fontSize = paste0(round(12 * cexText), "px")
              )
            ),
            tickLength = 5,
            tickWidth = 2,
            lineWidth = 2,
            lineColor = "#bdbdbd",
            gridLineColor = "#bdbdbd",
            title = list(
              text = xLab,
              style = list(
                color = "#000000",
                fontWeight = "bold",
                fontSize = paste0(round(12 * cexText), "px")
              )
            )
          ) %>%
          hc_legend(
            enabled = length(tab_list) > 1,
            symbolHeight = round(10 * cexText),
            symbolWidth = round(10 * cexText),
            symbolRadius = 0,
            itemStyle = list(
              color = "#000000",
              fontWeight = "normal",
              fontSize = paste0(round(12 * cexText), "px"),
              cursor = "default"
            )
          )

        if (!is.null(title)) {
          tempPlot <- tempPlot %>%
            hc_title(
              text = title,
              align = "left"
            )
        }
        if (!is.null(subtitle1) | !is.null(subtitle2)) {
          tempSubtitle <- paste0(
            if (!is.null(subtitle1)) {subtitle1} else {""},
            "<br>",
            if (!is.null(subtitle2)) {subtitle2} else {""}
          )
          tempPlot <- tempPlot %>%
            hc_subtitle(
              text = tempSubtitle,
              align = "left"
            )
        }

        if (!is.null(targetValues)) {
          if (length(targetValues) > 1) {
            tempPlot <- tempPlot %>%
              hc_yAxis(
                plotBands = list(
                  list(
                    color = col_target_1,
                    from = min(targetValues),
                    to = max(targetValues)
                  ),
                  list(
                    color = col_target_2,
                    from = ifelse(
                      targetValuesHigh,
                      max(targetValues),
                      0
                    ),
                    to = ifelse(
                      targetValuesHigh,
                      xMax,
                      min(targetValues)
                    )
                  )
                )
              )
          } else {
            tempPlot <- tempPlot %>%
              hc_yAxis(
                plotBands = list(
                  list(
                    color = col_target_2,
                    from = ifelse(
                      targetValuesHigh,
                      max(targetValues),
                      0
                    ),
                    to = ifelse(
                      targetValuesHigh,
                      xMax,
                      min(targetValues)
                    )
                  )
                )
              )
          }

        }

        if (indNumeric) {

          tempCexPoint <- cexPoint / 2.25

          tempPlot <- tempPlot %>%
            hc_chart(
              inverted = TRUE,
              spacing = c(20, 20, 20, 20)
            ) %>%
            hc_plotOptions(
              scatter = list(
                events = list(
                  legendItemClick = "function() {return false;}"
                )
              )
            ) %>%
            hc_tooltip(
              shared = TRUE,
              headerFormat = "<span style='font-size: 10px'>{point.key}</span><br>",
              pointFormat = paste0("<span style='color:{point.color}'>\u25CF</span> <span style='font-size: 10px'>", ifelse(length(tab_list) > 1, "{series.name}: ", ""), "<b>{point.yRound} ({point.lowRound}-{point.highRound}) dagar</b> (N = {point.n})</span><br>"),
              useHTML = TRUE,
              outside = TRUE
            )

          tempPlacements <- seq(0.3, -0.3, length.out = length(tab_list))
          for (i in 1:length(tab_list)) {
            tempLegendCol <-
              ifelse(
                i == length(tab_list),
                col_ind_act,
                tab_list[[i]]$col[1]
              )
            tab_list[[i]]$yRound <- round(tab_list[[i]]$ind, 1)
            tab_list[[i]]$lowRound <- round(tab_list[[i]]$lower, 1)
            tab_list[[i]]$highRound <- round(tab_list[[i]]$upper, 1)
            tempPlot <- tempPlot %>%
              hc_add_series(data = tab_list[[i]], type = "scatter", id = paste0("scatter", i), mapping = hcaes(x = groupOriginal, y = ind, color = col), name = tab_list[[i]]$period[1], pointPlacement = tempPlacements[i], showInLegend = TRUE, color = tempLegendCol, marker = list(symbol = "circle", radius = 6 * tempCexPoint, lineWidth = 5 * tempCexPoint, lineColor = NULL, fillColor = "#ffffff"), zIndex = 2 * i, enableMouseTracking = FALSE) %>%
              hc_add_series(data = tab_list[[i]], type = "errorbar", linkedTo = paste0("scatter", i), mapping = hcaes(x = groupOriginal, low = lower, high = upper, color = col), name = tab_list[[i]]$period[1], pointPlacement = tempPlacements[i], showInLegend = FALSE, color = tempLegendCol, whiskerLength = 0, lineWidth = 5 * tempCexPoint, zIndex = 2 * (i - 1) + 1, enableMouseTracking = TRUE)
          }

        } else if (indType == "factor") {

          tempPlot <- tempPlot %>%
            hc_chart(
              type = "bar",
              spacing = c(20, 20, 20, 20)
            ) %>%
            hc_plotOptions(
              bar = list(
                stacking = "normal",
                pointPadding = ifelse(length(tab_list) > 1, -0.2, 0.1),
                groupPadding = 0.15,
                borderColor = "#7f7f7f",
                events = list(
                  legendItemClick = "function() {return false;}"
                )
              )
            ) %>%
            hc_tooltip(
              formatter = JS(
                "function () {",
                "var seriesAll = this.point.series.chart.series, ",
                "hoverIndex = this.point.series.xData.indexOf(this.point.x), ",
                "hoverStack = this.series.userOptions.stack, ",
                "str = '<span style=\"font-size: 10px\">' + this.key + ' (' + hoverStack + ')</span><br>'",
                ";",
                "$.each(seriesAll, function(i, s) {",
                "if (s.userOptions.stack == hoverStack) {",
                paste0(
                  "str += '<span style=\"font-size: 10px\">",
                  ifelse(
                    indFactorShowN,
                    "<b>' + s.data[hoverIndex].ind.toFixed(0) + ' %</b> (' + s.data[hoverIndex].n + ')",
                    "(N = ' + s.data[hoverIndex].n + ')"
                  ),
                  "</span><br>'"
                ),
                "return false;",
                "}",
                "});",
                "$.each(seriesAll, function(i, s) {",
                "if (s.userOptions.stack == hoverStack) {",
                "str += '<span style=\"color:' + s.data[0].color + '\">\u25A0</span><span style=\"font-size: 10px\"> ' + s.name + ': <b>' + s.data[hoverIndex].y.toFixed(0) + ' %</b></span><br>';",
                "}",
                "});",
                "return str;",
                "}"
              ),
              useHTML = TRUE,
              outside = TRUE
            )

          tempNFactors <- sum(substr(colnames(tab_list[[num_periods]]), 1, 6) %in% "factor")
          for (i in 1:length(tab_list)) {
            for (j in 1:tempNFactors) {
              tab_list[[i]]$tempInd <- as.numeric(tab_list[[i]][, paste0("factor", j)])
              tab_list[[i]]$tempCol <- rcc2LightenCol(col = col[j], factor = i / num_periods)
              tab_list[[i]]$tempColBorder <- "#7f7f7f"
              tab_list[[i]]$tempWidthBorder <- 1
              if (!is.null(emphLab) & i == length(tab_list)) {
                tab_list[[i]]$tempColBorder[tab_list[[i]]$groupOriginal %in% emphLab] <- "#000000"
                tab_list[[i]]$tempWidthBorder[tab_list[[i]]$groupOriginal %in% emphLab] <- 2
              }
              tempPlot <- tempPlot %>%
                hc_add_series(data = tab_list[[i]], stack = tab_list[[i]]$period[1], type = "bar", mapping = hcaes(x = groupOriginal, y = tempInd, color = tempCol, borderColor = tempColBorder, borderWidth = tempWidthBorder), name = factor_legend[j], showInLegend = i == length(tab_list), color = tab_list[[i]]$tempCol[1])
            }
          }

        } else {

          tempPlot <- tempPlot %>%
            hc_chart(
              type = "bar",
              spacing = c(20, 20, 20, 20)
            ) %>%
            hc_plotOptions(
              bar = list(
                pointPadding = ifelse(length(tab_list) > 1, -0.2, 0.1),
                groupPadding = 0.15,
                borderColor = "#7f7f7f",
                events = list(
                  legendItemClick = "function() {return false;}"
                )
              )
            ) %>%
            hc_tooltip(
              shared = TRUE,
              headerFormat = "<span style='font-size: 10px'>{point.key}</span><br>",
              pointFormat = paste0("<span style='color:{point.color}'>\u25A0</span> <span style='font-size: 10px'>", ifelse(length(tab_list) > 1, "{series.name}: ", ""), "<b>{point.yRound} %</b> ({point.n})</span><br>"),
              useHTML = TRUE,
              outside = TRUE
            )

          for (i in 1:length(tab_list)) {
            tempLegendCol <-
              ifelse(
                i == length(tab_list),
                col_ind_act,
                tab_list[[i]]$col[1]
              )
            tab_list[[i]]$yRound <- round(tab_list[[i]]$ind)
            tempPlot <- tempPlot %>%
              hc_add_series(data = tab_list[[i]], type = "bar", mapping = hcaes(x = groupOriginal, y = ind, color = col), name = tab_list[[i]]$period[1], showInLegend = TRUE, color = tempLegendCol)
          }

        }

        tempPlot

      } else {

        if (indType %in% c("logical", "factor")) {
          y_bp <-
            0.5 * barheight_factor * barheight +
            barheight * (barheight_factor + 0.5 * (num_periods - 1)) * (0:(nrow(tab_list[[num_periods]]) - 1))
          y_lim <- c(
            0,
            barheight * (barheight_factor + 0.5 * (num_periods - 1)) * nrow(tab_list[[num_periods]])
          )
        } else {
          y_bp <-
            0.5 * barheight_factor * barheight +
            barheight_factor * barheight * num_periods * (0:(nrow(tab_list[[num_periods]]) - 1))
          y_lim <- c(
            0,
            num_periods * barheight_factor * barheight * nrow(tab_list[[num_periods]])
          )
        }

        # Change margins
        linchheight <- strheight("X", "inch", cex = cexText)
        linchwidth <- strwidth("X", "inch", cex = cexText)
        linch_label <-
          3 * linchwidth +
          max(strwidth(tab$group, "inch", cex = cexText), na.rm=TRUE)
        linch_i <-
          3 * linchwidth +
          max(strwidth(c(indTitle, round(tab$ind,1)), "inch", cex = cexText), na.rm=TRUE)
        linch_n <-
          3 * linchwidth +
          max(
            c(
              strwidth(tab$n, "inch", cex = cexText),
              ifelse(
                !indNumeric & any(is.na(tab_list[[num_periods]]$n)),
                strwidth(groupHideLessThanLabel, "inch", cex = 0.7 * cexText),
                0
              )
            ),
            na.rm=TRUE
          )

        par(
          mai = c(
            ifelse(!is.null(xLab), 6, 4) * linchheight +
              legend_nrow * (indType == "factor") * linchheight +
              (num_periods > 1) * linchheight +
              (indType == "factor" & num_periods > 1) * linchheight +
              (funnelplot) * linchheight +
              (!is.null(targetValues)) * 2 * linchheight,
            ifelse(
              indNumeric,
              linch_label + linch_n + linch_i,
              linch_label + linch_n
            ),
            (2 +
               2.5 * (!is.null(title)) +
               2.5 * (!is.null(subtitle1)) +
               1.5 * (!is.null(subtitle2))
            ) * linchheight,
            ifelse(
              indNumeric | (!indNumeric & !indShowPct),
              2,
              5
            ) * linchheight
          ),
          bg = "#ffffff",
          xpd = TRUE
        )

        # Empty plot
        plot(
          x = x_lim,
          y = y_lim,
          axes = FALSE,
          type = "n",
          xlab = "",
          ylab = ""
        )

        luserheight <- strheight("X", "user", cex = cexText)
        luserwidth <- strwidth("X", "user", cex = cexText)

        pos0x <- grconvertX(x = 0, from = "nfc", to = "user")
        pos1x <- grconvertX(x = 1, from = "nfc", to = "user")
        pos0y <- grconvertY(y = 0, from = "nfc", to = "user")
        pos1y <- grconvertY(y = 1, from = "nfc", to = "user")

        y_n_label <-
          grconvertY(
            grconvertY(
              y = y_lim[2],
              from = "user",
              to = "inches"
            ) + linchheight,
            from = "inches",
            to = "user"
          )

        # Target values (area)
        if (!is.null(targetValues)) {
          if (length(targetValues) > 1) {
            rect(
              xleft = min(targetValues),
              ybottom = 0,
              xright = max(targetValues),
              ytop = y_lim[2],
              col = col_target_1,
              border = NA
            )
          }
          rect(
            xleft =
              ifelse(
                targetValuesHigh,
                max(targetValues),
                0
              ),
            ybottom = 0,
            xright =
              ifelse(
                targetValuesHigh,
                xMax,
                min(targetValues)
              ),
            ytop = y_lim[2],
            col = col_target_2,
            border = NA
          )
        }

        # Grid
        for (i in x_ticks) {
          lines(
            x = rep(i, 2),
            y = c(0, y_lim[2]),
            lwd = 1,
            col = "#bdbdbd"
          )
        }

        # Title
        printTitle <- !is.null(title)
        printSubtitle1 <- !is.null(subtitle1)
        printSubtitle2 <- !is.null(subtitle2)
        text(
          x = pos0x,
          y = y_lim[2] +
            4 * luserheight,
          labels = ifelse(
            printSubtitle2,
            subtitle2,
            ""
          ),
          pos = 4,
          cex = cexText,
          offset = 1
        )
        text(
          x = pos0x,
          y = y_lim[2] +
            4 * luserheight + printSubtitle2 * 1.5 * luserheight,
          labels = ifelse(
            printSubtitle1,
            subtitle1,
            ""
          ),
          pos = 4,
          cex = cexText,
          offset = 1
        )
        text(
          x = pos0x,
          y = y_lim[2] +
            4 * luserheight +
            ((1*printSubtitle1 + 1*printSubtitle2) * 1.5 + 0.5*(printSubtitle1 | printSubtitle2)) * luserheight,
          labels = ifelse(
            printTitle,
            title,
            ""
          ),
          pos = 4,
          cex = 1.5 * cexText,
          offset = 1
        )

        # Axis
        axis(
          side = 1,
          pos = y_lim[1],
          at = x_ticks,
          cex.axis = cexText,
          las = 1,
          lwd = 3,
          col = "#d9d9d9"
        )

        # Axis label
        y_xlab_zeropos <-
          ifelse(
            indType == "factor",
            ifelse(
              num_periods > 1,
              pos0y + 2 * luserheight,
              pos0y
            ),
            ifelse(
              num_periods > 1 | funnelplot,
              pos0y + luserheight,
              pos0y
            )
          ) + legend_nrow * (indType == "factor") * luserheight +
          (!is.null(targetValues)) * 2 * luserheight
        text(
          x = 0.5*xMax,
          y = y_lim[1] - 0.6*(y_lim[1] - y_xlab_zeropos),
          labels = xLab,
          cex = cexText,
          font = 2
        )

        # Plot
        if (indNumeric) {
          for (i in 1:num_periods) {
            gplots::plotCI(
              x = rev(tab_list[[i]]$ind),
              y = y_bp + (num_periods - i) * barheight,
              li = rev(tab_list[[i]]$lower),
              ui = rev(tab_list[[i]]$upper),
              col = rev(tab_list[[i]]$col),
              pt.bg = par("bg"),
              lwd = 2.5*cexPoint,
              add = TRUE,
              pch = 21,
              cex = cexPoint,
              err = "x",
              sfrac = 0,
              gap = 0.75 * cexPoint / 2.25
            )
          }
        } else if (indType == "factor") {
          for (i in 1:num_periods) {
            for (j in rev(1:nrow(tab_list[[i]]))) {
              temp_tab <- tab_list[[i]][j, substr(names(tab),1,6) == "factor"]
              temp_tab <- cumsum(c(0, temp_tab))
              temp_col <-
                rcc2LightenCol(
                  col = col,
                  factor = i / num_periods
                )
              for (k in 2:length(temp_tab)) {
                rect(
                  xleft = temp_tab[k-1],
                  xright = temp_tab[k],
                  ybottom =
                    y_bp[length(y_bp) + 1 - j] -
                    0.5 * barheight +
                    (num_periods - i) * 0.5 * barheight,
                  ytop =
                    y_bp[length(y_bp) + 1 - j] +
                    0.5 * barheight +
                    (num_periods - i) * 0.5 * barheight,
                  col = temp_col[k-1],
                  border =
                    ifelse(
                      i == num_periods & tab_list[[i]]$group[j] %in% emphLab,
                      "#000000",
                      ifelse(
                        border,
                        "#7f7f7f",
                        NA
                      )
                    ),
                  lwd =
                    ifelse(
                      i == num_periods & tab_list[[i]]$group[j] %in% emphLab,
                      2,
                      1
                    )
                )
              }
            }
          }

          # Legend
          legend(
            x = 0.5 * xMax,
            y = pos0y +
              (num_periods > 1) * 2 * luserheight +
              (!is.null(targetValues)) * 2 * luserheight,
            legend = factor_legend,
            col = col,
            pch = 15,
            pt.cex = 1.75,
            bty = "n",
            cex = 0.8 * cexText,
            xjust = 0.5,
            yjust = 0,
            y.intersp = 1,
            ncol = legendNCol,
            text.width =
              if (legendFixedTextWidth) {
                max(strwidth(factor_legend))
              } else {
                NULL
              }
          )
        } else {
          # Funnelplot
          if (funnelplot) {
            temp_funnelplot_alphacol <- 255 * seq(0.75, 0.25, length.out = length(funnelplotProbs))
            temp_funnelplot_col <-
              rgb(
                t(col2rgb("#e74903")),
                alpha = temp_funnelplot_alphacol,
                maxColorValue = 255
              )
            temp_funnelplot_data <- tab_list[[num_periods]]
            temp_funnelplot_sectionheight <- barheight * barheight_factor
            temp_funnelplot_prev_lo <- 0
            temp_funnelplot_prev_hi <- 100
            for (i in 1:length(funnelplotProbs)) {
              temp_funnelplot_plot_lo <- rev(temp_funnelplot_data[, paste0("funnelplot_p", i, "_lo")])
              temp_funnelplot_plot_hi <- rev(temp_funnelplot_data[, paste0("funnelplot_p", i, "_hi")])
              rect(
                xleft = temp_funnelplot_prev_lo,
                xright = temp_funnelplot_plot_lo,
                ybottom = y_bp - 0.5 * temp_funnelplot_sectionheight,
                ytop = y_bp + 0.5 * temp_funnelplot_sectionheight,
                col = temp_funnelplot_col[i],
                border = NA
              )
              rect(
                xleft = temp_funnelplot_plot_hi,
                xright = temp_funnelplot_prev_hi,
                ybottom = y_bp - 0.5 * temp_funnelplot_sectionheight,
                ytop = y_bp + 0.5 * temp_funnelplot_sectionheight,
                col = temp_funnelplot_col[i],
                border=NA
              )
              temp_funnelplot_prev_lo <- temp_funnelplot_plot_lo
              temp_funnelplot_prev_hi <- temp_funnelplot_plot_hi
            }
            # Funnelplot legend
            legend(
              x = 0.5 * xMax,
              y = pos0y,
              legend = paste0("p < ", funnelplotProbs),
              col = temp_funnelplot_col,
              pch = 15,
              pt.cex = 1.75,
              bty = "n",
              cex = 0.8 * cexText,
              xjust = 0.5,
              yjust = 0,
              y.intersp = 1,
              ncol = length(funnelplotProbs)
            )
          }

          for (i in 1:num_periods) {
            rect(
              xleft = 0,
              xright = rev(tab_list[[i]]$ind),
              ybottom =
                y_bp -
                0.5 * barheight +
                (num_periods - i) * 0.5 * barheight,
              ytop =
                y_bp +
                0.5 * barheight +
                (num_periods - i) * 0.5 * barheight,
              col = rev(tab_list[[i]]$col),
              border =
                if (border) {
                  rev(tab_list[[i]]$col_border)
                } else {
                  NA
                }
            )
          }
        }

        # Period legend
        if (num_periods > 1) {
          period_legend_col <-
            rcc2LightenCol(
              col = col_ind_periods,
              factor = (1:num_periods / num_periods)
            )
          if (indType != "factor") {
            period_legend_col <- c(period_legend_col[1:(length(period_legend_col) - 1)], col_ind_act)
          }
          legend(
            x = 0.5 * xMax,
            y = pos0y +
              (!is.null(targetValues)) * 2 * luserheight,
            legend =
              paste0(
                c(
                  rep(
                    "",
                    length(show_periods) - 1
                  ),
                  "*"
                ),
                show_periods
              ),
            col = period_legend_col,
            pch = 15,
            pt.cex = 1.75 * cexText,
            bty = "n",
            cex = 0.8 * cexText,
            xjust = 0.5,
            yjust = 0,
            y.intersp = 1,
            ncol = num_periods,
            text.width = cexText * max(strwidth(show_periods))
          )
        }

        # Target values legend
        if (!is.null(targetValues)) {
          legendTargetValues <- targetValuesLabels
          legendTargetValuesCol <- c(col_target_1, col_target_2)
          if (length(targetValues) == 1) {
            legendTargetValues <- legendTargetValues[2]
            legendTargetValuesCol <- legendTargetValuesCol[2]
          }
          if (!targetValuesHigh) {
            legendTargetValues <- rev(legendTargetValues)
            legendTargetValuesCol <- rev(legendTargetValuesCol)
          }
          legend(
            x = 0.5 * xMax,
            y = pos0y,
            legend = legendTargetValues,
            col = legendTargetValuesCol,
            pch = 15,
            pt.cex = 1.75,
            bty = "n",
            cex = 0.8 * cexText,
            xjust = 0.5,
            yjust = 0,
            y.intersp = 1,
            ncol = length(legendTargetValuesCol)
          )
        }

        # Group labels
        if (indNumeric) {
          text(
            x = ((linch_n + linch_i) / (linch_label + linch_n + linch_i)) * pos0x,
            y = y_bp,
            labels = rev(tab_list[[num_periods]]$group),
            pos = 2,
            cex = cexText,
            col = rev(tab_list[[num_periods]]$col_text)
          )
          text(
            x = (linch_i / (linch_label + linch_n + linch_i)) * pos0x,
            y = y_n_label,
            labels =
              ifelse(
                num_periods > 1,
                paste0(
                  indNCasesTxt,
                  "*"
                ),
                indNCasesTxt
              ),
            pos = 2,
            cex = cexText,
            font = 2
          )
          text(
            x = (linch_i / (linch_label + linch_n + linch_i)) * pos0x,
            y = y_bp,
            labels = rev(tab_list[[num_periods]]$n),
            pos = 2,
            cex = cexText
          )
          text(
            x = -luserwidth,
            y = y_n_label,
            labels =
              ifelse(
                num_periods > 1,
                paste0(
                  indTitle,
                  "*"
                ),
                indTitle
              ),
            pos = 2,
            cex = cexText,
            font = 2
          )
          tempCex <- rep(cexText, length(tab_list[[num_periods]]$ind))
          tempCex[is.na(tab_list[[num_periods]]$ind)] <- 0.7 * cexText
          tab_list[[num_periods]]$ind <- round(tab_list[[num_periods]]$ind, 1)
          tab_list[[num_periods]]$ind[is.na(tab_list[[num_periods]]$ind)] <- groupHideLessThanLabel
          text(
            x = -luserwidth,
            y = y_bp,
            labels = rev(tab_list[[num_periods]]$ind),
            pos = 2,
            cex = rev(tempCex),
            col = rev(tab_list[[num_periods]]$col_text)
          )
        } else {
          text(
            x = (linch_n / (linch_label + linch_n)) * pos0x,
            y = y_bp,
            labels = rev(tab_list[[num_periods]]$group),
            pos = 2,
            cex = cexText,
            col = rev(tab_list[[num_periods]]$col_text)
          )
          text(
            x = -luserwidth,
            y = y_n_label,
            labels =
              ifelse(
                num_periods > 1,
                paste0(
                  indNCasesTxt,
                  "*"
                ),
                indNCasesTxt
              ),
            pos = 2,
            cex = cexText,
            font = 2
          )
          tempCex <- rep(cexText, length(tab_list[[num_periods]]$n))
          tempCex[is.na(tab_list[[num_periods]]$n)] <- 0.7 * cexText
          tab_list[[num_periods]]$n[tab_list[[num_periods]]$hide] <- groupHideLessThanLabel
          tab_list[[num_periods]]$n[tab_list[[num_periods]]$hideCellLessThan] <- "-"
          text(
            x = -luserwidth,
            y = y_bp,
            labels = rev(tab_list[[num_periods]]$n),
            pos = 2,
            cex = rev(tempCex),
            col = rev(tab_list[[num_periods]]$col_text)
          )

          if (indShowPct) {
            temp_pct <-
              paste(
                format(
                  round(
                    tab_list[[num_periods]]$ind,
                    digits = 0
                  ),
                  nsmall = 0
                ),
                "%"
              )
            temp_pct[tab_list[[num_periods]]$hide] <- ""
            text(
              x = xMax,
              y = y_bp,
              labels = rev(temp_pct),
              pos = 4,
              cex = cexText
            )
          }
        }

      }

    }

  }
