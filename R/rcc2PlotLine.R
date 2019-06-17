#' rcc2PlotLine
#' @description internal function.
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#' @export
rcc2PlotLine <-
  function(
    x = NULL,
    y = NULL,
    legend = NULL,
    legendPos = "bottom",
    legendPch = 15,
    legendNCol = NULL,
    legendTextWidth = NULL,
    col = NULL,
    stackedArea = FALSE,
    lineWidth = 4,
    lineType = "l",
    markers = TRUE,
    xLim = NULL,
    xBy = NULL,
    yLim = NULL,
    yBy = NULL,
    title = NULL,
    subtitle1 = NULL,
    subtitle2 = NULL,
    xLab = "x",
    yLab = "y",
    cexText = 1,
    targetValues = NULL,
    targetValuesHigh = NULL,
    targetValuesLabels = c("Mellannivå av måluppfyllelse", "Hög nivå av måluppfyllelse"),
    outputHighchart = FALSE
  ) {

    suppressMessages(require(highcharter))

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

    if (is.null(subtitle1) & !is.null(subtitle2)) {
      subtitle1 <- subtitle2
      subtitle2 <- NULL
    }

    if (is.null(targetValuesHigh)) targetValuesHigh <- TRUE

    # Check if x and y vector
    if (is.list(x)) {
      x <- x[[1]]
    }
    if (!is.list(y)) {
      y <- list(y)
    }

    if (!all(lengths(y) %in% length(x))) {
      stop("Lengths of all elements in list of y-values must match length of vector x")
    }

    if (stackedArea) {
      if (any(is.na(unlist(y)))) {
        stop("Missing (NA) values of y are not allowed when stackedArea = TRUE")
      }
      stackedAreaTable <-
        apply(
          data.frame(
            matrix(
              unlist(y),
              nrow = length(y[[1]]),
              byrow = FALSE
            )
          ),
          1,
          cumsum
        )
    }

    # Colors
    if (is.null(col)) {
      col <- c(
        "#00b3f6","#ffb117","#005092","#19975d","#e56284","#66cccc","#db5524","#7f3705","#7c458a","#95bf5d",
        "#7f7f7f","#8c8c8c","#999999","#a6a6a6","#b2b2b2","#bfbfbf","#cccccc","#d9d9d9","#e5e5e5","#f2f2f2"
      )
    }
    col_target_1 <- rcc2LightenCol("#ffb117", factor = 0.4)
    col_target_2 <- rcc2LightenCol("#19975d", factor = 0.4)

    # Line types
    lt <- rep(1, length(y))

    # Line width
    lw <- rep(lineWidth, length(y))

    # x- and y-axis labels and ticks
    if (is.numeric(x)) {
      xNum <- x
      if (is.null(xLim)) {
        xLim <- range(pretty(x))
      }
      if (is.null(xBy)) {
        x_ticks <- pretty(xLim)
      } else {
        x_ticks <- seq(xLim[1], xLim[2], xBy)
      }
      xTicksLabels <- x_ticks
    } else {
      xNum <- 1:length(x)
      xLim <- range(xNum)
      x_ticks <- xNum
      xTicksLabels <- x
    }

    if (is.null(yLim)) {
      if (stackedArea) {
        yLim <- range(pretty(c(stackedAreaTable)))
      } else {
        yLim <- range(pretty(unlist(y)))
      }
    }
    if (is.null(yBy)) {
      y_ticks <- pretty(yLim)
    } else {
      y_ticks <- seq(yLim[1], yLim[2], yBy)
    }

    # Output Highchart
    if (outputHighchart) {

      tempPlot <-
        highchart() %>%
        hc_boost(
          enabled = FALSE
        )

      if (is.numeric(x)) {
        tempPlot <- tempPlot %>%
          hc_xAxis(
            min = xLim[1],
            max = xLim[2],
            tickInterval = x_ticks[2] - x_ticks[1]
          )
      } else {
        tempPlot <- tempPlot %>%
          hc_xAxis(
            type = "category"
          )
      }
      tempPlot <- tempPlot %>%
        hc_xAxis(
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
        hc_yAxis(
          reversedStacks = FALSE,
          min = yLim[1],
          max = yLim[2],
          tickInterval = y_ticks[2] - y_ticks[1],
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
            text = yLab,
            style = list(
              color = "#000000",
              fontWeight = "bold",
              fontSize = paste0(round(12 * cexText), "px")
            )
          )
        ) %>%
        hc_legend(
          enabled = length(y) > 1,
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
                    yLim[1]
                  ),
                  to = ifelse(
                    targetValuesHigh,
                    yLim[2],
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
                    yLim[1]
                  ),
                  to = ifelse(
                    targetValuesHigh,
                    yLim[2],
                    min(targetValues)
                  )
                )
              )
            )
        }

      }

      tempPlot <- tempPlot %>%
        hc_chart(
          spacing = c(20, 20, 20, 20)
        ) %>%
        hc_plotOptions(
          line = list(
            lineWidth = lineWidth,
            marker = list(
              enabled = markers,
              symbol = "circle",
              radius = lineWidth * 1.5
            ),
            events = list(
              legendItemClick = "function() {return false;}"
            )
          ),
          area = list(
            stacking = "normal",
            lineWidth = lineWidth,
            marker = list(
              enabled = markers,
              symbol = "circle",
              radius = lineWidth * 1.5
            ),
            events = list(
              legendItemClick = "function() {return false;}"
            )
          )
        ) %>%
        hc_tooltip(
          shared = TRUE,
          headerFormat = "<span style='font-size: 10px'>{point.key}</span><br>",
          pointFormat = paste0("<span style='color:{point.color}'>\u25CF</span> <span style='font-size: 10px'>", ifelse(length(y) > 1, "{series.name}: ", ""), "<b>{point.y}</b></span><br>"),
          useHTML = TRUE,
          outside = TRUE
        )

      for (i in 1:length(y)) {
        tempData <-
          data.frame(
            x = x,
            y = as.numeric(y[[i]]),
            color = col[i],
            stringsAsFactors = FALSE
          )
        tempPlot <- tempPlot %>%
          hc_add_series(data = tempData, type = ifelse(stackedArea, "area", "line"), mapping = hcaes(x = x, y = y, color = color), name = ifelse(!is.null(legend), legend[i], i), showInLegend = !is.null(legend), color = col[i])
      }

      tempPlot

    } else {

      # Change margins
      legendNCol <-
        ifelse(
          !is.null(legend),
          ifelse(
            is.null(legendNCol),
            ifelse(
              length(legend) <= 3,
              length(legend),
              ifelse(
                length(legend) == 4,
                2,
                3
              )
            ),
            legendNCol
          ),
          0
        )
      legend_nrow <-
        ifelse(
          !is.null(legend),
          ceiling(length(legend) / legendNCol),
          0
        )

      linchheight <- strheight("X", "inch", cex = cexText)
      linchwidth <- strwidth("X", "inch", cex = cexText)
      par(
        mai = c(
          ifelse(!is.null(xLab), 6, 4) * linchheight +
            ifelse(legendPos == "bottom" & !is.null(legend), legend_nrow * linchheight, 0) +
            (!is.null(targetValues)) * 2 * linchheight,
          6 * linchheight,
          (2 +
             2.5 * (!is.null(title)) +
             2.5 * (!is.null(subtitle1)) +
             1.5 * (!is.null(subtitle2))
          ) * linchheight,
          2 * linchheight +
            ifelse(legendPos=="right" & !is.null(legend), 1 * linchwidth + max(strwidth(legend, "inch", cex = cexText)), 0)
        ),
        bg = "#ffffff",
        xpd = TRUE
      )

      # Empty plot
      plot(
        x = xLim,
        y = yLim,
        axes = FALSE,
        type = "n",
        xlab = "",
        ylab =""
      )

      # Target values
      if (!is.null(targetValues)) {
        if (length(targetValues) > 1) {
          rect(
            xleft = xLim[1],
            ybottom = min(targetValues),
            xright = xLim[2],
            ytop = max(targetValues),
            col = col_target_1,
            border = NA
          )
        }
        rect(
          xleft = xLim[1],
          ybottom =
            ifelse(
              targetValuesHigh,
              max(targetValues),
              yLim[1]
            ),
          xright = xLim[2],
          ytop =
            ifelse(
              targetValuesHigh,
              yLim[2],
              min(targetValues)
            ),
          col = col_target_2,
          border = NA
        )
      }

      # Grid
      for (i in y_ticks) {
        lines(
          x = xLim,
          y = rep(i,2),
          lwd = 1,
          col = "#bdbdbd"
        )
      }

      # Axes
      luserheight <- strheight("X", "user", cex = cexText)
      luserwidth <- strwidth("X", "user", cex = cexText)

      pos0x <- grconvertX(x = 0, from="nfc", to="user")
      pos1x <- grconvertX(x = 1, from="nfc", to="user")
      pos0y <- grconvertY(y = 0, from="nfc", to="user")
      pos1y <- grconvertY(y = 1, from="nfc", to="user")

      axis(
        side = 1,
        pos = yLim[1],
        at = x_ticks,
        labels = xTicksLabels,
        cex.axis = cexText,
        las = 1,
        lwd = 3,
        col = "#d9d9d9"
      )
      axis(
        side = 2,
        pos = xLim[1],
        at = y_ticks,
        cex.axis = cexText,
        las = 1,
        lwd = 3,
        col="#d9d9d9"
      )

      # Axis labels
      y_xlab_zeropos <-
        ifelse(
          legendPos == "bottom" & !is.null(legend),
          pos0y + legend_nrow * luserheight,
          pos0y
        ) + (!is.null(targetValues)) * 2 * luserheight
      text(
        x = 0.5 * sum(xLim),
        y = yLim[1] - 0.6 * (yLim[1] - y_xlab_zeropos),
        labels = xLab,
        cex = cexText,
        font = 2
      )
      text(
        x = xLim[1] - 0.7 * (xLim[1] - pos0x),
        y = 0.5 * sum(yLim),
        labels = yLab,
        cex = cexText,
        font = 2,
        srt = 90
      )

      # Title
      printTitle <- !is.null(title)
      printSubtitle1 <- !is.null(subtitle1)
      printSubtitle2 <- !is.null(subtitle2)
      text(
        x = pos0x,
        y = yLim[2] +
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
        y = yLim[2] +
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
        y = yLim[2] +
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

      # Plot
      for (i in 1:length(y)) {
        in_x_range <- xNum >= xLim[1] & xNum <= xLim[2]

        if (stackedArea) {
          temp_x <- xNum[in_x_range]
          temp_y1 <- stackedAreaTable[i, in_x_range]
          if (i == 1) {
            temp_y0 <- rep(0, length(temp_x))
          } else {
            temp_y0 <- stackedAreaTable[i - 1, in_x_range]
          }
          polygon(
            c(temp_x, rev(temp_x)),
            c(temp_y1, rev(temp_y0)),
            col = col[i],
            border = "#7f7f7f"
          )
        } else {
          lines(
            xNum[in_x_range],
            y[[i]][in_x_range],
            type = lineType,
            lwd = lw,
            lty = lt,
            col = col[i]
          )

          if (markers) {
            points(
              xNum[in_x_range],
              y[[i]][in_x_range],
              pch = 16,
              cex = 2,
              col = col[i]
            )
          }
        }
      }

      if (!is.null(legend)) {
        if (legendPos == "bottom") {
          legend(
            x = xLim[1] + 0.5*(xLim[2] - xLim[1]),
            y = pos0y +
              (!is.null(targetValues)) * 2 * luserheight,
            legend = legend,
            col = col,
            pch = legendPch,
            pt.cex = 1.75 * cexText,
            bty="n",
            cex = 0.8 * cexText,
            xjust = 0.5,
            yjust = 0,
            y.intersp = 1,
            ncol = legendNCol,
            text.width = ifelse(
              !is.null(legendTextWidth),
              strwidth(
                paste(
                  rep("X", legendTextWidth),
                  collapse = ""
                )
              ),
              cexText * max(strwidth(legend))
            )
          )
        } else if (legendPos=="right") {
          legend(
            x = xLim[2] + 0.1 * (xLim[1] - pos0x),
            y = 0.5 * sum(yLim),
            legend = legend,
            col = col,
            pch = legendPch,
            pt.cex = 1.75 * cexText,
            bty = "n",
            cex = 0.8 * cexText,
            yjust = 0.5,
            y.intersp = 2
          )
        }
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
          x = xLim[1] + 0.5*(xLim[2] - xLim[1]),
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

    }

  }
