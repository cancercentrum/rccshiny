#' rcc2PlotMap
#' @description internal function.
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#' @keywords internal
#' @export
rcc2PlotMap <-
  function(
    value = NULL,
    valueLim = NULL,
    valueOrder = c(
      "Östergötland",
      "Blekinge",
      "Dalarna",
      "Gävleborg",
      "Gotland",
      "Halland",
      "Jämtland",
      "Jönköping",
      "Kalmar",
      "Kronoberg",
      "Norrbotten",
      "Orebro",
      "Södermanland",
      "Skåne",
      "Stockholm",
      "Uppsala",
      "Värmland",
      "Västerbotten",
      "Västernorrland",
      "Västmanland",
      "Västra Götaland"
    ),
    valueOrderReturn = FALSE,
    legend = "",
    col = NULL,
    colBorder = "#FFFFFF",
    cexText = 1,
    title = NULL,
    subtitle1 = NULL,
    subtitle2 = NULL,
    nDec = 0,
    rdsPath = "./data/",
    outputHighchart = FALSE
  ) {

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

    if (valueOrderReturn) {

      valueOrder[valueOrder == "Orebro"] <- "Örebro"
      return(valueOrder)

    } else {

      # Check
      if (length(value) != 21) {
        stop("Length of value must be 21")
      }

      # Color
      if (is.null(col)) col <- "#db5524"

      temp_value <- value
      temp_value[is.na(value)] <- 0

      if (is.null(valueLim)) {
        valueLim <-
          range(
            pretty(
              c(
                min(
                  0,
                  min(value, na.rm = TRUE)
                ),
                max(
                  1,
                  max(value, na.rm=TRUE)
                )
              )
            ),
            na.rm = TRUE
          )
      }

      value_col <-
        rcc2LightenCol(
          col = col,
          factor = (temp_value - valueLim[1]) / (valueLim[2] - valueLim[1])
        )
      value_col[is.na(value)] <- "#bfbfbf"

      if (outputHighchart) {

        tempKey <-
          data.frame(
            key = c(
              "Östergötland",
              "Blekinge",
              "Dalarna",
              "Gävleborg",
              "Gotland",
              "Halland",
              "Jämtland",
              "Jönköping",
              "Kalmar",
              "Kronoberg",
              "Norrbotten",
              "Orebro",
              "Södermanland",
              "Skåne",
              "Stockholm",
              "Uppsala",
              "Värmland",
              "Västerbotten",
              "Västernorrland",
              "Västmanland",
              "Västra Götaland"
            ),
            keyCode = c(
              "se-og",
              "se-bl",
              "se-ko",
              "se-gv",
              "se-gt",
              "se-ha",
              "se-ja",
              "se-jo",
              "se-ka",
              "se-kr",
              "se-nb",
              "se-or",
              "se-sd",
              "se-sn",
              "se-st",
              "se-up",
              "se-vr",
              "se-vb",
              "se-vn",
              "se-vm",
              "se-vg"
            )
          )

        tempData <-
          merge(
            data.frame(
              key = valueOrder,
              value = value,
              stringsAsFactors = FALSE
            ),
            tempKey,
            by = "key",
            all = TRUE
          )

        tempPlot <-
          hcmap(
            map = "countries/se/se-all",
            data = tempData,
            value = "value",
            joinBy = c("hc-key", "keyCode")
          ) %>%
          hc_credits(
            enabled = FALSE
          ) %>%
          hc_plotOptions(
            map = list(
              dataLabels = list(
                enabled = TRUE,
                style = list(
                  fontSize = paste0(round(10 * cexText), "px")
                )

              )
            )
          ) %>%
          hc_tooltip(
            headerFormat = "",
            pointFormat = paste0("<span style='color:{point.color}'>\u25A0</span> <span style='font-size: 10px'>{point.key}: <b>{point.value}</b></span><br>"),
            useHTML = TRUE,
            outside = TRUE
          ) %>%
          hc_colorAxis(
            min = valueLim[1],
            max = valueLim[2],
            minColor = "#ffffff",
            maxColor = col,
            reversed = FALSE
          ) %>%
          hc_legend(
            enabled = TRUE,
            align = "left",
            verticalAlign = "middle",
            layout = "vertical",
            title = list(
              text = legend,
              style = list(
                fontWeight = "normal"
              )
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

        tempPlot

      } else {

        # Change margins
        linchheight <- strheight("X", "inch", cex = cexText)
        linchwidth <- strwidth("X", "inch", cex = cexText)
        par(
          mai = c(
            0 * linchheight,
            4 * linchheight,
            (4 +
               2.5 * (!is.null(title)) +
               2.5 * (!is.null(subtitle1)) +
               1.5 * (!is.null(subtitle2))
            ) * linchheight,
            1 * linchheight
          ),
          bg = "#ffffff",
          xpd = TRUE
        )

        # Load spatial data
        gadm1 <- readRDS(system.file("mapdata", "map_swe_adm1.rds", package = "rccShiny"))
        #gadm1 <- readRDS(paste0(rdsPath, "map_swe_adm1.rds"))
        gadm2 <- readRDS(system.file("mapdata", "map_swe_adm2.rds", package = "rccShiny"))
        #gadm2 <- readRDS(paste0(rdsPath, "map_swe_adm2.rds"))

        gadm1 <- gadm1[gadm1$NAME_1 %in% valueOrder,]

        value_match <- match(gadm1$NAME_1, valueOrder)
        value_names <- valueOrder[value_match]
        value <- value[value_match]

        value[value < valueLim[1]] <- valueLim[1]
        value[value > valueLim[2]] <- valueLim[2]

        # Plot
        plot(
          gadm1,
          col = value_col,
          border = colBorder,
          lwd = 1,
          main = ""
        )

        luserheight <- strheight("X", "user", cex = cexText)
        luserwidth <- strwidth("X", "user", cex = cexText)

        pos0x <- grconvertX(x = 0, from = "nfc", to = "user")
        pos1x <- grconvertX(x = 1, from = "nfc", to = "user")
        pos0y <- grconvertY(y = 0, from = "nfc", to = "user")
        pos1y <- grconvertY(y = 1, from = "nfc", to = "user")

        y_lim <- par("usr")[3:4]

        # Fix Heby
        plot(
          gadm2[gadm2$NAME_2 %in% "Heby",],
          col = value_col[value_names == "Uppsala"],
          border = colBorder,
          add = TRUE
        )

        coordinates_uppsala <- gadm1[gadm1$NAME_1 %in% "Uppsala",]@polygons[[1]]@Polygons[[64]]@coords
        coordinates_heby <- gadm2[gadm2$NAME_2 %in% "Heby",]@polygons[[1]]@Polygons[[1]]@coords

        match <-
          coordinates_heby[, 1] %in% coordinates_uppsala[, 1] &
          coordinates_heby[, 2] %in% coordinates_uppsala[, 2]
        lines(
          x = coordinates_heby[match, 1],
          y = coordinates_heby[match, 2],
          lwd = 1,
          col = value_col[value_names == "Uppsala"]
        )

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

        # Legend
        Xscale <- 3 * luserwidth
        Xshift <- pos0x + 5 * luserwidth
        Yscale <- (par("usr")[4] - par("usr")[3]) * 0.005

        colFactors <-
          for (i in 1:100) {
            Yshift <-
              pos0y +
              0.5 * (pos1y - pos0y) +
              i * 0.5 * Yscale
            polygon(
              x = c(0, 1, 1, 0) * Xscale + Xshift,
              y = c(-1, -1, 1, 1) * Yscale + Yshift,
              col = rcc2LightenCol(col = col, factor = i / 100),
              border = NA
            )
          }
        Yshift <-
          pos0y +
          0.5 * (pos1y - pos0y) +
          50 * 0.5 * Yscale
        text(
          x = Xshift - 0.5 * Xscale,
          y = Yshift,
          labels = legend,
          srt = 90
        )

        text(
          x = Xshift + 0.5 * Xscale,
          y = pos0y + 0.5 * (pos1y - pos0y),
          labels = valueLim[1],
          cex = 0.8 * cexText,
          font = 2,
          pos = 1
        )
        text(
          x = Xshift + 0.5 * Xscale,
          y = pos0y + 0.5 * (pos1y - pos0y) + 100 * 0.5 * Yscale,
          labels = valueLim[2],
          cex = 0.8 * cexText,
          font = 2,
          pos = 3
        )

        # Print numbers
        for (i in 1:length(value)) {
          if (!is.na(value[i])) {
            text(
              coordinates(gadm1[gadm1$NAME_1 == value_names[i],]),
              labels = format(round(value[i], digits = nDec), nsmall = nDec),
              font = 2,
              col = "#000000",
              cex = 0.7 * cexText
            )
          }
        }

      }

    }

  }
