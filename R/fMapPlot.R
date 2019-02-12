#' Creates map plot
#' @description internal function used by server.R for plot in tab Map.
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#' @export
fMapPlot <- function(value = NULL, value_lim = NULL, value_order = c("Östergötland", "Blekinge", "Dalarna", "Gävleborg", "Gotland", "Halland", "Jämtland", "Jönköping",
    "Kalmar", "Kronoberg", "Norrbotten", "Orebro", "Södermanland", "Skåne", "Stockholm", "Uppsala", "Värmland", "Västerbotten", "Västernorrland", "Västmanland", "Västra Götaland"),
    value_order_return = FALSE, legend = "", col = NULL, col_border = "#7f7f7f", text_cex = 1, title = NULL, subtitle1 = NULL, subtitle2 = NULL, ndec = 1, rds_path = "./") {

    lightenCol <- function(col = "#000000", factor = 0.8, bg = "#ffffff") {

        # Check
        if (length(factor) > 1)
            col <- col[1]
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

        return(paste0("#", RR, GG, BB))

    }

    if (is.null(subtitle1) & !is.null(subtitle2)) {
        subtitle1 <- subtitle2
        subtitle2 <- NULL
    }

    if (value_order_return) {

        value_order[value_order == "Orebro"] <- "Örebro"
        return(value_order)

    } else {

        # Check
        if (length(value) != 21) {
            stop("Length of value must be 21")
        }

        #suppressMessages(require(sp))

        # Color
        if (is.null(col))
            col <- "#ffb117"

        temp_value <- value
        temp_value[is.na(value)] <- 0

        if (is.null(value_lim))
            value_lim <- range(pretty(c(min(0, min(value, na.rm = TRUE)), max(1, max(value, na.rm = TRUE)))), na.rm = TRUE)

        value_col <- lightenCol(col = col, factor = (temp_value - value_lim[1])/(value_lim[2] - value_lim[1]))
        value_col[is.na(value)] <- "#bfbfbf"

        # Load spatial data
        gadm <- readRDS(system.file("mapdata", "map_swe_adm1.rds", package = "rccShiny"))
        #gadm <- readRDS(paste0(rds_path, "map_swe_adm1.rds"))

        if (is.null(gadm) | !("NAME_1"%in%names(gadm)))
          stop(".rds data file was not loaded correctly")

        gadm <- gadm[gadm$NAME_1 %in% value_order, ]

        value_match <- match(gadm$NAME_1, value_order)
        value_names <- value_order[value_match]
        value <- value[value_match]

        value[value < value_lim[1]] <- value_lim[1]
        value[value > value_lim[2]] <- value_lim[2]

        # Change margins
        linchheight <- strheight("X", "inch", cex = text_cex)
        linchwidth <- strwidth("X", "inch", cex = text_cex)
        par(mai = c(0 * linchheight, 4 * linchheight, (4 * (!is.null(title)) + 2 * (!is.null(title) & !is.null(subtitle1)) + 2 * (!is.null(title) & !is.null(subtitle1) & !is.null(subtitle2))) *
            linchheight, 1 * linchheight), bg = "#ffffff", xpd = TRUE)

        # Plot
        plot(gadm, col = value_col, border = col_border, lwd = 1, main = "")

        luserheight <- strheight("X", "user", cex = text_cex)
        luserwidth <- strwidth("X", "user", cex = text_cex)

        pos0x <- grconvertX(x = 0, from = "nfc", to = "user")
        pos1x <- grconvertX(x = 1, from = "nfc", to = "user")
        pos0y <- grconvertY(y = 0, from = "nfc", to = "user")
        pos1y <- grconvertY(y = 1, from = "nfc", to = "user")

        y_lim <- par("usr")[3:4]

        # Fix Heby
        gadm <- readRDS(system.file("mapdata", "map_swe_adm2.rds", package = "rccShiny"))
        #gadm <- readRDS(paste0(rds_path, "map_swe_adm2.rds"))

        plot(gadm[gadm$NAME_2 %in% "Heby", ], col = value_col[value_names == "Uppsala"], border = value_col[value_names == "Uppsala"], add = TRUE)
        temp_y_border <- 59.864  #min(coordinates_heby[,2][coordinates_heby[,1]%in%coordinates_vastmanland[,1] & coordinates_heby[,2]%in%coordinates_vastmanland[,2]]) #59.864

        coordinates_heby <- gadm[gadm$NAME_2 %in% "Heby", ]@polygons[[1]]@Polygons[[1]]@coords
        lines(coordinates_heby[, 1], coordinates_heby[, 2], lwd = 2, col = value_col[value_names == "Uppsala"])
        match <- coordinates_heby[, 1] <= 16.85 & coordinates_heby[, 2] >= temp_y_border
        lines(coordinates_heby[match, 1], coordinates_heby[match, 2], lwd = 1, col = col_border)

        #gadm <- readRDS(paste0(rds_path, "map_swe_adm1.rds"))
        gadm <- readRDS(system.file("mapdata", "map_swe_adm1.rds", package = "rccShiny"))

        coordinates_vastmanland <- gadm[gadm$NAME_1 %in% "Västmanland", ]@polygons[[1]]@Polygons[[2]]@coords
        match <- coordinates_vastmanland[, 1] <= 16.85 & coordinates_vastmanland[, 1] >= 16.8 & coordinates_vastmanland[, 2] <= temp_y_border & coordinates_vastmanland[, 2] >=
            temp_y_border - 0.03
        lines(coordinates_vastmanland[match, 1], coordinates_vastmanland[match, 2], lwd = 1, col = col_border)

        sp::plot(gadm[gadm$NAME_1 %in% "Gävleborg", ], col = value_col[value_names == "Gävleborg"], border = col_border, add = TRUE)

        # Title
        if (!is.null(title)) {
            if (!is.null(subtitle1)) {
                if (!is.null(subtitle2)) {
                  text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]), labels = subtitle2, pos = 4, cex = text_cex, offset = 1)
                  text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]) + 1 * 1.4 * strheight("", "user", cex = text_cex), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                  text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]) + 2 * 1.8 * strheight("", "user", cex = text_cex), labels = title, pos = 4, cex = 1.5 * text_cex,
                    offset = 1)
                } else {
                  text(x = pos0x, y = y_lim[2] + 0.4 * (pos1y - y_lim[2]), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                  text(x = pos0x, y = y_lim[2] + 0.4 * (pos1y - y_lim[2]) + 1.8 * luserheight, labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
                }
            } else {
                text(x = pos0x, y = 0.5 * sum(y_lim[2], pos1y), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
            }
        }

        # Legend
        Xscale <- (par("usr")[2] - par("usr")[1]) * 0.05
        Xshift <- pos0x + 2 * luserwidth
        Yscale <- (par("usr")[4] - par("usr")[3]) * 0.005

        colFactors <- for (i in 1:100) {
            Yshift <- pos0y + 0.5 * (pos1y - pos0y) + i * 0.5 * Yscale  #par('usr')[4]-16*Yscale-i*Yscale/2
            polygon(c(0, 1, 1, 0) * Xscale + Xshift, c(-1, -1, 1, 1) * Yscale + Yshift, col = lightenCol(col = col, factor = i/100), border = NA)
        }
        Yshift <- pos0y + 0.5 * (pos1y - pos0y) + 50 * 0.5 * Yscale
        text(x = Xscale + Xshift + 2 * luserwidth, y = Yshift, legend, srt = 270)

        text(Xshift + 0.5 * Xscale, pos0y + 0.5 * (pos1y - pos0y), value_lim[1], cex = 0.8 * text_cex, font = 2, pos = 1)
        text(Xshift + 0.5 * Xscale, pos0y + 0.5 * (pos1y - pos0y) + 100 * 0.5 * Yscale, value_lim[2], cex = 0.8 * text_cex, font = 2, pos = 3)

        # Print numbers
        for (i in 1:length(value)) {
            if (!is.na(value[i])) {
                text(coordinates(gadm[gadm$NAME_1 == value_names[i], ]), labels = round(value[i], digits = ndec), font = 2, col = "#000000", cex = 0.7 * text_cex)
            }
        }

    }

}
