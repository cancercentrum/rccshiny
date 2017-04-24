fLinePlot <- function(x = NULL, y = NULL, legend = NULL, legend_pos = "bottom", legend_pch = 15, legend_ncol = NULL, legend_textwidth = NULL, col = NULL, stacked_area = FALSE,
    linewidth = 4, linetype = "l", markers = TRUE, x_lim = NULL, y_lim = NULL, x_by = NULL, y_by = NULL, x_ticks_labels = NULL, title = NULL, subtitle1 = NULL, subtitle2 = NULL,
    x_lab = "x", y_lab = "y", text_cex = 1, target_values = NULL, target_values_high = NULL) {

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

    if (is.null(target_values_high))
        target_values_high <- TRUE

    # Check if x and y vector
    if (!is.list(x) & !is.list(y)) {
        x <- list(x)
        y <- list(y)
    } else if (!is.list(x)) {
        x <- list(x)
        y <- y[[1]]
    } else if (!is.list(y)) {
        y <- list(y)
        x <- y[[1]]
    }

    if (stacked_area) {
        if (min(lengths(x)) != max(lengths(x))) {
            stop(paste0("Only equal lengths of all elements in list of x-values are allowed if stacked_area=TRUE"))
        }
        if (any(is.na(unlist(y)))) {
            stop(paste0("Missing (NA) values of y are not allowed when stacked_area=TRUE"))
        }
        stackedarea_table <- apply(data.frame(matrix(unlist(y), nrow = length(y[[1]]), byrow = FALSE)), 1, cumsum)
    }

    # Colors
    if (is.null(col)) {
        col <- c("#438DCC", "#FFCC33", "#EC6698", "#003366", "#339966", "#66CCCC", "#FF9933", "#8A4F71", "#9999CC", "#E94949", "#7f7f7f", "#8c8c8c", "#999999", "#a6a6a6", "#b2b2b2",
            "#bfbfbf", "#cccccc", "#d9d9d9", "#e5e5e5", "#f2f2f2")
    }
    col_target_1 <- lightenCol("#FFCC33", factor = 0.4)
    col_target_2 <- lightenCol("#339966", factor = 0.4)

    # Line types
    lt <- rep(1, length(x))

    # Line width
    lw <- rep(linewidth, length(x))

    # x- and y-axis labels and ticks
    if (is.null(x_lim)) {
        x_lim <- range(pretty(unlist(x)))
    }
    if (is.null(y_lim)) {
        if (stacked_area) {
            y_lim <- range(pretty(c(stackedarea_table)))
        } else {
            y_lim <- range(pretty(unlist(y)))
        }
    }

    if (!is.null(x_ticks_labels)) {
        x_ticks <- seq(x_lim[1], x_lim[2], 1)
    } else {
        if (is.null(x_by)) {
            x_ticks <- pretty(x_lim)
        } else {
            x_ticks <- seq(x_lim[1], x_lim[2], x_by)
        }
        x_ticks_labels <- x_ticks
    }

    if (is.null(y_by)) {
        y_ticks <- pretty(y_lim)
    } else {
        y_ticks <- seq(y_lim[1], y_lim[2], y_by)
    }

    # Change margins
    legend_ncol <- ifelse(!is.null(legend), ifelse(is.null(legend_ncol), ifelse(length(legend) <= 3, length(legend), ifelse(length(legend) == 4, 2, 3)), legend_ncol), 0)
    legend_nrow <- ifelse(!is.null(legend), ceiling(length(legend)/legend_ncol), 0)

    linchheight <- strheight("X", "inch", cex = text_cex)
    linchwidth <- strwidth("X", "inch", cex = text_cex)
    par(mai = c(ifelse(!is.null(x_lab), 6, 4) * linchheight + ifelse(legend_pos == "bottom" & !is.null(legend), legend_nrow * linchheight, 0), 6 * linchheight, (2 + 2 * (!is.null(title)) +
        2 * (!is.null(title) & !is.null(subtitle1)) + 2 * (!is.null(title) & !is.null(subtitle1) & !is.null(subtitle2))) * linchheight, 2 * linchheight + ifelse(legend_pos ==
        "right" & !is.null(legend), 1 * linchwidth + max(strwidth(legend, "inch", cex = text_cex)), 0)), bg = "#ffffff", xpd = TRUE)

    # Empty plot
    plot(x = x_lim, y = y_lim, axes = FALSE, type = "n", xlab = "", ylab = "")

    # Target values (area)
    if (!is.null(target_values) & length(target_values) > 1) {
        rect(xleft = x_lim[1], ybottom = min(target_values), xright = x_lim[2], ytop = max(target_values), col = col_target_1, border = NA)
        rect(xleft = x_lim[1], ybottom = ifelse(target_values_high, max(target_values), y_lim[1]), xright = x_lim[2], ytop = ifelse(target_values_high, y_lim[2], min(target_values)),
            col = col_target_2, border = NA)
    }

    # Grid
    for (i in y_ticks) {
        lines(x = x_lim, y = rep(i, 2), lwd = 1, col = "#bdbdbd")
    }

    # Target values (line)
    if (!is.null(target_values) & length(target_values) == 1) {
        lines(x = x_lim, y = rep(target_values, 2), col = col_target_2, lwd = 3)
    }

    # Axes
    luserheight <- strheight("X", "user", cex = text_cex)
    luserwidth <- strwidth("X", "user", cex = text_cex)

    pos0x <- grconvertX(x = 0, from = "nfc", to = "user")
    pos1x <- grconvertX(x = 1, from = "nfc", to = "user")
    pos0y <- grconvertY(y = 0, from = "nfc", to = "user")
    pos1y <- grconvertY(y = 1, from = "nfc", to = "user")

    axis(side = 1, pos = y_lim[1], at = x_ticks, labels = x_ticks_labels, cex.axis = text_cex, las = 1, lwd = 3, col = "#d9d9d9")
    axis(side = 2, pos = x_lim[1], at = y_ticks, cex.axis = text_cex, las = 1, lwd = 3, col = "#d9d9d9")

    # Axis labels
    y_xlab_zeropos <- ifelse(legend_pos == "bottom" & !is.null(legend), pos0y + legend_nrow * luserheight, pos0y)
    text(x = 0.5 * sum(x_lim), y = y_lim[1] - 0.6 * (y_lim[1] - y_xlab_zeropos), labels = x_lab, cex = text_cex, font = 2)
    text(x = x_lim[1] - 0.7 * (x_lim[1] - pos0x), y = 0.5 * sum(y_lim), labels = y_lab, cex = text_cex, font = 2, srt = 90)

    # Title
    if (!is.null(title)) {
        if (!is.null(subtitle1)) {
            if (!is.null(subtitle2)) {
                text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]), labels = subtitle2, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]) + 1 * 1.4 * strheight("", "user", cex = text_cex), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_lim[2] + 0.3 * (pos1y - y_lim[2]) + 2 * 1.8 * strheight("", "user", cex = text_cex), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
            } else {
                text(x = pos0x, y = y_lim[2] + 0.4 * (pos1y - y_lim[2]), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_lim[2] + 0.4 * (pos1y - y_lim[2]) + 1.8 * strheight("", "user", cex = text_cex), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
            }
        } else {
            text(x = pos0x, y = 0.5 * sum(y_lim[2], pos1y), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
        }
    }

    # Plot
    for (i in 1:length(x)) {
        in_x_range <- x[[i]] >= x_lim[1] & x[[i]] <= x_lim[2]

        if (stacked_area) {
            temp_x <- x[[i]][in_x_range]
            temp_y1 <- stackedarea_table[i, in_x_range]
            if (i == 1) {
                temp_y0 <- rep(0, length(temp_x))
            } else {
                temp_y0 <- stackedarea_table[i - 1, in_x_range]
            }
            polygon(c(temp_x, rev(temp_x)), c(temp_y1, rev(temp_y0)), col = col[i], border = "#7f7f7f")
        } else {
            lines(x[[i]][in_x_range], y[[i]][in_x_range], type = linetype, lwd = lw, lty = lt, col = col[i])

            if (markers) {
                points(x[[i]][in_x_range], y[[i]][in_x_range], pch = 16, cex = 2, col = col[i])
            }
        }
    }

    if (!is.null(legend)) {
        if (legend_pos == "bottom") {
            legend(x = x_lim[1] + 0.5 * (x_lim[2] - x_lim[1]), y = pos0y, legend = legend, col = col, pch = legend_pch, pt.cex = 1.75, bty = "n", cex = 0.8 * text_cex, xjust = 0.5,
                yjust = 0, y.intersp = 1, ncol = legend_ncol, text.width = ifelse(!is.null(legend_textwidth), strwidth(paste(rep("X", legend_textwidth), collapse = "")), max(strwidth(legend))))
        } else if (legend_pos == "right") {
            legend(x = x_lim[2] + 0.1 * (x_lim[1] - pos0x), y = 0.5 * sum(y_lim), legend = legend, col = col, pch = legend_pch, pt.cex = 1.75, bty = "n", cex = 0.8 * text_cex,
                yjust = 0.5, y.intersp = 2)
        }
    }

}
