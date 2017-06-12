#' @export
fIndPlot <- function(group = NULL, group_hide_less_than = FALSE, group_maxchars = NULL, ind = NULL, period = NULL, max_periods = 99, ind_type = class(ind), ind_numeric = ind_type %in%
    c("difftime", "numeric", "integer"), ind_numeric_exclude_neg = TRUE, ind_numeric_percentiles = c(0.25, 0.5, 0.75), ind_factor_hide = NULL, ind_factor_sortbycols = NULL,
    ind_factor_shownN = ifelse(!is.null(ind_factor_hide), TRUE, FALSE), legend_ncol = NULL, legend_fixedtextwidth = TRUE, ind_showpct = ifelse(ind_type == "factor", FALSE,
        TRUE), ind_title = ifelse(ind_numeric, "Median", "Procent"), ind_noofcasestxt = "Antal fall", col = NULL, border = TRUE, x_max = if (ind_numeric) {
        NULL
    } else {
        100
    }, x_by = NULL, title = NULL, subtitle1 = NULL, subtitle2 = NULL, x_lab = ifelse(ind_numeric, "Median samt kvartilavstånd", "Procent"), all_lab = "RIKET", emph_lab = NULL,
    text_cex = 1, point_cex = 2.25, target_values = NULL, target_values_high = NULL, funnelplot = FALSE, funnelplot_probs = c(0.05, 0.01), sort = TRUE, subset = NULL, subset_lab = "SUBSET") {

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

    if (is.null(subset)) {
        subset <- rep(TRUE, length(group))
    }

    if (is.null(subtitle1) & !is.null(subtitle2)) {
        subtitle1 <- subtitle2
        subtitle2 <- NULL
    }

    if (is.null(target_values_high))
        target_values_high <- ifelse(ind_numeric, FALSE, TRUE)

    if (!(ind_type %in% c("difftime", "numeric", "integer", "logical", "factor"))) {
        stop(paste0("Variable of class ", class(ind), " is not supported."))
    }

    if (funnelplot) {
        if (ind_type != "logical") {
            funnelplot <- FALSE
            warning("funnelplot = TRUE is only allowed for ind of type logical => funnelplot set to FALSE in analysis")
        } else {
            if (!is.null(period)) {
                period <- NULL
                warning("funnelplot = TRUE is not allowed together with period => period set to NULL in analysis")
            }
            if (!is.null(target_values)) {
                target_values <- NULL
                warning("funnelplot = TRUE is not allowed together with target_values => target_values set to NULL in analysis")
            }
        }
    }

    if (funnelplot) {
        funnelplot_probs <- sort(funnelplot_probs)
        sort <- TRUE
        target_values_high <- TRUE
    }

    #suppressMessages(require(gplots))
    #suppressMessages(require(plyr))
    #suppressMessages(require(Hmisc))

    if (is.null(period)) {
        period <- rep(1, length(group))
    }
    show_periods <- tail(sort(unique(period)), max_periods)
    num_periods <- length(show_periods)
    act_period <- tail(show_periods, 1)

    # Handle missing values
    include <- !is.na(ind) & !is.na(period)
    if (ind_numeric) {
        ind <- as.numeric(ind)
        if (ind_numeric_exclude_neg)
            include <- include & ind >= 0
    }

    group <- as.character(group)
    group[is.na(group)] <- "(NA)"

    group <- group[include]
    ind <- ind[include]
    period <- period[include]
    subset <- subset[include]

    tabdata <- data.frame(group, ind, period, subset, stringsAsFactors = FALSE)
    tabdata <- subset(tabdata, period %in% show_periods)
    byvars <- c("group", "period")

    if (ind_type == "factor") {
        factor_legend <- levels(ind)[!(levels(ind) %in% ind_factor_hide)]
    } else {
        factor_legend <- NULL
    }
    if (is.null(legend_ncol)) {
        legend_ncol <- ifelse(ind_type == "factor", ifelse(length(factor_legend) <= 3, length(factor_legend), ifelse(length(factor_legend) == 4, 2, 3)), 0)
    }
    legend_nrow <- ifelse(ind_type == "factor", ceiling(length(factor_legend)/legend_ncol), 0)

    # Tabulate
    summaryFunction <- function(x) {
        if (ind_numeric) {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            if (hide) {
                measurements <- c(NA, NA, NA, NA)
            } else {
                measurements <- quantile(as.numeric(x$ind), probs = ind_numeric_percentiles, na.rm = TRUE)
                measurements <- c(measurements, sum(!is.na(x$ind), na.rm = TRUE))
            }
            measurements <- c(measurements, hide)
            names(measurements) <- c("lower", "ind", "upper", "n", "hide")
        } else if (ind_type == "logical") {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            if (hide) {
                measurements <- c(NA, NA, NA, NA, NA)
            } else {
                measurements <- 100 * Hmisc::binconf(sum(x$ind, na.rm = TRUE), sum(!is.na(x$ind), na.rm = TRUE), method = "exact")
                measurements <- c(measurements, paste0(sum(x$ind, na.rm = TRUE), " av ", sum(!is.na(x$ind), na.rm = TRUE)))
                measurements <- c(measurements, sum(!is.na(x$ind)))
            }
            measurements <- c(measurements, hide)
            names(measurements) <- c("ind", "lower", "upper", "n", "N", "hide")
        } else if (ind_type == "factor") {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            measurements <- vector()
            if (hide) {
                for (i in factor_legend) {
                  measurements <- c(measurements, NA)
                }
                measurements <- c(measurements, NA, NA)
            } else {
                for (i in factor_legend) {
                  measurements <- c(measurements, 100 * (sum(x$ind == i)/sum(!is.na(x$ind))))
                }
                if (ind_factor_shownN) {
                  measurements <- c(measurements, sum(measurements), if (!is.null(ind_factor_hide)) {
                    paste0(sum(x$ind %in% factor_legend), " av ", sum(!is.na(x$ind), na.rm = TRUE))
                  } else {
                    sum(!is.na(x$ind), na.rm = TRUE)
                  })
                } else {
                  measurements <- c(measurements, sum(measurements), sum(!is.na(x$ind), na.rm = TRUE))
                }
            }
            measurements <- c(measurements, hide)
            names(measurements) <- c(paste0("factor", 1:(length(measurements) - 3)), "ind", "n", "hide")
        }
        return(measurements)
    }

    hideLowVolume <- as.logical(group_hide_less_than)

    tab <- plyr::ddply(.data = subset(tabdata, subset), .variables = byvars, .fun = summaryFunction, .drop = FALSE)

    include_groups <- sort(unique(subset(tabdata, period == act_period)$group))
    tab <- subset(tab, group %in% include_groups)

    subsetUniqueGroups <- unique(tabdata$group[tabdata$subset & tabdata$period == act_period])
    if (!all(tabdata$subset) & !(length(subsetUniqueGroups) == 1 & all(subsetUniqueGroups %in% subset_lab))) {
        tab_subset <- plyr::ddply(.data = subset(tabdata, subset), .variables = byvars[byvars != "group"], .fun = summaryFunction, .drop = FALSE)
        tab_subset <- tab_subset[intersect(names(tab_subset), names(tab))]
        tab_subset$group <- subset_lab

        tab <- rbind(tab, tab_subset[, names(tab)])
    }

    if (!is.null(all_lab)) {
        # hideLowVolume <- FALSE

        tab_all <- plyr::ddply(.data = tabdata, .variables = byvars[byvars != "group"], .fun = summaryFunction, .drop = FALSE)
        tab_all <- tab_all[intersect(names(tab_all), names(tab))]
        tab_all$group <- all_lab

        tab <- rbind(tab, tab_all[, names(tab)])
    }

    tab$hide <- as.logical(tab$hide)

    # Funnelplot
    if (funnelplot) {
        temp_ind_all <- as.numeric(subset(tab, group == all_lab & period == act_period)$ind)/100
        temp_x <- temp_ind_all * as.numeric(tab$N)
        temp_n <- as.numeric(tab$N)
        # Fix for hide
        temp_x[is.na(temp_x)] <- 0
        temp_n[is.na(temp_n)] <- 0
        for (i in 1:length(funnelplot_probs)) {
            temp_binconf <- Hmisc::binconf(x = temp_x, n = temp_n, method = "exact", alpha = funnelplot_probs[i])
            colnames(temp_binconf) <- paste0("funnelplot_p", i, "_", c("est", "lo", "hi"))
            tab <- cbind(tab, 100 * temp_binconf)
        }
    }

    # Determine sorting variable
    if (ind_type == "factor") {
        tab$ind_sort <- round(rowSums(apply(cbind(tab[, if (!is.null(ind_factor_sortbycols)) {
            paste0("factor", 1:ind_factor_sortbycols)
        } else {
            substr(names(tab), 1, 6) == "factor"
        }]), 2, as.numeric)), 6)
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

    col_target_1 <- lightenCol("#FFCC33", factor = 0.4)
    col_target_2 <- lightenCol("#339966", factor = 0.4)

    col_factors <- c("#00b3f6","#ffb117","#005092","#19975d","#e56284","#66cccc","#db5524","#7f3705","#7c458a","#95bf5d",
                     "#7f7f7f","#8c8c8c","#999999","#a6a6a6","#b2b2b2","#bfbfbf","#cccccc","#d9d9d9","#e5e5e5","#f2f2f2")

    if (is.null(col)) {
        if (ind_type == "factor") {
            col <- col_factors
        } else {
            tab$col <- rep(col_ind_periods, nrow(tab))
            tab$col[act] <- col_ind_act
            tab$col[act & tab$group %in% all_lab] <- col_ind_all
            tab$col[act & tab$group %in% emph_lab] <- col_ind_emph
            tab$col[act & tab$group %in% subset_lab] <- col_ind_subset
        }
    }

    tab$col_border <- "#7f7f7f"
    tab$col_border[tab$hide] <- "transparent"

    tab$col_text <- "#000000"
    tab$col_text[tab$hide] <- "#7f7f7f"

    # Order
    if (sort) {
        if (target_values_high) {
            o <- order(tab[act, ]$ind_sort, decreasing = TRUE)
        } else {
            o <- order(tab[act, ]$ind_sort, tab[act, ]$group)
        }
    } else {
        o <- 1:nrow(tab[act, ])
    }

    # Shorten group names
    if (!is.null(group_maxchars)) {
        tab$group[nchar(tab$group) > group_maxchars] <- paste0(substr(tab$group[nchar(tab$group) > group_maxchars], 1, group_maxchars - 3), "...")
    }

    alphacol <- tail(255 * seq(0.25, 1, length.out = max(2, min(max_periods, num_periods))), min(max_periods, num_periods))

    tab_list <- list()
    for (i in 1:num_periods) {
        tab_list[[i]] <- subset(tab, period == show_periods[i])
        tab_list[[i]] <- tab_list[[i]][o, ]

        if (ind_type != "factor") {
            tab_list[[i]]$col <- lightenCol(col = tab_list[[i]]$col, factor = i/num_periods)
        }
    }

    # x-axis label and ticks
    barheight <- 1
    barheight_factor <- 1.4
    if (is.null(x_max)) {
        x_max <- max(pretty(c(0, ifelse(ind_numeric, max(tab$upper, na.rm = TRUE), max(tab$ind, na.rm = TRUE)))))
    }
    x_lim <- c(0, x_max)

    if (ind_type %in% c("logical", "factor")) {
        y_bp <- 0.5 * barheight_factor * barheight + barheight * (barheight_factor + 0.5 * (num_periods - 1)) * (0:(nrow(tab_list[[num_periods]]) - 1))
        y_lim <- c(0, barheight * (barheight_factor + 0.5 * (num_periods - 1)) * nrow(tab_list[[num_periods]]))
    } else {
        y_bp <- 0.5 * barheight_factor * barheight + barheight_factor * barheight * num_periods * (0:(nrow(tab_list[[num_periods]]) - 1))
        y_lim <- c(0, num_periods * barheight_factor * barheight * nrow(tab_list[[num_periods]]))
    }

    if (is.null(x_by)) {
        x_ticks <- pretty(x_lim)
    } else {
        x_ticks <- seq(x_lim[1], x_lim[2], x_by)
    }

    # Change margins
    linchheight <- strheight("X", "inch", cex = text_cex)
    linchwidth <- strwidth("X", "inch", cex = text_cex)
    linch_label <- 3 * linchwidth + max(strwidth(tab$group, "inch", cex = text_cex), na.rm = TRUE)
    linch_i <- 3 * linchwidth + max(strwidth(c(ind_title, round(tab$ind, 1)), "inch", cex = text_cex), na.rm = TRUE)
    linch_n <- 3 * linchwidth + max(strwidth(tab$n, "inch", cex = text_cex), na.rm = TRUE)

    par(mai = c(ifelse(!is.null(x_lab), 6, 4) * linchheight + legend_nrow * (ind_type == "factor") * linchheight + (num_periods > 1) * linchheight + (ind_type == "factor" &
        num_periods > 1) * linchheight + (funnelplot) * linchheight, ifelse(ind_numeric, linch_label + linch_n + linch_i, linch_label + linch_n), (2 + 2 * (!is.null(title)) +
        2 * (!is.null(title) & !is.null(subtitle1)) + 2 * (!is.null(title) & !is.null(subtitle1) & !is.null(subtitle2))) * linchheight, ifelse(ind_numeric | (!ind_numeric &
        !ind_showpct), 2, 5) * linchheight), bg = "#ffffff", xpd = TRUE)

    # Empty plot
    plot(x = x_lim, y = y_lim, axes = FALSE, type = "n", xlab = "", ylab = "")

    luserheight <- strheight("X", "user", cex = text_cex)
    luserwidth <- strwidth("X", "user", cex = text_cex)

    pos0x <- grconvertX(x = 0, from = "nfc", to = "user")
    pos1x <- grconvertX(x = 1, from = "nfc", to = "user")
    pos0y <- grconvertY(y = 0, from = "nfc", to = "user")
    pos1y <- grconvertY(y = 1, from = "nfc", to = "user")

    y_n_label <- grconvertY(grconvertY(y = y_lim[2], from = "user", to = "inches") + 1 * linchheight, from = "inches", to = "user")

    # Target values (area)
    if (!is.null(target_values) & length(target_values) > 1) {
        rect(xleft = min(target_values), ybottom = 0, xright = max(target_values), ytop = y_lim[2], col = col_target_1, border = NA)
        rect(xleft = ifelse(target_values_high, max(target_values), 0), ybottom = 0, xright = ifelse(target_values_high, x_max, min(target_values)), ytop = y_lim[2], col = col_target_2,
            border = NA)
    }

    # Grid
    for (i in x_ticks) {
        lines(x = rep(i, 2), y = c(0, y_lim[2]), lwd = 1, col = "#bdbdbd")
    }

    # Title
    if (!is.null(title)) {
        if (!is.null(subtitle1)) {
            if (!is.null(subtitle2)) {
                text(x = pos0x, y = y_n_label + 0.3 * (pos1y - y_n_label), labels = subtitle2, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_n_label + 0.3 * (pos1y - y_n_label) + 1 * 1.4 * strheight("", "user", cex = text_cex), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_n_label + 0.3 * (pos1y - y_n_label) + 2 * 1.8 * strheight("", "user", cex = text_cex), labels = title, pos = 4, cex = 1.5 * text_cex,
                  offset = 1)
            } else {
                text(x = pos0x, y = y_n_label + 0.4 * (pos1y - y_n_label), labels = subtitle1, pos = 4, cex = text_cex, offset = 1)
                text(x = pos0x, y = y_n_label + 0.4 * (pos1y - y_n_label) + 1.8 * strheight("", "user", cex = text_cex), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
            }
        } else {
            text(x = pos0x, y = 0.5 * sum(y_n_label, pos1y), labels = title, pos = 4, cex = 1.5 * text_cex, offset = 1)
        }
    }

    # Axis
    axis(side = 1, pos = y_lim[1], at = x_ticks, cex.axis = text_cex, las = 1, lwd = 3, col = "#d9d9d9")

    # Axis label
    y_xlab_zeropos <- ifelse(ind_type == "factor", ifelse(num_periods > 1, pos0y + 2 * luserheight, pos0y), ifelse(num_periods > 1 | funnelplot, pos0y + luserheight, pos0y)) +
        legend_nrow * (ind_type == "factor") * luserheight
    text(x = 0.5 * x_max, y = y_lim[1] - 0.6 * (y_lim[1] - y_xlab_zeropos), labels = x_lab, cex = text_cex, font = 2)

    # Plot
    if (ind_numeric) {
        # Target values (line)
        if (!is.null(target_values) & length(target_values) == 1) {
            lines(x = rep(target_values, 2), y = c(0, y_lim[2]), col = col_target_2, lwd = 3)
        }

        for (i in 1:num_periods) {
          gplots::plotCI(x = rev(tab_list[[i]]$ind), y = y_bp + (num_periods - i) * barheight, li = rev(tab_list[[i]]$lower), ui = rev(tab_list[[i]]$upper), col = rev(tab_list[[i]]$col),
                pt.bg = par("bg"), lwd = 2.5 * point_cex, add = TRUE, pch = 21, cex = point_cex, err = "x", sfrac = 0, gap = 0.75 * point_cex/2.25)
        }
    } else if (ind_type == "factor") {
        for (i in 1:num_periods) {
            for (j in rev(1:nrow(tab_list[[i]]))) {
                temp_tab <- tab_list[[i]][j, substr(names(tab), 1, 6) == "factor"]
                temp_tab <- cumsum(c(0, temp_tab))
                temp_col <- lightenCol(col = col, factor = i/num_periods)
                for (k in 2:length(temp_tab)) {
                  rect(xleft = temp_tab[k - 1], xright = temp_tab[k], ybottom = y_bp[length(y_bp) + 1 - j] - 0.5 * barheight + (num_periods - i) * 0.5 * barheight, ytop = y_bp[length(y_bp) +
                    1 - j] + 0.5 * barheight + (num_periods - i) * 0.5 * barheight, col = temp_col[k - 1], border = ifelse(i == num_periods & tab_list[[i]]$group[j] %in% emph_lab,
                    "#000000", ifelse(border, "#7f7f7f", NA)), lwd = ifelse(i == num_periods & tab_list[[i]]$group[j] %in% emph_lab, 2, 1))
                }
            }
        }

        # Target values (line)
        if (!is.null(target_values) & length(target_values) == 1) {
            lines(x = rep(target_values, 2), y = c(0, y_lim[2]), col = col_target_2, lwd = 3)
        }

        # Legend
        legend(x = 0.5 * x_max, y = ifelse(num_periods > 1, pos0y + 2 * luserheight, pos0y), legend = factor_legend, col = col, pch = 15, pt.cex = 1.75, bty = "n", cex = 0.8 *
            text_cex, xjust = 0.5, yjust = 0, y.intersp = 1, ncol = legend_ncol, text.width = if (legend_fixedtextwidth) {
            max(strwidth(factor_legend))
        } else {
            NULL
        })
    } else {
        # Funnelplot
        if (funnelplot) {
            temp_funnelplot_alphacol <- 255 * seq(0.75, 0.25, length.out = length(funnelplot_probs))
            temp_funnelplot_col <- rgb(t(col2rgb("#95bf5d")), alpha = temp_funnelplot_alphacol, maxColorValue = 255)
            temp_funnelplot_data <- tab_list[[num_periods]]
            temp_funnelplot_sectionheight <- barheight * barheight_factor
            temp_funnelplot_prev_lo <- 0
            temp_funnelplot_prev_hi <- 100
            for (i in 1:length(funnelplot_probs)) {
                temp_funnelplot_plot_lo <- rev(temp_funnelplot_data[, paste0("funnelplot_p", i, "_lo")])
                temp_funnelplot_plot_hi <- rev(temp_funnelplot_data[, paste0("funnelplot_p", i, "_hi")])
                rect(xleft = temp_funnelplot_prev_lo, xright = temp_funnelplot_plot_lo, ybottom = y_bp - 0.5 * temp_funnelplot_sectionheight, ytop = y_bp + 0.5 * temp_funnelplot_sectionheight,
                  col = temp_funnelplot_col[i], border = NA)
                rect(xleft = temp_funnelplot_plot_hi, xright = temp_funnelplot_prev_hi, ybottom = y_bp - 0.5 * temp_funnelplot_sectionheight, ytop = y_bp + 0.5 * temp_funnelplot_sectionheight,
                  col = temp_funnelplot_col[i], border = NA)
                temp_funnelplot_prev_lo <- temp_funnelplot_plot_lo
                temp_funnelplot_prev_hi <- temp_funnelplot_plot_hi
            }
            # Funnelplot legend
            legend(x = 0.5 * x_max, y = pos0y, legend = paste0("p < ", funnelplot_probs), col = temp_funnelplot_col, pch = 15, pt.cex = 1.75, bty = "n", cex = 0.8 * text_cex,
                xjust = 0.5, yjust = 0, y.intersp = 1, ncol = length(funnelplot_probs))
        }

        for (i in 1:num_periods) {
            rect(xleft = 0, xright = rev(tab_list[[i]]$ind), ybottom = y_bp - 0.5 * barheight + (num_periods - i) * 0.5 * barheight, ytop = y_bp + 0.5 * barheight + (num_periods -
                i) * 0.5 * barheight, col = rev(tab_list[[i]]$col), border = if (border) {
                rev(tab_list[[i]]$col_border)
            } else {
                NA
            })
        }

        # Target values (line)
        if (!is.null(target_values) & length(target_values) == 1) {
            lines(x = rep(target_values, 2), y = c(0, y_lim[2]), col = col_target_2, lwd = 3)
        }
    }

    # Period legend
    if (num_periods > 1) {
        period_legend_col <- lightenCol(col = col_ind_periods, factor = (1:num_periods/num_periods))
        if (ind_type != "factor") {
            period_legend_col <- c(period_legend_col[1:(length(period_legend_col) - 1)], col_ind_act)
        }
        legend(x = 0.5 * x_max, y = pos0y, legend = paste0(c(rep("", length(show_periods) - 1), "*"), show_periods), col = period_legend_col, pch = 15, pt.cex = 1.75, bty = "n",
            cex = 0.8 * text_cex, xjust = 0.5, yjust = 0, y.intersp = 1, ncol = num_periods, text.width = max(strwidth(show_periods)))
    }

    # Group labels
    if (ind_numeric) {
        text(x = ((linch_n + linch_i)/(linch_label + linch_n + linch_i)) * pos0x, y = y_bp, labels = rev(tab_list[[num_periods]]$group), pos = 2, cex = text_cex, col = rev(tab_list[[num_periods]]$col_text))
        text(x = (linch_i/(linch_label + linch_n + linch_i)) * pos0x, y = y_n_label, labels = ifelse(num_periods > 1, paste0(ind_noofcasestxt, "*"), ind_noofcasestxt), pos = 2,
            cex = text_cex, font = 2)
        text(x = (linch_i/(linch_label + linch_n + linch_i)) * pos0x, y = y_bp, labels = rev(tab_list[[num_periods]]$n), pos = 2, cex = text_cex)
        text(x = -luserwidth, y = y_n_label, labels = ifelse(num_periods > 1, paste0(ind_title, "*"), ind_title), pos = 2, cex = text_cex, font = 2)
        text(x = -luserwidth, y = y_bp, labels = rev(round(tab_list[[num_periods]]$ind, 1)), pos = 2, cex = text_cex)
    } else {
        text(x = (linch_n/(linch_label + linch_n)) * pos0x, y = y_bp, labels = rev(tab_list[[num_periods]]$group), pos = 2, cex = text_cex, col = rev(tab_list[[num_periods]]$col_text))
        text(x = -luserwidth, y = y_n_label, labels = ifelse(num_periods > 1, paste0(ind_noofcasestxt, "*"), ind_noofcasestxt), pos = 2, cex = text_cex, font = 2)
        text(x = -luserwidth, y = y_bp, labels = rev(tab_list[[num_periods]]$n), pos = 2, cex = text_cex)

        if (ind_showpct) {
            temp_pct <- paste(format(round(tab_list[[num_periods]]$ind, 0), nsmall = 0), "%")
            temp_pct[tab_list[[num_periods]]$hide] <- ""
            text(x = x_max, y = y_bp, labels = rev(temp_pct), pos = 4, cex = text_cex)
        }
    }

}
