rccShinyIndTable <- function(language = "sv", group = NULL, group_factors = NULL, group_hide_less_than = FALSE, ind = NULL, ind_type = class(ind), ind_numeric_percentiles = c(0.25,
    0.5, 0.75), ind_factor_pct = FALSE, period = NULL, period_factors = NULL, period_alwaysinclude = TRUE, all_lab = rccShinyTXT(language = language)$RIKET, lab_percentiles = c(rccShinyTXT(language = language)$q1,
    rccShinyTXT(language = language)$median, rccShinyTXT(language = language)$q3), lab_noofcases = rccShinyTXT(language = language)$noofcases, lab_numerator = rccShinyTXT(language = language)$numerator,
    lab_denominator = rccShinyTXT(language = language)$denominator, lab_percent = rccShinyTXT(language = language)$percent, lab_total = rccShinyTXT(language = language)$total,
    lab_period = rccShinyTXT(language = language)$period, ndec = rccShinyDecimals(), subset = NULL, subset_lab = "SUBSET") {

    if (is.null(subset)) {
        subset <- rep(TRUE, length(group))
    }

    if (!(ind_type %in% c("logical", "numeric", "integer", "factor"))) {
        stop(paste0("Variable of class ", ind_type, " is not supported."))
    }

    #suppressMessages(require(plyr))

    if (is.null(period)) {
        period <- rep(1, length(group))
    }

    # Handle missing values
    include <- !is.na(ind) & !is.na(period)

    group <- as.character(group)
    group[is.na(group)] <- "(NA)"

    group <- group[include]
    if (!is.null(group_factors))
        group <- factor(group, levels = group_factors)
    ind <- ind[include]
    period <- period[include]
    if (!is.null(period_factors))
        period <- factor(period, levels = period_factors)
    subset <- subset[include]

    tabdata <- data.frame(group, ind, period, subset, stringsAsFactors = FALSE)
    byvars <- c("group", "period")

    # Tabulate
    summaryFunction <- function(x) {
        if (ind_type %in% c("numeric", "integer")) {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            if (hide) {
                measurements <- c(NA, NA, NA, NA)
            } else {
                measurements <- c(quantile(x$ind, na.rm = TRUE, probs = ind_numeric_percentiles), sum(!is.na(x$ind)))
            }
            names(measurements) <- c(lab_percentiles[1], lab_percentiles[2], lab_percentiles[3], lab_noofcases)
            return(measurements)
        } else if (ind_type == "logical") {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            if (hide) {
                measurements <- c(NA, NA, NA)
            } else {
                measurements <- c(sum(x$ind, na.rm = TRUE), sum(!is.na(x$ind)))
                measurements <- c(measurements, format(round(100 * (measurements[1]/measurements[2]), digits = ndec), nsmall = ndec))
            }
            names(measurements) <- c(lab_numerator, lab_denominator, lab_percent)
            return(measurements)
        } else if (ind_type %in% c("factor")) {
            hide <- ifelse(hideLowVolume, sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than, FALSE)
            if (hide) {
                measurements <- rep(NA, length(levels(x$ind)) + 1)
            } else {
                measurements <- if (ind_factor_pct) {
                  format(round(addmargins(100 * prop.table(table(x$ind))), digits = ndec), nsmall = ndec)
                } else {
                  c(table(x$ind), sum(!is.na(x$ind)))
                }
            }
            names(measurements) <- c(levels(x$ind), lab_total)
            return(measurements)
        }
    }

    hideLowVolume <- as.logical(group_hide_less_than)

    tab <- plyr::ddply(.data = subset(tabdata, subset), .variables = byvars, .fun = summaryFunction, .drop = FALSE)

    subsetUniqueGroups <- unique(tabdata$group[tabdata$subset])
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

    if (!period_alwaysinclude & length(unique(tabdata$period)) == 1) {
        tab <- tab[, colnames(tab)[colnames(tab) != "period"]]
    } else {
        tab[, "period"] <- as.character(tab[, "period"])
        colnames(tab)[colnames(tab) == "period"] <- lab_period
    }

    return(tab)

}
