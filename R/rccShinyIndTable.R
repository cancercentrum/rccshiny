#' Creates table
#' @description internal function used by server.R for table in tab Table.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyIndTable <-
  function(
    language = "sv",
    group = NULL,
    group_factors = NULL,
    group_hide_less_than = FALSE,
    group_hide_less_than_label = FALSE,
    group_hide_less_than_cell = FALSE,
    ind = NULL,
    ind_type = class(ind),
    ind_numeric_percentiles = c(0.25, 0.5, 0.75),
    ind_factor_pct = FALSE,
    period = NULL,
    period_factors = NULL,
    period_alwaysinclude = TRUE,
    all_lab = rccShinyTXT(language = language)$RIKET,
    lab_percentiles = c(rccShinyTXT(language = language)$q1, rccShinyTXT(language = language)$median, rccShinyTXT(language = language)$q3),
    lab_noofcases = rccShinyTXT(language = language)$noofcases,
    lab_numerator = rccShinyTXT(language = language)$numerator,
    lab_denominator = rccShinyTXT(language = language)$denominator,
    lab_percent = rccShinyTXT(language = language)$percent,
    lab_total = rccShinyTXT(language = language)$total,
    lab_period = rccShinyTXT(language = language)$period,
    ndec = rccShinyDecimals(),
    subset = NULL,
    subset_lab = "SUBSET",
    include_missing_column = FALSE,
    lab_missing = rccShinyTXT(language = language)$missing,
    lab_ind_null = rccShinyTXT(language = language)$noofcases
  ) {

    if (is.null(ind)) {
      ind_type <- "NULL"
      ind <- rep(TRUE, length(group))
    }

    if (is.null(subset)) {
      subset <- rep(TRUE, length(group))
    }

    if (!(ind_type %in% c("logical", "numeric", "integer", "factor", "NULL"))) {
      stop(paste0("Variable of class ", ind_type, " is not supported."))
    }

    if (is.null(period)) {
      period <- rep(1, length(group))
    }

    # Handle missing values
    if (include_missing_column){
      include <- !is.na(period)
    } else {
      include <- !is.na(ind) & !is.na(period)
    }

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
    summaryFunction <-
      function(x) {
        hide <-
          ifelse(
            hideLowVolume,
            sum(!is.na(x$ind), na.rm = TRUE) < group_hide_less_than,
            FALSE
          )
        if (ind_type == "NULL") {
          if (nrow(x) < group_hide_less_than) {
            measurements <- c(NA)
          } else {
            measurements <- nrow(x)
          }
          names(measurements) <- lab_ind_null
        } else if (ind_type %in% c("numeric", "integer")) {
          if (hide) {
            if (include_missing_column){
              measurements <- c(NA, NA, NA, NA, NA)
            } else {
              measurements <- c(NA, NA, NA, NA)
            }
          } else {
            if (include_missing_column){
              measurements <- c(stats::quantile(x$ind, na.rm = TRUE, probs = ind_numeric_percentiles), sum(!is.na(x$ind)), sum(is.na(x$ind)))
            } else {
              measurements <- c(stats::quantile(x$ind, na.rm = TRUE, probs = ind_numeric_percentiles), sum(!is.na(x$ind)))
            }
          }
          if (include_missing_column){
            names(measurements) <- c(lab_percentiles[1], lab_percentiles[2], lab_percentiles[3], lab_noofcases, lab_missing)
          } else {
            names(measurements) <- c(lab_percentiles[1], lab_percentiles[2], lab_percentiles[3], lab_noofcases)
          }
        } else if (ind_type == "logical") {
          hideCellLessThan <-
            ifelse(
              hideLowVolume,
              (sum(x$ind, na.rm = TRUE) > 0 & sum(x$ind, na.rm = TRUE) < group_hide_less_than_cell) |
                (sum(!x$ind, na.rm = TRUE) > 0 & sum(!x$ind, na.rm = TRUE) < group_hide_less_than_cell),
              FALSE
            )
          if (hide | hideCellLessThan) {
            measurements <- c(NA, NA)
          } else {
            measurements <- c(sum(x$ind, na.rm = TRUE), sum(!is.na(x$ind)))
          }
          if (hide) {
            if (include_missing_column){
              measurements <- c(measurements, NA, NA)
            } else {
              measurements <- c(measurements, NA)
            }
          } else {
            if (include_missing_column){
              measurements <- c(measurements, format(round(100 * (sum(x$ind, na.rm = TRUE) / sum(!is.na(x$ind))), digits = ndec), nsmall = ndec), sum(is.na(x$ind)))
            } else {
              measurements <- c(measurements, format(round(100 * (sum(x$ind, na.rm = TRUE) / sum(!is.na(x$ind))), digits = ndec), nsmall = ndec))
            }
          }
          if (include_missing_column){
            names(measurements) <- c(lab_numerator, lab_denominator, lab_percent, lab_missing)
          } else {
            names(measurements) <- c(lab_numerator, lab_denominator, lab_percent)
          }
        } else if (ind_type == "factor") {
          hideCellLessThan <-
            ifelse(
              hideLowVolume,
              any(table(x$ind) > 0 & table(x$ind) < group_hide_less_than_cell),
              FALSE
            )
          if (hide) {
            if (include_missing_column){
              measurements <- rep(NA, length(levels(x$ind)) + 2)
            } else {
              measurements <- rep(NA, length(levels(x$ind)) + 1)
            }
          } else {
            measurements <-
              if (ind_factor_pct) {
                if (hideCellLessThan) {
                  rep(NA, length(levels(x$ind)) + 1)
                } else {
                  format(round(stats::addmargins(100 * prop.table(table(x$ind))), digits = ndec), nsmall = ndec)
                }
              } else {
                if (hideCellLessThan) {
                  if (include_missing_column){
                    c(rep(NA, length(levels(x$ind))), sum(!is.na(x$ind)), NA)
                  } else {
                    c(rep(NA, length(levels(x$ind))), sum(!is.na(x$ind)))
                  }
                } else {
                  if (include_missing_column){
                    c(table(x$ind), sum(!is.na(x$ind)), sum(is.na(x$ind)))
                  } else {
                    c(table(x$ind), sum(!is.na(x$ind)))
                  }
                }
              }
          }
          if (include_missing_column){
            names(measurements) <- c(levels(x$ind), lab_total, lab_missing)
          } else {
            names(measurements) <- c(levels(x$ind), lab_total)
          }
        }
        return(measurements)
      }

    hideLowVolume <- as.logical(group_hide_less_than)

    tab <-
      plyr::ddply(
        .data = subset(tabdata, subset),
        .variables = byvars,
        .fun = summaryFunction,
        .drop = FALSE
      )

    subsetUniqueGroups <- unique(tabdata$group[tabdata$subset])
    if (!all(tabdata$subset) & !(length(subsetUniqueGroups) == 1 & all(subsetUniqueGroups %in% subset_lab))) {
      tab_subset <-
        plyr::ddply(
          .data = subset(tabdata, subset),
          .variables = byvars[byvars != "group"],
          .fun = summaryFunction,
          .drop = FALSE
        )
      tab_subset <- tab_subset[intersect(names(tab_subset), names(tab))]
      tab_subset$group <- subset_lab

      tab <- rbind(tab, tab_subset[, names(tab)])
    }

    if (!is.null(all_lab)) {
      tab_all <-
        plyr::ddply(
          .data = tabdata,
          .variables = byvars[byvars != "group"],
          .fun = summaryFunction,
          .drop = FALSE
        )
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

    if (group_hide_less_than_label != FALSE) {
      tab$group[is.na(tab[,ncol(tab)])] <- paste(tab$group[is.na(tab[,ncol(tab)])], group_hide_less_than_label)
    }

    return(tab)

  }
