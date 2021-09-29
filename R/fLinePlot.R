#' Creates trend plot
#' @description internal function used by server.R for plot in tab Trend.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @author Fredrik Sandin, RCC Mellansverige
#' @noRd
fLinePlot <- function(x = NULL, y = NULL, legend = NULL, legend_pos = "bottom", legend_pch = 15, legend_ncol = NULL, legend_textwidth = NULL, col = NULL, stacked_area = FALSE,
    linewidth = 4, linetype = "l", markers = TRUE, x_lim = NULL, y_lim = NULL, x_by = NULL, y_by = NULL, x_ticks_labels = NULL, title = NULL, subtitle1 = NULL, subtitle2 = NULL,
    x_lab = "x", y_lab = "y", text_cex = 1, target_values = NULL, target_values_high = NULL, target_values_labels = c("Mellanniv\u00e5 av m\u00e5luppfyllelse", "H\u00f6g niv\u00e5 av m\u00e5luppfyllelse")) {

    lifecycle::deprecate_stop("1.8.0", "rccShiny::fLinePlot()", "rccShiny::rcc2PlotLine()")

}
