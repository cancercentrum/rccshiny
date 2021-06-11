#' Creates map plot
#' @description internal function used by server.R for plot in tab Map.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
fMapPlot <- function(value = NULL, value_lim = NULL, value_order = c("\u00d6sterg\u00f6tland", "Blekinge", "Dalarna", "G\u00e4vleborg", "Gotland", "Halland", "J\u00e4mtland", "J\u00f6nk\u00f6ping",
    "Kalmar", "Kronoberg", "Norrbotten", "Orebro", "S\u00f6dermanland", "Sk\u00e5ne", "Stockholm", "Uppsala", "V\u00e4rmland", "V\u00e4sterbotten", "V\u00e4sternorrland", "V\u00e4stmanland", "V\u00e4stra G\u00f6taland"),
    value_order_return = FALSE, legend = "", col = NULL, col_border = "#7f7f7f", text_cex = 1, title = NULL, subtitle1 = NULL, subtitle2 = NULL, ndec = 1, rds_path = "./") {

    lifecycle::deprecate_stop("1.8.0", "rccShiny::fMapPlot()", "rccShiny::rcc2PlotMap()")

}
