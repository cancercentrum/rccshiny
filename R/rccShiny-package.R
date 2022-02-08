#' @aliases rccShiny-package
#' @keywords internal
"_PACKAGE"

# The rccShiny-package Depends on the R package 'shiny' (in DESCRIPTION) since
# 'rccShiny' is build on top of 'shiny' and is designed to be used in conjunction
# with 'shiny'.
#
# Inspiration:
# - https://r-pkgs.org/description.html#other-dependencies
# - https://r-pkgs.org/namespace.html#search-path

# Import all functions in 'shiny', 'graphics' and 'sp' since they are repeatedly used.
# Use :: for the few calls of functions in 'shinydashboard', 'shinyWidgets',
# 'DT', 'grDevices', 'Hmisc', 'dplyr', etc.
# Use @importFrom to import functions from 'highcharter' that are repeatedly used.
#
# Inspiration: https://r-pkgs.org/namespace.html#imports
#
#' @import shiny
#' @import graphics
#' @import sp
#' @importFrom highcharter hc_add_series hc_boost hc_chart hc_colorAxis
#' @importFrom highcharter hc_credits hc_legend hc_plotOptions hc_subtitle
#' @importFrom highcharter hc_title hc_tooltip hc_xAxis hc_yAxis hcaes hcmap
#' @importFrom highcharter highchart
#' @importFrom highcharter %>%
NULL

# To avoid undefined global variables when running check
utils::globalVariables(
  c(
    "GLOBAL_allLabel",
    "GLOBAL_comment",
    "GLOBAL_data",
    "GLOBAL_description",
    "GLOBAL_funnelplot",
    "GLOBAL_geoUnitsCountyInclude",
    "GLOBAL_geoUnitsCountyLabel",
    "GLOBAL_geoUnitsDefault",
    "GLOBAL_geoUnitsHospitalInclude",
    "GLOBAL_geoUnitsHospitalLabel",
    "GLOBAL_geoUnitsHospitalSelected",
    "GLOBAL_geoUnitsPatient",
    "GLOBAL_geoUnitsRegionInclude",
    "GLOBAL_geoUnitsRegionLabel",
    "GLOBAL_hideLessThan",
    "GLOBAL_hideLessThanCell",
    "GLOBAL_hideLessThanGroup",
    "GLOBAL_id",
    "GLOBAL_idAuthorisedToView",
    "GLOBAL_idInclude",
    "GLOBAL_idOverviewLink",
    "GLOBAL_idOverviewLinkInclude",
    "GLOBAL_inca",
    "GLOBAL_incaIncludeList",
    "GLOBAL_incaUserHospital",
    "GLOBAL_includeMissingColumn",
    "GLOBAL_includeTabs",
    "GLOBAL_language",
    "GLOBAL_medianiqrlab",
    "GLOBAL_npcrGroupPrivateOthers",
    "GLOBAL_outcome",
    "GLOBAL_outcomeClass",
    "GLOBAL_outcomeNumericExcludeNeg",
    "GLOBAL_outcomeTitle",
    "GLOBAL_outputHighcharts",
    "GLOBAL_periodDate",
    "GLOBAL_periodDateLevel",
    "GLOBAL_periodDefaultEnd",
    "GLOBAL_periodDefaultEnd_quarters",
    "GLOBAL_periodDefaultStart",
    "GLOBAL_periodDefaultStart_quarters",
    "GLOBAL_periodEnd",
    "GLOBAL_periodInclude",
    "GLOBAL_periodLabel",
    "GLOBAL_periodSplitDefault",
    "GLOBAL_periodStart",
    "GLOBAL_periodValues",
    "GLOBAL_periodValues_quarters",
    "GLOBAL_prob",
    "GLOBAL_prob_labels",
    "GLOBAL_propWithinShow",
    "GLOBAL_propWithinUnit",
    "GLOBAL_propWithinValue",
    "GLOBAL_regionChoices",
    "GLOBAL_regionLabel",
    "GLOBAL_regionSelected",
    "GLOBAL_regionSelection",
    "GLOBAL_sort",
    "GLOBAL_sortDescending",
    "GLOBAL_targetValues",
    "GLOBAL_textAfterSubtitle",
    "GLOBAL_textBeforeSubtitle",
    "GLOBAL_varOther",
    "GLOBAL_varOtherComparisonVariables",
    "GLOBAL_varOtherComparisonLabels",
    "df",
    "environmentVariables"
  )
)
