#' Shiny apps for RCC
#' @description returns shiny apps that can be used as a complement to the annual reports from the cancer quality registries in Sweden.
#'
#' @param inca should output be as if on the INCA platform? Default is FALSE.
#' @param incaScript script to be run after loading data on the INCA platform. Default is NULL.
#' @param incaIncludeList Should the tab with list of patients be included if on the INCA platform? Default is TRUE.
#' @param folder name of folder where the results are placed. Default is "ind".
#' @param folderLinkText name displayed in ready-to-use html link returned by the function. Default is NULL, which results in the use of arguments outcomeTitle, folder and language to construct a name depending on the number of outcomes.
#' @param path search path to folder returned by the function. Default is working directory.
#' @param language vector giving the language for the app. Possible values are "sv" and "en". Default is "sv". See details.
#' @param data data frame containing the variables used when not on the INCA platform.
#' @param id optional name of variable in data containing the id of each individual. This is displayed in the list tab if on the INCA platform. Default is NULL.
#' @param idOverviewLink optional name of variable in data containing the HTML link to the patient overview on INCA for each individual. This is displayed in the list if on the INCA platform. Default is NULL.
#' @param idAuthorisedToView optional name of binary variable in data containing information on whether or not (1 = yes or 0 = no) the user running the app is authorised to see the id and idOverviewLink in the patient list on INCA. If id or idOverviewLink is provided, idAuthorisedToView must also be provided. Default is NULL.
#' @param outcome vector with names(s) of variable(s) in data containing the variable(s) to be presented in the app, for example a quality indicator. Variable(s) must be of type logical, factor or numeric. Default is "outcome". Observe that observations with missing values for outcome are not included in the output.
#' @param outcomeNumericExcludeNeg should negative values be excluded when presenting a numeric outcome? Particularly relevant for waiting times. Default is TRUE.
#' @param outcomeTitle label(s) of the outcome(s) shown in the app. Must be the same length as argument outcome. Default is argument outcome.
#' @param textBeforeSubtitle optional text placed before the subtitles in the tabs.
#' @param textAfterSubtitle optional text placed after the subtitles in the tabs.
#' @param comment optional comment printed under the sidebar panel.
#' @param description vector of 3 character strings, or a list of vectors, one for each language, shown in the three subsections in the tab Beskrivning/Description. Default is c(NA, NA, NA).
#' @param geoUnitsHospital optional name of variable in data containing hospital names. Variable must be of type character. If NULL or if variable is not found in 'data', hospital is not available as a level of presentation. Default is "sjukhus". At least one geoUnit must be given.
#' @param geoUnitsHospitalAlt optional name of variable in data containing alternative hospital names to be used when only one region is selected to be shown. Variable must be of type character. If NULL or if variable is not found in 'data', geoUnitsHospital is used. Default is "sjukhus_alt".
#' @param geoUnitsHospitalCode optional name of variable in data containing hospital codes. Variable must be of type numeric. If NULL or if variable is not found in 'data', the list tab can not be displayed. The hospital codes are used to determine which patients to show in the list tab by matching it to the enviromental variable on INCA containing the hospital code of the logged in user. Default is "sjukhuskod".
#' @param geoUnitsHospitalSelected optional name of the choice that should initially be selected in the list of hospitals. Variable must be of type character. Default is NULL.
#' @param geoUnitsCounty optional name of variable in data containing county codes. Variable must be of type numeric. Can be either county of residence for the patient or the county the hospital belongs to. See details for valid values. If NULL or if variable is not found in 'data', county is not available as a level of presentation. Default is "landsting". At least one geoUnit must be given. To be implemented: Codes for county of hospital are fetched automatically from hospital codes.
#' @param geoUnitsRegion optional name of variable in data containing region codes (1=Stockholm, 2=Mellansverige, 3=Sydöstra, 4=Södra, 5=Västra, 6=Norra, NA=Uppgift saknas). Variable must be of type numeric. Can be either region of residence for the patient or the region the hospital belongs to. If NULL or if variable is not found in 'data', region is not available as a level of presentation. Default is "region". At least one geoUnit must be given. To be implemented: Codes for region of hospital are fetched automatically from hospital codes.
#' @param geoUnitsPatient if geoUnitsCounty/geoUnitsRegion is county/region of residence for the patient (LKF). If FALSE and a hospital is chosen by the user in the sidebar panel the output is highlighted for the respective county/region that the hospital belongs to. Default is FALSE.
#' @param geoUnitsDefault optional default level of presentation. Valid values are "region", "county", "hospital", and any of the variable names (var) in varOtherComparison. Default is "county".
#' @param regionSelection adds a widget to the sidebar panel with the option to show only one region at a time. Default is TRUE.
#' @param regionSelectionDefault optional numeric value (1-6) which specifies the default selection in the list of regions. Default is NULL, which selects all regions.
#' @param regionLabel change the default label of widget shown in the sidebar panel if regionSelection = TRUE. Should be a character vector of length 1 or a vector with a label corresponding to each language. Default is NULL.
#' @param period name of variable in data containing time periods, for example date or year of diagnosis. Variable must be of type numeric or Date. Default is "period". If period = NULL then no period variable is required and period will not be included anywhere in the Shiny app.
#' @param periodDateLevel If the variable in data containing time period is of type Date, how are the time periods going to be grouped? Allowed values are "year and "quarter", with default "year".
#' @param periodLabel change the default label of the period widget in the sidebar panel. Should be a character vector of length 1 or a vector with a label corresponding to each language. Default is NULL.
#' @param periodDefaultStart optional value which specifies the preselected default start of the period of interest. Default is NULL.
#' @param periodDefaultEnd optional value which specifies the preselected default end of the period of interest. Default is NULL.
#' @param varOtherComparison optional list of variable(s) which beside geoUnits is to be available as level of comparison in the sidebar panel. Arguments to the list are: var (name of variable in data) and label (optional label shown in the list in sidebar panel, defaults to var if not given).
#' @param varOther optional list of variable(s), other than period and geoUnits, to be shown in the sidebar panel. Arguments to the list are: var (name of variable in data), label (label shown over widget in sidebar panel), choices (which values of var should be shown, min, max for continuous variables), selected (which values should be selected when app is launched, default is all avalible values), multiple (should multiple choises be availible, default is TRUE), showInTitle (should selection be displayed in subtitle, default is TRUE). Observe that observations with missing values for varOthers are not included in the output.
#' @param allLabel change the default label for the total in all plots and tables. Should be a character vector of length 1 or a vector with a label corresponding to each language. Default is NULL.
#' @param targetValues optional vector or list of vectors (one for each outcome) with 1-2 target levels to be plotted in the tabs Jämförelse/Comparison and Trend for outcomes of type logical or numeric. If the outcome is numeric the target levels are shown when "Andel inom..."/"Proportion within..." is selected, and then only for the default propWithinValue.
#' @param funnelplot adds a widget to the sidebar panel with the option to show a funnel plot in the tab Jämförelse/Comparison. Only applicaple for dichotomous variables. Default is FALSE.
#' @param sort should the bars in tab Jämförelse/Comparison be sorted? Default is TRUE.
#' @param sortDescending should the bars in tab Jämförelse/Comparison be plotted in descending order? The argument could have the same length as argument outcome, giving different values for each outcome. Default is NULL, which sorts logical outcomes in descending order and continuous outcomes in ascending order.
#' @param propWithinShow display the choice "Andel inom..."/"Proportion within..." for numeric outcome(s). Default is TRUE.
#' @param propWithinUnit change the default unit shown for numeric outcome when "Andel inom..."/"Proportion within..." is selected. Should be a character vector of length 1 or a vector with a label corresponding to each language. Default is NULL.
#' @param propWithinValue vector with default value(s) shown for numeric outcome(s) when "Andel inom..."/"Proportion within..." is selected. The length of the vector should be either 1 or the length of outcome. Default is 30.
#' @param prob a vector of quantiles for summarizing indicator if indicator is numeric. Defaults to c(0.25,0.5,0.75).
#' @param hideLessThan value under which groups are supressed. Default is 5 and all values < 5 are set to 5 unless inca = TRUE.
#' @param hideLessThanCell if a cell for a group falls below this value, the absolute number for the group is supressed and only proportion or median etc. is displayed. Default is 0 (disabled).
#' @param gaPath optional path to Google Analytics .js-file. Default is NULL.
#' @param npcrGroupPrivateOthers deprecated argument, see geoUnitsHospitalAlt.
#' @param outputHighcharts should Highcharts be used to draw the figures? Default is FALSE.
#' @param includeTabs vector containing names of which tabs should be included in the shiny app. Default is c("compare", "table", "map", "trend", "description").
#' @param includeMissingColumn Include a column in Table tab for the number of post with a missing value. Default is FALSE.
#'
#' @details Valid values for geoUnitsCounty are:
#'   \tabular{lll}{
#' \strong{geoUnitsPatient}   \tab \strong{!geoUnitsPatient} \tab \strong{Text shown}\cr
#' 1\tab 10,11 \tab  Stockholm\cr
#' 3\tab 12 \tab  Uppsala\cr
#' 4\tab 13 \tab  Södermanland\cr
#' 5\tab 21 \tab  Östergötland\cr
#' 6\tab 22 \tab Jönköping\cr
#' 7\tab 23 \tab  Kronoberg\cr
#' 8\tab 24,25 \tab  Kalmar\cr
#' 9\tab 26 \tab  Gotland\cr
#' 10\tab 27 \tab  Blekinge\cr
#' 12\tab 28,30,41   \tab  Skåne\cr
#' 13\tab 42 \tab  Halland\cr
#' 14\tab 50,51,52,53   \tab  Västra Götaland\cr
#' 17\tab 54 \tab  Värmland\cr
#' 18\tab 55 \tab  Örebro\cr
#' 19\tab 56 \tab  Västmanland\cr
#' 20\tab 57 \tab  Dalarna\cr
#' 21\tab 61 \tab  Gävleborg\cr
#' 22\tab 62 \tab  Västernorrland\cr
#' 23\tab 63 \tab  Jämtland\cr
#' 24\tab 64 \tab  Västerbotten\cr
#' 25\tab 65 \tab  Norrbotten\cr
#' -\tab 91,92,93,94,95,96 \tab  Övriga/privata - region\cr
#' NA\tab NA \tab  Uppgift saknas
#' }
#'
#'
#'
#' If language = c("sv", "en") the following applies to arguments textBeforeSubtitle, textAfterSubtitle, comment, regionLabel, label in list varOther: if there are two values the first is used in the Swedish version and the second in the English version. If there is only one value this is recycled in both versions.
#' The following applies to argument outcomeTitle, description: the arguments should be given in a list, the first listargument is used in the Swedish version and the second in the English version. The Swedish title(s) will be recycled if English is missing.
#' The following applies to arguments outcome, geoUnitsHospital, geoUnitsHospitalAlt, geoUnitsCounty, geoUnitsRegion, period, var in list varOther: in the English version the variable name with the suffix _en (for example "outcome_en") will be used if this exists and otherwise the Swedish variable name will be recycled.
#'
#' @author Fredrik Sandin, RCC Mellansverige
#'
#' @return A folder path/sv|en/folder containing: global.R, server.R, ui.R, data/data.RData, docs/description.html.
#' @examples
#' ind1 <- rccShiny2(
#'   data = rccShinyData,
#'   folder = "Indikator1",
#'   folderLinkText = "Indikator 1",
#'   outcome = paste0("outcome", 1:3),
#'   outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
#'   description = c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
#'   varOther = list(
#'     list(
#'       var = "age",
#'       label = "Alder vid diagnos"
#'     ),
#'     list(
#'       var = "stage",
#'       label = "Stadium",
#'       choices = c("I", "II"),
#'       selected = "I",
#'       multiple = TRUE,
#'       showInTitle = TRUE
#'     )
#'   ),
#'   funnelplot = TRUE
#' )
#' \dontrun{
#' shiny::runApp("./sv/Indikator1")
#'
#' cat(ind1) # displays the html link that can be used in index.html
#' }
#'
#' # For Swedish/English version
#' rccShinyData$outcome1_en <- rccShinyData$outcome1
#' rccShiny2(
#'   language = c("sv", "en"),
#'   data = rccShinyData,
#'   folder = "Indikator2",
#'   folderLinkText = c("Indikator 2", "Indicator 2"),
#'   outcome = "outcome1",
#'   outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
#'   textBeforeSubtitle = c("Nagot pa svenska", "Something in English"),
#'   description = list(
#'     c("Har beskrivs indikatorn.", "Viktig information!", "Information om variabler etc."),
#'     c("Description of the indicator", "Important information!", "Information on variables etc.")
#'   ),
#'   varOther = list(
#'     list(
#'       var = "age",
#'      label = c("Alder vid diagnos", "Age at diagnosis"),
#'       choices = c(0, 120)
#'     )
#'   ),
#'   targetValues = c(95, 99)
#' )
#'
#' # Using stage as level of presentation
#' rccShiny2(
#'   language = c("sv", "en"),
#'   data = rccShinyData,
#'   folder = "Indikator3",
#'   outcome = "outcome1",
#'   outcomeTitle = list("Dikotom", "Dichotomous"),
#'   description = list(
#'     c("Beskrivning!", "Viktigt!", "Information!"),
#'     c("Description!", "Important!", "Information!")
#'   ),
#'   geoUnitsDefault = "stage",
#'   varOtherComparison = list(
#'     list(
#'       var = "stage",
#'       label = c(
#'         "Stadium",
#'         "Stage"
#'       )
#'     )
#'   ),
#'   varOther = list(
#'     list(
#'       var = "age",
#'       label = c(
#'         "Alder vid diagnos",
#'         "Age at diagnosis"
#'       )
#'     )
#'   ),
#'   sort = FALSE
#' )
#' @export

rccShiny2 <-
  function(
    inca = FALSE,
    incaScript = NULL,
    incaIncludeList = TRUE,
    folder = "ind",
    folderLinkText = NULL,
    path = getwd(),
    language = "sv",
    data = NULL,
    id = NULL,
    idOverviewLink = NULL,
    idAuthorisedToView = NULL,
    outcome = "outcome",
    outcomeNumericExcludeNeg = TRUE,
    outcomeTitle = outcome,
    textBeforeSubtitle = NULL,
    textAfterSubtitle = NULL,
    comment = "",
    description = rep(NA, 3),
    geoUnitsHospital = "sjukhus",
    geoUnitsHospitalAlt = "sjukhus_alt",
    geoUnitsHospitalCode = "sjukhuskod",
    geoUnitsHospitalSelected = NULL,
    geoUnitsCounty = "landsting",
    geoUnitsRegion = "region",
    geoUnitsPatient = FALSE,
    geoUnitsDefault = "county",
    regionSelection = TRUE,
    regionSelectionDefault = NULL,
    regionLabel = NULL,
    period = "period",
    periodDateLevel = "year",
    periodLabel = NULL,
    periodDefaultStart = NULL,
    periodDefaultEnd = NULL,
    varOtherComparison = NULL,
    varOther = NULL,
    allLabel = NULL,
    targetValues = NULL,
    funnelplot = FALSE,
    sort = TRUE,
    sortDescending = NULL,
    propWithinShow = TRUE,
    propWithinUnit = NULL,
    propWithinValue = 30,
    prob = c(0.25, 0.50, 0.75),
    hideLessThan = 5,
    hideLessThanCell = 0,
    gaPath = NULL,
    npcrGroupPrivateOthers = FALSE,
    outputHighcharts = FALSE,
    includeTabs = c("compare", "table", "map", "trend", "description"),
    includeMissingColumn = FALSE
  ) {

    # # # # # # # # # # # # # # # #
    # Checking input parameters
    # # # # # # # # # # # # # # # #

    # Testing function
    testVariableError <-
      function(
        var = "var",
        listAllowed = TRUE
      ) {
        if (is.null(get(var)))
          stop("'", var, "' is missing", call. = FALSE)
        if (any(is.na(get(var))))
          stop("'", var, "' is missing", call. = FALSE)
        if (is.list(get(var))) {
          if (!listAllowed) {
            stop("'", var, "' should not be a list", call. = FALSE)
          } else {
            tempList <- get(var)
            for (i in 1:length(tempList)) {
              if (!is.character(tempList[[i]]))
                stop("'", var, "' should be of type character", call. = FALSE)
            }
          }
        } else if (!is.character(get(var))) {
            stop("'", var, "' should be of type character", call. = FALSE)
        }
      }

    # inca
    if (is.null(inca) | !is.logical(inca) | length(inca) != 1)
      stop("'inca' should a logical vector of length 1", call. = FALSE)
    if (inca) {
      testVariableError("incaScript", listAllowed = FALSE)
      if (!file.exists(incaScript))
        stop("The file '", incaScript, "' does not exist", call. = FALSE)
    }

    # incaIncludeList
    if (is.null(incaIncludeList) | !is.logical(incaIncludeList) | length(incaIncludeList) != 1)
      stop("'incaIncludeList' should a logical vector of length 1", call. = FALSE)

    # folder
    testVariableError("folder", listAllowed = FALSE)
    if (length(folder) != 1)
      stop("'folder' should be of length 1", call. = FALSE)

    # folderLinkTest
    if (is.null(folderLinkText)) {
      if (length(outcome) > 1 | length(language) > 1) {
        folderLinkText <- paste0(folder, "_", language)
      } else {
        folderLinkText <- unlist(outcomeTitle)
      }
    }
    testVariableError("folderLinkText", listAllowed = FALSE)
    if (length(language) != length(folderLinkText))
      stop(paste0("'language' and 'folderLinkText' should have the same number of elements"), call. = FALSE)

    # path
    if (!inca) {
      if (is.null(path) | !is.character(path) | length(path) != 1)
        stop("'path' should be a character vector of length 1", call. = FALSE)
      if (!dir.exists(path))
        stop("The folder '", path, "' does not exist", call. = FALSE)
    }

    # language
    if (is.null(language) | !is.character(language))
      stop("'language' should be a character vector", call. = FALSE)
    if (inca & length(language) > 1)
      stop("When 'inca' = TRUE, 'language' should be of length 1", call. = FALSE)

    # data
    if (!inca) {
      if (is.null(data) | !is.data.frame(data))
        stop("'data' should be a data.frame", call. = FALSE)
    }

    # id
    if (!is.null(id) & (!is.character(id) | length(id) != 1))
      stop("'id' should be either NULL or a character vector of length 1", call. = FALSE)

    # idOverviewLink
    if (!is.null(idOverviewLink) & (!is.character(idOverviewLink) | length(idOverviewLink) != 1))
      stop("'idOverviewLink' should be either NULL or a character vector of length 1", call. = FALSE)

    # idAuthorisedToView
    if (!is.null(idAuthorisedToView) & (!is.character(idAuthorisedToView) | length(idAuthorisedToView) != 1))
      stop("'idAuthorisedToView' should be either NULL or a character vector of length 1", call. = FALSE)

    # outcome
    testVariableError("outcome", listAllowed = FALSE)

    # outcomeNumericExcludeNeg
    if (is.null(outcomeNumericExcludeNeg) | !is.logical(outcomeNumericExcludeNeg) | length(outcomeNumericExcludeNeg) != 1)
      stop("'outcomeNumericExcludeNeg' should a logical vector of length 1", call. = FALSE)

    # outcomeTitle
    testVariableError("outcomeTitle")
    if (!is.list(outcomeTitle))
      outcomeTitle <- list(outcomeTitle)
    for (i in 1:length(outcomeTitle)) {
      if (length(outcome) != length(outcomeTitle[[i]]))
        stop("'outcome' and 'outcomeTitle' should have the same number of elements", call. = FALSE)
    }

    # textBeforeSubtitle
    if (is.null(textBeforeSubtitle))
      textBeforeSubtitle <- ""
    testVariableError("textBeforeSubtitle", listAllowed = FALSE)

    # textAfterSubtitle
    if (is.null(textAfterSubtitle))
      textAfterSubtitle <- ""
    testVariableError("textAfterSubtitle", listAllowed = FALSE)

    # comment
    if (is.null(comment))
      comment <- ""
    testVariableError("comment", listAllowed = FALSE)

    # description
    if (is.null(description))
      stop("'description' is missing", call. = FALSE)
    if (!is.list(description))
      description <- list(description)
    for (i in 1:length(description)) {
      if (!is.character(description[[i]]) & !all(is.na(description[[i]])))
        stop("'description' should be of type character", call. = FALSE)
    }

    # geoUnitsHospital
    if (!is.null(geoUnitsHospital) & (!is.character(geoUnitsHospital) | length(geoUnitsHospital) != 1))
      stop("'geoUnitsHospital' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsHospitalAlt
    if (!is.null(geoUnitsHospitalAlt) & (!is.character(geoUnitsHospitalAlt) | length(geoUnitsHospitalAlt) != 1))
      stop("'geoUnitsHospitalAlt' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsHospitalCode
    if (!is.null(geoUnitsHospitalCode) & (!is.character(geoUnitsHospitalCode) | length(geoUnitsHospitalCode) != 1))
      stop("'geoUnitsHospitalCode' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsHospitalSelected
    if (!is.null(geoUnitsHospitalSelected) & (!is.character(geoUnitsHospitalSelected) | length(geoUnitsHospitalSelected) != 1))
      stop("'geoUnitsHospitalSelected' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsCounty
    if (!is.null(geoUnitsCounty) & (!is.character(geoUnitsCounty) | length(geoUnitsCounty) != 1))
      stop("'geoUnitsCounty' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsRegion
    if (!is.null(geoUnitsRegion) & (!is.character(geoUnitsRegion) | length(geoUnitsRegion) != 1))
      stop("'geoUnitsRegion' should be either NULL or a character vector of length 1", call. = FALSE)

    # geoUnitsPatient
    if (is.null(geoUnitsPatient) | !is.logical(geoUnitsPatient) | length(geoUnitsPatient) != 1)
      stop("'geoUnitsPatient' should a logical vector of length 1", call. = FALSE)

    # geoUnitsDefault
    if (is.null(geoUnitsDefault) | !is.character(geoUnitsDefault) | length(geoUnitsDefault) != 1)
      stop("'geoUnitsDefault' should a character vector of length 1", call. = FALSE)

    # regionSelection
    if (is.null(regionSelection) | !is.logical(regionSelection) | length(regionSelection) != 1)
      stop("'regionSelection' should a logical vector of length 1", call. = FALSE)

    # regionSelectionDefault
    if (!is.null(regionSelectionDefault)) {
      if (!is.numeric(regionSelectionDefault) | length(regionSelectionDefault) != 1)
        stop("'regionSelectionDefault' should be either NULL or a numeric vector of length 1", call. = FALSE)
      if (!(regionSelectionDefault %in% 1:6))
        stop("Valid values for 'regionSelectionDefault' are 1, 2, 3, 4, 5 or 6", call. = FALSE)
    }

    # regionLabel
    if (is.null(regionLabel)) {
      regionLabel <- rccShinyTXT(language = language)$limitRegion
    } else if (!is.character(regionLabel) | !(length(regionLabel) %in% c(1, length(language))))
      stop("'regionLabel' should be either NULL or a character vector of length 1 or same length as 'language'", call. = FALSE)

    # period
    if (!is.null(period) & (!is.character(period) | length(period) != 1))
      stop("'period' should be either NULL or a character vector of length 1", call. = FALSE)

    # periodDateLevel
    if (is.null(periodDateLevel)) {
      periodDateLevel <- "year"
    } else {
      if (!is.character(periodDateLevel))
        stop("'periodDateLevel' should be a character vector", call. = FALSE)
      if (!all(periodDateLevel %in% c("year", "quarter")))
        stop("Allowed values for 'periodDateLevel' are 'year' and 'quarter'", call. = FALSE)
    }

    # periodLabel
    if (is.null(periodLabel)) {
      periodLabel <- rccShinyTXT(language = language)$period
    } else if (!is.character(periodLabel) | !(length(periodLabel) %in% c(1, length(language))))
      stop("'periodLabel' should be either NULL or a character vector of length 1 or same length as 'language'", call. = FALSE)

    # periodDefaultStart, periodDefaultEnd
    if (!is.null(periodDefaultStart) & length(periodDefaultStart) != 1)
      stop("'periodDefaultStart' should be either NULL or a vector of length 1", call. = FALSE)
    if (!is.null(periodDefaultEnd) & length(periodDefaultEnd) != 1)
      stop("'periodDefaultEnd' should be either NULL or a vector of length 1", call. = FALSE)

    # varOtherComparison
    if (!is.null(varOtherComparison) & (!is.list(varOtherComparison) | length(varOtherComparison) < 1))
      stop("'varOtherComparison' should be either NULL or a list with at least one element", call. = FALSE)
    if (!all(sapply(varOtherComparison, is.list)))
      stop("The elements of 'varOtherComparison' should be lists", call. = FALSE)
    if (!is.null(varOtherComparison)) {
      for (i in 1:length(varOtherComparison)) {
        if (!("var" %in% names(varOtherComparison[[i]])) | is.null(varOtherComparison[[i]]$var))
          stop("'var' is missing from varOtherComparison[[", i, "]]", call. = FALSE)
      }
    }

    # varOther
    if (!is.null(varOther) & (!is.list(varOther) | length(varOther) < 1))
      stop("'varOther' should be either NULL or a list with at least one element", call. = FALSE)
    if (!all(sapply(varOther, is.list)))
      stop("The elements of 'varOther' should be lists", call. = FALSE)
    if (!is.null(varOther)) {
      for (i in 1:length(varOther)) {
        if (!("var" %in% names(varOther[[i]])) | is.null(varOther[[i]]$var))
          stop("'var' is missing from varOther[[", i, "]]", call. = FALSE)
      }
    }

    # allLabel
    if (is.null(allLabel)) {
      allLabel <- rccShinyTXT(language = language)$RIKET
    } else if (!is.character(allLabel) | !(length(allLabel) %in% c(1, length(language))))
      stop("'allLabel' should be either NULL or a character vector of length 1 or same length as 'language'", call. = FALSE)

    # targetValues
    if (!is.list(targetValues))
      targetValues <- list(targetValues)
    targetValuesOld <- targetValues
    targetValues <- rep(list(NULL), length(outcome))
    for (i in 1:length(targetValues)) {
      if (i <= length(targetValuesOld)) {
        if (!is.null(targetValuesOld[[i]]))
          targetValues[[i]] <- targetValuesOld[[i]]
        if (!is.null(targetValues[[i]]) & (!is.numeric(targetValues[[i]]) | length(targetValues[[i]]) < 1 | length(targetValues[[i]]) > 2))
          stop("'targetValues[[",i,"]]' should be either NULL or a numeric vector of length 1 or 2", call. = FALSE)
      }
    }

    # funnelplot
    if (is.null(funnelplot) | is.na(funnelplot) | !is.logical(funnelplot) | length(funnelplot) != 1)
      stop("'funnelplot' should be a logical vector of length 1", call. = FALSE)

    # sort
    if (is.null(sort) | is.na(sort) | !is.logical(sort) | length(sort) != 1)
      stop("'sort' should be a logical vector of length 1", call. = FALSE)

    # sortDescending
    if (!is.null(sortDescending) & (!is.logical(sortDescending) | length(sortDescending) < 1))
      stop("'sortDescending' should be either NULL or a logical vector with at least one element", call. = FALSE)
    if (!is.null(sortDescending) & length(sortDescending) < length(outcome))
      sortDescending <- c(
        sortDescending,
        rep(TRUE, length(outcome) - length(sortDescending))
      )

    # propWithinShow
    if (is.null(propWithinShow) | is.na(propWithinShow) | !is.logical(propWithinShow) | length(propWithinShow) != 1)
      stop("'funnelplot' should be a logical vector of length 1", call. = FALSE)

    # propWithinValue
    if (is.null(propWithinValue) | any(is.na(propWithinValue)) | !(length(propWithinValue) %in% c(1, length(outcome))) | (!is.integer(propWithinValue) & !is.numeric(propWithinValue)))
      stop("'propWithinValue' should be a numeric or integer vector of length 1 or length of 'outcome'", call. = FALSE)

    # propWithinUnit
    if (is.null(propWithinUnit)) {
      propWithinUnit <- rccShinyTXT(language = language)$propWithinUnit
    } else if (!is.character(propWithinUnit) | !(length(propWithinUnit) %in% c(1, length(language))))
      stop("'propWithinUnit' should be either NULL or a character vector of length 1 or same length as 'language'", call. = FALSE)

    # prob
    if (is.null(prob)) {
      prob <- c(0.25, 0.50, 0.75)
    } else if (length(prob) != 3 | !is.numeric(prob) | !(1 >= prob[3] & prob[3] >= prob[2] & prob[2] >= prob[1])) {
      stop("'prob' should be an increasing numeric vector of length 3 with values in [0,1]", call. = FALSE)
    }

    # hideLessThan
    if (is.null(hideLessThan) | !is.numeric(hideLessThan) | length(hideLessThan) != 1)
      stop("'hideLessThan' should be a numeric vector of length 1", call. = FALSE)

    # hideLessThanCell
    if (is.null(hideLessThanCell) | !is.numeric(hideLessThanCell) | length(hideLessThanCell) != 1)
      stop("'hideLessThanCell' should be a numeric vector of length 1", call. = FALSE)

    # gaPath
    if (!is.null(gaPath) & (!is.character(gaPath) | length(gaPath) != 1))
      stop("'gaPath' should be either NULL or a character vector of length 1", call. = FALSE)
    if (!is.null(gaPath))
      gaPath <- ifelse(substr(gaPath, 1, 1) == "/", gaPath, paste0("/", gaPath))

    # npcrGroupPrivateOthers (deprecated)
    npcrGroupPrivateOthers <- FALSE

    # outputHighcharts
    if (is.null(outputHighcharts) | !is.logical(outputHighcharts) | length(outputHighcharts) != 1)
      stop("'outputHighcharts' should be a logical vector of length 1", call. = FALSE)

    # includeTabs
    if (is.null(includeTabs) | !is.character(includeTabs))
      stop("'includeTabs' should be a character vector", call. = FALSE)

    # includeMissingColumn
    if (is.null(includeMissingColumn) | !is.logical(includeMissingColumn) | length(includeMissingColumn) != 1)
      stop("'includeMissingColumn' should be a logical vector of length 1", call. = FALSE)
    # includeMissingColumn=TRUE but factor contains 'Uppgift saknas' or 'Missing'
    miss.values <- c("Uppgift saknas", "Missing")
    if (isTRUE(includeMissingColumn) & any(unlist(lapply(data.frame(data[,outcome]), levels)) %in% miss.values)){
      tmp <- outcome[unlist(lapply(data.frame(data[,outcome]), function(x) any(levels(x) %in% miss.values)))]
      data[, tmp][data[, tmp] == miss.values[1] | data[, tmp] == miss.values[2]] <- NA
      data[, tmp] <- droplevels(data[, tmp])
      message("'includeMissingColumn' = TRUE and 'outcome' contains value 'Uppgift saknas' or 'Missing'. \nTo avoid errors the levels 'Uppgift saknas' and/or 'Missing' have been converted to 'NA'. \nIf you want to keep your Missing value level, change 'includeMissingColumn' to FALSE")
    }

    # # # # # # # # # # # # # # # #
    # Produce app for each language
    # # # # # # # # # # # # # # # #

    # Create vector for link to html-document
    tempLinks <- vector()

    for (loopLanguage in language) {

      optionsList <-
        list(
          inca = inca,
          incaScript = incaScript,
          incaIncludeList = incaIncludeList,
          incaUserHospital = NA,
          language = loopLanguage,
          whichLanguage = which(language == loopLanguage),
          pageTitle = paste0(folder, "_", loopLanguage),
          data = data,
          id = id,
          idOverviewLink = idOverviewLink,
          idAuthorisedToView = idAuthorisedToView,
          outcome = outcome,
          outcomeNumericExcludeNeg = outcomeNumericExcludeNeg,
          outcomeTitle = outcomeTitle,
          textBeforeSubtitle = textBeforeSubtitle,
          textAfterSubtitle = textAfterSubtitle,
          comment = comment,
          description = description,
          geoUnitsHospital = geoUnitsHospital,
          geoUnitsHospitalAlt = geoUnitsHospitalAlt,
          geoUnitsHospitalCode = geoUnitsHospitalCode,
          geoUnitsHospitalSelected = geoUnitsHospitalSelected,
          geoUnitsCounty = geoUnitsCounty,
          geoUnitsRegion = geoUnitsRegion,
          geoUnitsPatient = geoUnitsPatient,
          geoUnitsDefault = geoUnitsDefault,
          regionSelection = regionSelection,
          regionSelectionDefault = regionSelectionDefault,
          regionLabel = regionLabel,
          period = period,
          periodDateLevel = periodDateLevel,
          periodLabel = periodLabel,
          periodDefaultStart = periodDefaultStart,
          periodDefaultEnd = periodDefaultEnd,
          varOtherComparison = varOtherComparison,
          varOther = varOther,
          allLabel = allLabel,
          targetValues = targetValues,
          funnelplot = funnelplot,
          sort = sort,
          sortDescending = sortDescending,
          propWithinShow = propWithinShow,
          propWithinUnit = propWithinUnit,
          propWithinValue = propWithinValue,
          prob = prob,
          hideLessThan = hideLessThan,
          hideLessThanCell = hideLessThanCell,
          gaPath = gaPath,
          npcrGroupPrivateOthers = npcrGroupPrivateOthers,
          outputHighcharts = outputHighcharts,
          includeTabs = includeTabs,
          includeMissingColumn = includeMissingColumn
        )

      if (!inca) {
        optionsList <-
          rccShinyCheckData(
            optionsList = optionsList
          )

        if (optionsList$error != "") {
          stop(optionsList$error, call. = FALSE)
        }

        if (!dir.exists(paste0(path, "/", loopLanguage))) {
          dir.create(paste0(path, "/", loopLanguage), showWarnings = FALSE)
        }

        dir.create(paste0(path, "/", loopLanguage, "/", folder), showWarnings = FALSE)
        dir.create(paste0(path, "/", loopLanguage, "/", folder, "/data"), showWarnings = FALSE)

        file.copy(
          system.file("source", "app.R", package = "rccShiny"),
          paste0(path, "/", loopLanguage, "/", folder, "/app.R"),
          overwrite = TRUE
        )

        save(
          optionsList,
          file = paste0(path, "/", loopLanguage, "/", folder, "/data/data.RData")
        )

        whichLanguage <- which(language == loopLanguage)

        tempLinks <-
          cbind(
            tempLinks,
            paste0(
              "<li class='reportLi'><a data-toggle='pill' href='#reportDiv' class='reportLink' id='",
              folder,
              "'>",
              paste(
                folderLinkText[whichLanguage],
                collapse=" / "
                ),
              "</a></li>"
              )
            )
      }

    }

    if (inca) {
      # Attaching packages
      # Needed when on INCA?
      #
      # TODO Investigate further
      # See also
      # https://r-pkgs.org/description.html#dependencies
      # https://r-pkgs.org/namespace.html
      # https://r-pkgs.org/namespace.html#imports
      # https://r-pkgs.org/namespace.html#search-path

      # require("shiny", quietly = TRUE)
      require("shinydashboard", quietly = TRUE)
      require("shinyWidgets", quietly = TRUE)
      require("DT", quietly = TRUE)
      require("sp", quietly = TRUE)
      if (outputHighcharts) require("highcharter", quietly = TRUE)

      rccShinyApp(optionsList = optionsList)
    } else {
      return(invisible(tempLinks))
    }

  }
