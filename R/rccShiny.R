#' Shiny apps for RCC
#' @description returns shiny apps that can be used as a complement to the annual reports from the cancer quality registries in Sweden.
#'
#' @param language vector giving the language for the app. Possible values are "sv" and "en". Default is "sv". See details.
#' @param data data frame containing the variables used.
#' @param outcome vector with names(s) of variable(s) in data containing the variable(s) to be presented in the app, for example a quality indicator. Variable(s) must be of type logical, factor or numeric. Default is "outcome". Observe that observations with missing values for outcome are not included in the output.
#' @param outcomeNumericExcludeNeg should negative values be excluded when presenting a numeric outcome? Particularly relevant for waiting times. Default is TRUE.
#' @param outcomeTitle label(s) of the outcome(s) shown in the app. Must be the same length as argument outcome. Default is argument outcome.
#' @param folder name of folder where the results are placed. Default is "ind".
#' @param folderLinkText name displayed in ready-to-use html link returned by the function. Default is NULL, which results in the use of arguments outcomeTitle, folder and language to construct a name depending on the number of outcomes.
#' @param path search path to folder returned by the function. Default is working directory.
#' @param textBeforeSubtitle optional text placed before the subtitles in the tabs.
#' @param textAfterSubtitle optional text placed after the subtitles in the tabs.
#' @param comment optional comment printed under the sidebar panel.
#' @param description vector of 3 character strings, or a list of vectors, one for each language, shown in the three subsections in the tab Beskrivning/Description. Default is c("...", "...", "...").
#' @param geoUnitsHospital optional name of variable in data containing hospital names. Variable must be of type character. If NULL or if "sjukhus" is not found in 'data', hospital is not available as a level of presentation. At least one geoUnit must be given. To be implemented: Hospital codes.
#' @param geoUnitsCounty optional name of variable in data containing county codes. Variable must be of type numeric. Can be either county of residence for the patient or the county the hospital belongs to. See details for valid values. If NULL or if "landsting" is not found in 'data', county is not available as a level of presentation. At least one geoUnit must be given. To be implemented: Codes for county of hospital are fetched automatically from hospital codes.
#' @param geoUnitsRegion optional name of variable in data containing region codes (1=Stockholm, 2=Uppsala-Örebro, 3=Sydöstra, 4=Södra, 5=Västra, 6=Norra, NA=Uppgift saknas). Variable must be of type numeric. Can be either region of residence for the patient or the region the hospital belongs to. If NULL or if "region" is not found in 'data', region is not available as a level of presentation. At least one geoUnit must be given. To be implemented: Codes for region of hospital are fetched automatically from hospital codes.
#' @param geoUnitsPatient if geoUnitsCounty/geoUnitsRegion is county/region of residence for the patient (LKF). If FALSE and a hospital is chosen by the user in the sidebar panel the output is highlighted for the respective county/region that the hospital belongs to. Default is FALSE.
#' @param regionSelection adds a widget to the sidebar panel with the option to show only one region at a time. Default is TRUE.
#' @param regionLabel if regionSelection = TRUE label of widget shown in the sidebar panel. Default is "Begränsa till region", "Limit to region" depending on language.
#' @param period name of variable in data containing time periods, for example year of diagnosis. Variable must be of type numeric. Default is "period".
#' @param periodLabel label for the period widget in the sidebar panal. Default is "Diagnosår", "Year of diagnosis" depending on language.
#' @param varOther optional list of variable(s), other than period and geoUnits, to be shown in the sidebar panel. Arguments to the list are: var (name of variable in data), label (label shown over widget in sidebar panel), choices (which values of var should be shown, min, max for continuous variables). Observe that observations with missing values for varOthers are not included in the output.
#' @param targetValues optional vector or list of vectors (one for each outcome) with 1-2 target levels to be plotted in the tabs Jämförelse/Comparison and Trend for outcomes of type logical or numeric. If the outcome is numeric the target levels are shown when "Andel inom..."/"Proportion within..." is selected, and then only for the default propWithinValue.
#' @param funnelplot adds a widget to the sidebar panel with the option to show a funnel plot in the tab Jämförelse/Comparison. Only applicaple for dichotomous variables. Default is FALSE.
#' @param sortDescending should the bars in tab Jämförelse/Comparison be plotted in descending order? The argument could have the same length as argument outcome, giving different values for each outcome. Default is NULL, which sorts logical outcomes in descending order and continuous outcomes in ascending order.
#' @param propWithinShow display the choice "Andel inom..."/"Proportion within..." for numeric outcome(s). Default is TRUE.
#' @param propWithinUnit unit shown for numeric outcome when "Andel inom..."/"Proportion within..." is selected. Default is "dagar", "days" depending on language.
#' @param propWithinValue vector with default value(s) shown for numeric outcome(s) when "Andel inom..."/"Proportion within..." is selected. If the length of the vector is less than the number of numeric outcomes the values are recycled. Default is 30.
#' @param hideLessThan value under which groups (cells) are supressed. Default is 5 and all values < 5 are set to 5.
#' @param showHide To be implemented: Should levels with values < 5 be shown but without values? Default is TRUE.
#' @param gaPath optional path to Google Analytics .js-file. Default is NULL.
#' @param npcrGroupPrivateOthers should private hospitals be grouped when displaying data for the entire country. Applicable for NPCR. Default is FALSE.
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
#' If language = c("sv", "en") the following applies to arguments: textBeforeSubtitle, textAfterSubtitle, comment,
#' regionLabel, label in list varOther: if there are two values the first is used in the Swedish version and the second in the English version. If there is only one value this is recycled in both versions.
#' The following applies to argument outcomeTitle, description: the arguments should be given in a list, the first listargument is used in the Swedish version and the second in the English version. The Swedish title(s) will be recycled if English is missing.
#' The following applies to arguments outcome, geoUnitsHospital, geoUnitsCounty, geoUnitsRegion, period, var in list varOther: in the English version the variable name with the suffix _en (for example "outcome_en") will be used if this exists and otherwise the Swedish variable name will be recycled.
#'
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#'
#' @return A folder path/apps/sv|en/folder containing: global.R, server.R, ui.R, data/data.RData, docs/description.html.
#' @examples
#' ind1 <- rccShiny(
#'   data = rccShinyData,
#'   folder = "Indikator1",
#'   folderLinkText = "Indikator 1",
#'   outcome = paste0("outcome",1:3),
#'   outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
#'   comment = "Skovde och Lidkoping tillhor Skaraborg",
#'   description = c("Har beskrivs indikatorn.","Viktig information!","Information om variabler etc."),
#'   varOther = list(
#'     list(
#'       var = "age",
#'       label = "Alder vid diagnos"
#'     ),
#'     list(
#'       var = "stage",
#'       label = "Stadium",
#'       choices = c("I", "II")
#'     )
#'   ),
#'   funnelplot = TRUE
#' )
#' \dontrun{
#' library(shiny)
#' runApp("./apps/sv/Indikator1")
#'
#' cat(ind1) # displays the html link that can be used in index.html
#' }
#'
#'# For Swedish/English version
#' rccShinyData$outcome1_en <- rccShinyData$outcome1
#' rccShiny(
#'   language = c("sv", "en"),
#'   data = rccShinyData,
#'   folder = "Indikator2",
#'   outcome = "outcome1",
#'   outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
#'   textBeforeSubtitle = c("Nagot pa svenska","Something in English"),
#'   description = list(
#'     c("Har beskrivs indikatorn.","Viktig information!","Information om variabler etc."),
#'     c("Description of the indicator","Important information!","Information on variables etc.")
#'   ),
#'   varOther = list(
#'     list(
#'       var = "age",
#'      label = c("Alder vid diagnos","Age at diagnosis"),
#'       choices = c(0,120)
#'     )
#'   ),
#'   targetValues = c(95,99)
#' )
#' @export

rccShiny <-
  function(
    language = "sv",
    data = NULL,
    outcome = "outcome",
    outcomeNumericExcludeNeg = TRUE,
    outcomeTitle = outcome,
    folder = "ind",
    folderLinkText = NULL,
    path = getwd(),
    textBeforeSubtitle = NULL,
    textAfterSubtitle = NULL,
    comment = "",
    description = rep("...",3),
    geoUnitsHospital = "sjukhus",
    geoUnitsCounty = "landsting",
    geoUnitsRegion = "region",
    geoUnitsPatient = FALSE,
    regionSelection = TRUE,
    regionLabel = rccShinyTXT(language = language)$limitRegion,
    period = "period",
    periodLabel = rccShinyTXT(language = language)$dxYear,
    varOther = NULL,
    targetValues = NULL,
    funnelplot = FALSE,
    sortDescending = NULL,
    propWithinShow = TRUE,
    propWithinUnit = rccShinyTXT(language = language)$propWithinUnit,
    propWithinValue = 30,
    hideLessThan = 5,
    showHide = TRUE,
    gaPath = NULL,
    npcrGroupPrivateOthers = FALSE
  ) {

    # # # # # # # # # # # # # # # #
    # Checking input parameters
    # # # # # # # # # # # # # # # #

    if (is.null(language) | !is.character(language))
      stop("'language' should be a character vector", call. = FALSE)

    if (is.null(data) | !is.data.frame(data))
      stop("'data' should be a data.frame", call. = FALSE)

    testVariableError <- function(var, listAllowed = TRUE) {
      if (is.null(get(var)))
        stop(paste0("'", var, "' is missing"), call. = FALSE)
      if (any(is.na(get(var))))
        stop(paste0("'", var, "' is missing"), call. = FALSE)
      if (is.list(get(var))) {
        if (!listAllowed) {
          stop(paste0("'", var, "' should not be a list"), call. = FALSE)
        } else {
          tempList <- get(var)
          for (i in 1:length(tempList)) {
            if (!is.character(tempList[[i]]))
              stop(paste0("'", var, "' should be of type character"), call. = FALSE)
          }
        }
      } else {
        if (!is.character(get(var)))
          stop(paste0("'", var, "' should be of type character"), call. = FALSE)
      }
    }

    testVariableError("outcome", listAllowed = FALSE)

    if (is.null(outcomeNumericExcludeNeg) | !is.logical(outcomeNumericExcludeNeg) | length(outcomeNumericExcludeNeg) != 1)
      stop(paste0("'outcomeNumericExcludeNeg' should a logical vector of length 1"), call. = FALSE)

    testVariableError("outcomeTitle")
    if (!is.list(outcomeTitle))
      outcomeTitle <- list(outcomeTitle)
    for (i in 1:length(outcomeTitle)) {
      if (length(outcome) != length(outcomeTitle[[i]]))
        stop(paste0("'outcome' and 'outcomeTitle' should have the same number of elements"), call. = FALSE)
    }

    testVariableError("folder", listAllowed = FALSE)
    if (length(folder) != 1)
      stop(paste0("'folder' should be of length 1"), call. = FALSE)

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

    if (is.null(path) | !is.character(path) | length(path) != 1)
      stop("'path' should be a character vector of length 1", call. = FALSE)
    if (!dir.exists(path))
      stop("The folder '",path,"' does not exist", call. = FALSE)

    if (is.null(textBeforeSubtitle))
      textBeforeSubtitle <- ""
    testVariableError("textBeforeSubtitle", listAllowed = FALSE)

    if (is.null(textAfterSubtitle))
      textAfterSubtitle <- ""
    testVariableError("textAfterSubtitle", listAllowed = FALSE)

    if (is.null(comment))
      comment <- ""
    testVariableError("comment", listAllowed = FALSE)

    testVariableError("description")
    if (!is.list(description))
      description <- list(description)

    GLOBAL_geoUnitsHospitalInclude <- TRUE
    if (!is.null(geoUnitsHospital) & (!is.character(geoUnitsHospital) | length(geoUnitsHospital) != 1)) {
      stop("'geoUnitsHospital' should be either NULL or a character vector of length 1", call. = FALSE)
    } else if (is.null(geoUnitsHospital)) {
      GLOBAL_geoUnitsHospitalInclude <- FALSE
      geoUnitsHospital <- "sjukhus"
    } else if (!(geoUnitsHospital %in% colnames(data))) {
      GLOBAL_geoUnitsHospitalInclude <- FALSE
    }

    GLOBAL_geoUnitsCountyInclude <- TRUE
    if (!is.null(geoUnitsCounty) & (!is.character(geoUnitsCounty) | length(geoUnitsCounty) != 1)) {
      stop("'geoUnitsCounty' should be either NULL or a character vector of length 1", call. = FALSE)
    } else if (is.null(geoUnitsCounty)) {
      GLOBAL_geoUnitsCountyInclude <- FALSE
      geoUnitsCounty <- "landsting"
    } else if (!(geoUnitsCounty %in% colnames(data))) {
      GLOBAL_geoUnitsCountyInclude <- FALSE
    } else {
      data$landstingCode <- suppressWarnings(as.numeric(as.character(data[, geoUnitsCounty])))
      if (!(all(data$landstingCode %in% rccShinyCounties(lkf = geoUnitsPatient)$landstingCode)))
        stop(paste0("'", geoUnitsCounty, "' contains invalid values. When 'geoUnitsPatient'=", geoUnitsPatient, ", '", geoUnitsCounty, "' should only contain the values (",
                    paste(rccShinyCounties(lkf = geoUnitsPatient)$landstingCode, collapse = ", "), ")."), call. = FALSE)
    }

    GLOBAL_geoUnitsRegionInclude <- TRUE
    if (!is.null(geoUnitsRegion) & (!is.character(geoUnitsRegion) | length(geoUnitsRegion) != 1)) {
      stop("'geoUnitsRegion' should be either NULL or a character vector of length 1", call. = FALSE)
    } else if (is.null(geoUnitsRegion)) {
      GLOBAL_geoUnitsRegionInclude <- FALSE
      geoUnitsRegion <- "landsting"
    } else if (!(geoUnitsRegion %in% colnames(data))) {
      GLOBAL_geoUnitsRegionInclude <- FALSE
    } else {
      data$regionCode <- suppressWarnings(as.numeric(as.character(data[, geoUnitsRegion])))
      if (!(all(data$regionCode %in% c(1:6, NA))))
        stop(paste0("'", geoUnitsRegion, "' contains invalid values. '", geoUnitsRegion, "' should only contain the values (", paste(c(1:6, NA), collapse = ", "), ")."),
             call. = FALSE)
    }

    if (sum(GLOBAL_geoUnitsHospitalInclude, GLOBAL_geoUnitsCountyInclude, GLOBAL_geoUnitsRegionInclude) < 1)
      stop(paste0("At least one level of comparison (hospital/county/region) must be available"), call. = FALSE)

    if (is.null(geoUnitsPatient) | !is.logical(geoUnitsPatient) | length(geoUnitsPatient) != 1)
      stop(paste0("'geoUnitsPatient' should a logical vector of length 1"), call. = FALSE)

    if (is.null(regionSelection) | !is.logical(regionSelection) | length(regionSelection) != 1)
      stop(paste0("'regionSelection' should a logical vector of length 1"), call. = FALSE)

    testVariableError("regionLabel", listAllowed = FALSE)

    if (is.null(period) | !is.character(period) | length(period) != 1)
      stop("'period' should be a character vector of length 1", call. = FALSE)

    testVariableError("periodLabel", listAllowed = FALSE)

    if (!is.null(varOther) & (!is.list(varOther) | length(varOther) < 1))
      stop("'varOther' should be either NULL or a list with at least one element", call. = FALSE)
    if (!all(sapply(varOther,is.list)))
      stop("The elements of 'varOther' should be lists", call. = FALSE)

    if (!is.list(targetValues))
      targetValues <- list(targetValues)
    targetValuesOld <- targetValues
    targetValues <- rep(list(NULL),length(outcome))
    for (i in 1:length(targetValues)) {
      if (i <= length(targetValuesOld)) {
        if (!is.null(targetValuesOld[[i]]))
          targetValues[[i]] <- targetValuesOld[[i]]
        if (!is.null(targetValues[[i]]) & (!is.numeric(targetValues[[i]]) | length(targetValues[[i]]) < 1 | length(targetValues[[i]]) > 2))
          stop(paste0("'targetValues[[",i,"]]' should be either NULL or a numeric vector of length 1 or 2"), call. = FALSE)
      }
    }

    if (is.null(funnelplot) | !is.logical(funnelplot) | length(funnelplot) != 1)
      stop(paste0("'funnelplot' should be a logical vector of length 1"), call. = FALSE)

    if (!is.null(sortDescending) & (!is.logical(sortDescending) | length(sortDescending) < 1))
      stop(paste0("'sortDescending' should be either NULL or a logical vector with at least one element"), call. = FALSE)
    if (!is.null(sortDescending) & length(sortDescending)<length(outcome))
      sortDescending <- c(
        sortDescending,
        rep(TRUE,length(outcome)-length(sortDescending))
      )

    if (is.null(hideLessThan) | !is.numeric(hideLessThan) | length(hideLessThan) != 1)
      stop("'hideLessThan' should be a numeric vector of length 1", call. = FALSE)

    if (is.null(npcrGroupPrivateOthers) | !is.logical(npcrGroupPrivateOthers) | length(npcrGroupPrivateOthers) != 1)
      stop(paste0("'npcrGroupPrivateOthers' should be a logical vector of length 1"), call. = FALSE)

    if (!is.null(gaPath) & (!is.character(gaPath) | length(gaPath) != 1))
      stop("'gaPath' should be either NULL or a character vector of length 1", call. = FALSE)
    if (!is.null(gaPath))
      gaPath <- ifelse(substr(gaPath,1,1) == "/", gaPath, paste0("/", gaPath))

    if (npcrGroupPrivateOthers & sum(GLOBAL_geoUnitsHospitalInclude, GLOBAL_geoUnitsCountyInclude, GLOBAL_geoUnitsRegionInclude) < 3) {
      npcrGroupPrivateOthers <- FALSE
      warning(paste0("'npcrGroupPrivateOthers' = TRUE can only be used when all levels of comparison (geoUnitsHospital, geoUnitsCounty and geoUnitsRegion) are active. 'npcrGroupPrivateOthers' set to FALSE."), call. = FALSE)
    }

    # # # # # # # # # # # # # # # #

    tempLinks <- vector()

    masterData <- data

    # Save folder for each language

    for (loop_language in language) {

      data <- masterData

      which_language <- which(language == loop_language)

      GLOBAL_language <- loop_language

      # Check for period variable in data
      if (period %in% colnames(data)) {
        data$period <- data[, period]
      } else {
        stop(paste0("Column '", period, "' not found in 'data'"), call. = FALSE)
      }

      # Check for outcome variable(s) in data
      for (i in 1:length(outcome)) {
        if (paste0(outcome[i], "_", loop_language) %in% colnames(data)) {
          data[, outcome[i]] <- data[, paste0(outcome[i], "_", loop_language)]
        } else if (!(outcome[i] %in% colnames(data))) {
          stop(paste0("Column '", outcome[i], "' not found in 'data'"), call. = FALSE)
        }
      }

      # Add region names
      if (GLOBAL_geoUnitsRegionInclude) {
        data$region <- factor(data$regionCode,
                              levels = c(1:6, NA),
                              labels = rccShinyRegionNames(language = loop_language),
                              exclude = NULL)
      } else {
        data$region <- "(not displayed)"
      }

      # Add county names
      if (GLOBAL_geoUnitsCountyInclude) {
        data <- data[, colnames(data) != "landsting"]
        data <- merge(data, rccShinyCounties(language = loop_language, lkf = geoUnitsPatient), by = "landstingCode", all.x = TRUE)
      } else {
        data$landsting <- "(not displayed)"
      }

      # Check for hospital variable in data
      if (GLOBAL_geoUnitsHospitalInclude) {
        data$sjukhus <- if (paste0(geoUnitsHospital, "_", loop_language) %in% colnames(data)) {
          data[, paste0(geoUnitsHospital, "_", loop_language)]
        } else {
          data[, geoUnitsHospital]
        }
        # Fix missing in hospital variable
        data$sjukhus[is.na(data$sjukhus) | data$sjukhus == ""] <- rccShinyTXT(language = GLOBAL_language)$missing
      } else {
        data$sjukhus <- "(not displayed)"
      }

      includeVariables <- c("period", "region", "landsting", "sjukhus")

      # Check for user variable(s) in data
      GLOBAL_varOther <- varOther
      if (!is.null(GLOBAL_varOther)) {
        userInputVariables <- vector()
        for (i in 1:length(GLOBAL_varOther)) {
          if (is.null(GLOBAL_varOther[[i]]$var))
            stop(paste0("'var' is missing from varOther[[", i, "]]"), call. = FALSE)
          temp_var <- GLOBAL_varOther[[i]]$var
          if (!(temp_var %in% colnames(data)))
            stop(paste0("The variable '", temp_var, "' from varOther[[", i, "]] is missing in 'data'"), call. = FALSE)

          if (paste0(temp_var, "_", loop_language) %in% colnames(data))
            data[, temp_var] <- data[, paste0(temp_var, "_", loop_language)]
          userInputVariables <- c(userInputVariables, temp_var)

          if (is.null(GLOBAL_varOther[[i]]$label))
            GLOBAL_varOther[[i]]$label <- temp_var
          GLOBAL_varOther[[i]]$label <- ifelse(length(GLOBAL_varOther[[i]]$label) >= which_language, GLOBAL_varOther[[i]]$label[which_language], GLOBAL_varOther[[i]]$label[1])

          GLOBAL_varOther[[i]]$classNumeric <- class(data[, temp_var]) %in% c("difftime", "numeric", "integer")

          if (is.null(GLOBAL_varOther[[i]]$choices)) {
            if (GLOBAL_varOther[[i]]$classNumeric) {
              GLOBAL_varOther[[i]]$choices <- range(data[, temp_var], na.rm = TRUE)
            } else {
              GLOBAL_varOther[[i]]$choices <- levels(factor(data[, temp_var]))
            }
          }
          if (is.list(GLOBAL_varOther[[i]]$choices)) {
            GLOBAL_varOther[[i]]$choices <- GLOBAL_varOther[[i]]$choices[[ifelse(length(GLOBAL_varOther[[i]]$choices) >= which_language, which_language, 1)]]
          }

          if (is.null(GLOBAL_varOther[[i]]$selected))
            GLOBAL_varOther[[i]]$selected <- GLOBAL_varOther[[i]]$choices
          if (is.list(GLOBAL_varOther[[i]]$selected)) {
            GLOBAL_varOther[[i]]$selected <- GLOBAL_varOther[[i]]$selected[[ifelse(length(GLOBAL_varOther[[i]]$selected) >= which_language, which_language, 1)]]
          }

          if (is.null(GLOBAL_varOther[[i]]$multiple))
            GLOBAL_varOther[[i]]$multiple <- TRUE

          if (is.null(GLOBAL_varOther[[i]]$showInTitle))
            GLOBAL_varOther[[i]]$showInTitle <- TRUE
        }
        includeVariables <- c(includeVariables, userInputVariables)
      }

      includeVariables <- c(includeVariables, outcome)
      GLOBAL_outcome <- outcome
      GLOBAL_outcomeClass <- vector()
      for (i in 1:length(outcome)) {
        GLOBAL_outcomeClass[i] <- class(data[, outcome[i]])
      }

      GLOBAL_data <- subset(data, select = includeVariables)

      GLOBAL_outcomeNumericExcludeNeg <- outcomeNumericExcludeNeg

      GLOBAL_outcomeTitle <-
        if (length(outcomeTitle) >= which_language) {
          outcomeTitle[[which_language]]
        } else {
          outcomeTitle[[1]]
        }

      GLOBAL_folderLinkText <-
        if (length(folderLinkText) >= which_language) {
          folderLinkText[which_language]
        } else {
          folderLinkText[1]
        }

      GLOBAL_textBeforeSubtitle <- if (length(textBeforeSubtitle) >= which_language) {
        textBeforeSubtitle[which_language]
      } else {
        textBeforeSubtitle[1]
      }
      GLOBAL_textAfterSubtitle <- if (length(textAfterSubtitle) >= which_language) {
        textAfterSubtitle[which_language]
      } else {
        textAfterSubtitle[1]
      }

      GLOBAL_comment <-
        enc2utf8(
          ifelse(
            length(comment) >= which_language,
            comment[which_language],
            comment[1]
          )
        )

      GLOBAL_description <-
        if(length(description) >= which_language) {
          description[[which_language]]
        } else {
          description[[1]]
        }
      if (length(GLOBAL_description)<3)
        GLOBAL_description <- c(
          GLOBAL_description,
          rep("...",3-length(GLOBAL_description))
        )

      GLOBAL_periodLabel <- ifelse(length(periodLabel) >= which_language, periodLabel[which_language], periodLabel[1])
      GLOBAL_periodStart <- min(data$period, na.rm = TRUE)
      GLOBAL_periodEnd <- max(data$period, na.rm = TRUE)

      GLOBAL_geoUnitsPatient <- geoUnitsPatient

      GLOBAL_regionSelection <- regionSelection
      GLOBAL_regionLabel <- ifelse(length(regionLabel) >= which_language, regionLabel[which_language], regionLabel[1])
      GLOBAL_regionChoices <- levels(factor(data$region))[!levels(factor(data$region)) %in% rccShinyTXT(language = GLOBAL_language)$missing]
      GLOBAL_regionSelected <- rccShinyTXT(language = GLOBAL_language)$all

      GLOBAL_targetValues <- targetValues
      GLOBAL_funnelplot <- funnelplot
      GLOBAL_sortDescending <- sortDescending

      GLOBAL_propWithinShow <- propWithinShow
      GLOBAL_propWithinUnit <- ifelse(length(propWithinUnit) >= which_language, propWithinUnit[which_language], propWithinUnit[1])
      GLOBAL_propWithinValue <- rep(NA, length(outcome))
      GLOBAL_propWithinValue[GLOBAL_outcomeClass == "numeric"] <-  rep(propWithinValue,
                                                                       length.out = sum(GLOBAL_outcomeClass == "numeric")
                                                                       )

      GLOBAL_hideLessThan <- ifelse(hideLessThan < 5, 5, hideLessThan)

      GLOBAL_gaPath <- gaPath

      GLOBAL_npcrGroupPrivateOthers <- npcrGroupPrivateOthers

      if (!dir.exists(paste0(path,"/apps/"))) {
        dir.create(paste0(path,"/apps/"), showWarnings = FALSE)
      }
      if (!dir.exists(paste0(path,"/apps/", loop_language))) {
        dir.create(paste0(path,"/apps/", loop_language), showWarnings = FALSE)
      }

      dir.create(paste0(path, "/apps/", loop_language, "/", folder), showWarnings = FALSE)
      dir.create(paste0(path, "/apps/", loop_language, "/", folder, "/data"), showWarnings = FALSE)
      dir.create(paste0(path, "/apps/", loop_language, "/", folder, "/docs"), showWarnings = FALSE)

      file.copy(system.file("source", "global.R", package = "rccShiny"), paste0(path, "/apps/", loop_language, "/", folder, "/global.R"), overwrite = TRUE)
      file.copy(system.file("source", "server.R", package = "rccShiny"), paste0(path, "/apps/", loop_language, "/", folder, "/server.R"), overwrite = TRUE)
      file.copy(system.file("source", "ui.R", package = "rccShiny"), paste0(path, "/apps/", loop_language, "/", folder, "/ui.R"), overwrite = TRUE)

      GLOBAL_data <- fixEncoding(GLOBAL_data)

      save(GLOBAL_data, GLOBAL_outcome, GLOBAL_outcomeNumericExcludeNeg, GLOBAL_outcomeTitle, GLOBAL_outcomeClass, GLOBAL_textBeforeSubtitle, GLOBAL_textAfterSubtitle, GLOBAL_comment, GLOBAL_description,
           GLOBAL_periodLabel, GLOBAL_periodStart, GLOBAL_periodEnd, GLOBAL_geoUnitsHospitalInclude, GLOBAL_geoUnitsCountyInclude, GLOBAL_geoUnitsRegionInclude, GLOBAL_geoUnitsPatient,
           GLOBAL_regionSelection, GLOBAL_regionLabel, GLOBAL_regionChoices, GLOBAL_regionSelected, GLOBAL_targetValues, GLOBAL_funnelplot, GLOBAL_sortDescending,
           GLOBAL_propWithinShow, GLOBAL_propWithinUnit, GLOBAL_propWithinValue, GLOBAL_varOther, GLOBAL_hideLessThan, GLOBAL_language, GLOBAL_gaPath, GLOBAL_npcrGroupPrivateOthers,
           file = paste0(path,"/apps/", loop_language, "/", folder, "/data/data.RData"))

      # Output description to .html-file

      printRow <- function(row = "", file = globalOutFile, append = TRUE) {
        cat(paste0(row, "\n"), file = file, append = append)
      }

      globalOutFile <<- file(paste0(path,"/apps/", loop_language, "/", folder, "/docs/description.html"), "w", encoding = "UTF-8")
      printRow(paste0("<!DOCTYPE html>"), append = FALSE)
      printRow(paste0("<html>"))
      printRow(paste0("<body>"))
      printRow(paste0("<p></p>"))
      printRow(paste0("<p>", rccShinyTXT(language = GLOBAL_language)$descriptionAbout, "</p>"))
      printRow(paste0("<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>"))
      printRow(paste0(GLOBAL_description[1]))
      printRow(paste0("</div>"))
      printRow(paste0("<p>", rccShinyTXT(language = GLOBAL_language)$descriptionInterpretation, "</p>"))
      printRow(paste0("<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>"))
      printRow(paste0(GLOBAL_description[2]))
      printRow(paste0("<p></p>"))
      printRow(paste0(rccShinyTXT(language = GLOBAL_language)$fewcases1,
                      " ",
                      GLOBAL_hideLessThan,
                      " ",
                      rccShinyTXT(language = GLOBAL_language)$fewcases2,
                      "."
      ))
      printRow(paste0("</div>"))
      printRow(paste0("<p>", rccShinyTXT(language = GLOBAL_language)$descriptionTechnical, "</p>"))
      printRow(paste0("<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>"))
      printRow(paste0(GLOBAL_description[3]))
      printRow(paste0("</div>"))
      printRow(paste0("</body>"))
      printRow(paste0("</html>"))
      close(globalOutFile)

      tempLinks <- cbind(tempLinks, paste0("<li class='reportLi'><a data-toggle='pill' href='#reportDiv' class='reportLink' id='", folder, "'>", paste(GLOBAL_folderLinkText,collapse=" / "), "</a></li>"))

    }

    return(invisible(tempLinks))
  }
