#' Shiny apps for RCC
#' @description returns shiny apps that can be used as a complement to the annual reports from the cancer quality registries in Sweden.
#'
#' @param data data frame containing the variables used.
#' @param outcome vector with names(s) of variable(s) in data containing the variable(s) to be presented in the app, for example a quality indicator. Variable(s) must be of type logical, factor or numeric. Default is "outcome".
#' @param outcomeTitle label(s) of the outcome(s) shown in the app. Must be the same length as argument outcome. Default is argument outcome.
#' @param folder name of folder where the results are placed. Default is "ind".
#' @param folderLinkText short name displayed in ready-to-use html link returned by the function. Default is argument outcomeTitle.
#' @param path search path to folder returned by the function. Default is working directory.
#' @param textBeforeSubtitle optional text placed before the subtitles in the tabs.
#' @param textAfterSubtitle optional text placed after the subtitles in the tabs.
#' @param comment optional comment printed under the sidebar panel.
#' @param description description shown in the tab Beskrivning/Description.
#' @param geoUnitsHospital optional name of variable in data containing hospital names. Variable must be of type character. At least one geoUnit must be given. To be implemented: Hospital codes.
#' @param geoUnitsCounty optional name of variable in data containing county codes. Variable must be of type numeric. Can be either county of residence for the patient or the county the hospital belongs to. See details for valid values. At least one geoUnit must be given. To be implemented: Codes for county of hospital are fetched automatically from hospital codes.
#' @param geoUnitsRegion optional name of variable in data containing region codes (1=Stockholm, 2=Uppsala-Örebro, 3=Sydöstra, 4=Södra, 5=Västra, 6=Norra). Variable must be of type numeric. Can be either region of residence for the patient or the region the hospital belongs to. At least one geoUnit must be given. To be implemented: Codes for region of hospital are fetched automatically from hospital codes.
#' @param geoUnitsPatient if geoUnitsCounty/geoUnitsRegion is county/region of residence for the patient (LKF). If FALSE and a hospital is chosen by the user in the sidebar panel the output is highlighted for the respective county/region that the hospital belongs to. Default is FALSE.
#' @param regionSelection adds a widget to the sidebar panel with the option to show only one region at a time. Default is TRUE.
#' @param regionLabel if regionSelection = TRUE label of widget shown in the sidebar panel. Default is c("Begränsa till region", "Limit to region").
#' @param period name of variable in data containing time periods, for example year of diagnosis. Variable must be of type numeric. Default is "period".
#' @param periodLabel label for the period widget in the sidebar panal. Default is c("Diagnosår", "Year of diagnosis").
#' @param varOther optional list of variable(s), other than period and geoUnits, to be shown in the sidebar panel. Arguments to the list are: var (name of variable in data), label (label shown over widget in sidebar panel), choices (which values of var should be shown, min, max for continuous variables).
#' @param targetValues optional vector of 1-2 target levels to be plotted in the tab Jämförelse/Comparison. Only applicable for dichotomous variables.
#' @param funnelplot adds a widget to the sidebar panel with the option to show a funnel plot in the tab Jämförelse/Comparison. Only applicaple for dichotomous variables. Default is FALSE.
#' @param sortDescending should the bars in tab Jämförelse/Comparison be plotted in descending order. Default is TRUE.
#' @param hideLessThan value under which groups (cells) are supressed. Default is 5 and all values < 5 are set to 5.
#' @param language vector giving the language for the app. Possible values are "sv" and "en". Default is "sv". See details.
#' @param npcrGroupPrivateOthers should private hospitals be grouped when displaying data for the entire country. Applicable for NPCR. Default is TRUE.
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
#' -\tab 91,92,93,94,95,96 \tab  Övriga/privata - region
#' }
#'
#'
#'
#' If language = c("sv", "en") the following applies to arguments: textBeforeSubtitle, textAfterSubtitle, comment, description,
#' regionLabel, label in list varOther: if there are two values the first is used in the Swedish version and the second in the English version. If there is only one value this is recycled in both versions.
#' The following applies to argument outcomeTitle: the titles should be given in a list, the first listargument is used in the Swedish version and the second in the English version. The Swedish title(s) will be recycled if English is missing.
#' The following applies to arguments outcome, geoUnitsHospital, geoUnitsCounty, geoUnitsRegion, period, var in list varOther: in the English version the variable name with the suffix _en (for example "outcome_en") will be used if this exists and otherwise the Swedish variable name will be recycled.
#'
#' @author Fredrik Sandin, RCC Uppsala-Örebro
#'
#' @return A folder path/apps/sv|en/folder containing: global.R, server.R, ui.R, data/data.RData, docs/description.html.
#' @examples
#' rccShiny(
#'   data = rccShinyData,
#'   folder = "Indikator1",
#'   outcome = paste0("outcome",1:3),
#'   outcomeTitle = c("Dikotom", "Kontinuerlig", "Kategorisk"),
#'   comment = "Skovde och Lidkoping tillhor Skaraborg",
#'   description = "Att tanka pa vid tolkning ....",
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
#' }
#'
#'# For Swedish/English version
#' rccShinyData$outcome1_en <- rccShinyData$outcome1
#' rccShiny(
#'   data = rccShinyData,
#'   folder = "Indikator2",
#'   outcome = "outcome1",
#'   outcomeTitle = list("Kontaktsjukskoterska", "Contact nurse"),
#'   textBeforeSubtitle = c("Nagot pa svenska","Something in English"),
#'   description = c("Superbra att ha!","Supergood to have!"),
#'   varOther = list(
#'     list(
#'       var = "age",
#'      label = c("Alder vid diagnos","Age at diagnosis"),
#'       choices = c(0,120)
#'     )
#'   ),
#'   targetValues = c(95,99),
#'   language = c("sv", "en")
#' )
#'
#' @export

rccShiny <- function(data = NULL,
                     outcome = "outcome",
                     outcomeTitle = outcome,
                     folder = "ind",
                     folderLinkText = outcomeTitle,
                     path = getwd(),
                     textBeforeSubtitle = NULL,
                     textAfterSubtitle = NULL,
                     comment = "",
                     description = c("(beskrivning saknas)", "(description missing)"),
                     geoUnitsHospital = "sjukhus",
                     geoUnitsCounty = "landsting",
                     geoUnitsRegion = "region",
                     geoUnitsPatient = FALSE,
                     regionSelection = TRUE,
                     regionLabel = c("Begränsa till region", "Limit to region"),
                     period = "period",
                     periodLabel = c("Diagnosår", "Year of diagnosis"),
                     varOther = NULL,
                     targetValues = NULL,
                     funnelplot = FALSE,
                     sortDescending = NULL,
                     hideLessThan = 5,
                     language = c("sv"),
                     npcrGroupPrivateOthers = TRUE) {

    # # # # # # # # # # # # # # # # Lägg till felkontroller!  # # # # # # # # # # # # # # #

    if (is.null(data) | !is.data.frame(data))
        stop("'data' has to be a data.frame", call. = FALSE)

    testVariableError <- function(var) {
        if (is.null(get(var)))
            stop(paste0("'", var, "' is missing"), call. = FALSE)
        if (any(is.na(get(var))))
            stop(paste0("'", var, "' is missing"), call. = FALSE)
        if (is.list(get(var))) {
            tempList <- get(var)
            for (i in 1:length(tempList)) {
                if (!is.character(tempList[[i]]))
                  stop(paste0("'", var, "' has to be of type character"), call. = FALSE)
            }
        } else {
            if (!is.character(get(var)))
                stop(paste0("'", var, "' has to be of type character"), call. = FALSE)
        }
    }
    testVariableError("outcome")
    testVariableError("outcomeTitle")
    testVariableError("folder")
    testVariableError("folderLinkText")

    if (is.null(comment))
        comment <- ""

    if (is.null(period))
        period <- "period"
    if (is.null(outcome))
        outcome <- "outcome"

    # Check for region variable in data
    if (is.null(geoUnitsRegion))
        geoUnitsRegion <- "region"
    if (!(geoUnitsRegion %in% colnames(data)))
        stop(paste0("Column '", geoUnitsRegion, "' not found in 'data'"), call. = FALSE)
    data$regionCode <- suppressWarnings(as.numeric(as.character(data[, geoUnitsRegion])))
    if (any(is.na(data$regionCode)) | !(all(data$regionCode %in% 1:6)))
        stop(paste0("'", geoUnitsRegion, "' contains missing or invalid values. '", geoUnitsRegion, "' should only contain the values (", paste(1:6, collapse = ", "), ")."),
            call. = FALSE)

    if (is.null(geoUnitsCounty))
        geoUnitsCounty <- "landsting"
    if (!(geoUnitsCounty %in% colnames(data)))
        stop(paste0("Column '", geoUnitsCounty, "' not found in 'data'"), call. = FALSE)
    data$landstingCode <- suppressWarnings(as.numeric(as.character(data[, geoUnitsCounty])))
    if (any(is.na(data$landstingCode)) | !(all(data$landstingCode %in% rccShinyCounties(lkf = geoUnitsPatient)$landstingCode)))
        stop(paste0("'", geoUnitsCounty, "' contains missing or invalid values. When 'geoUnitsPatient'=", geoUnitsPatient, ", '", geoUnitsCounty, "' should only contain the values (",
            paste(rccShinyCounties(lkf = geoUnitsPatient)$landstingCode, collapse = ", "), ")."), call. = FALSE)





















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
            if (period %in% colnames(data)) {
                if (paste0(outcome[i], "_", loop_language) %in% colnames(data))
                  data[, outcome[i]] <- data[, paste0(outcome[i], "_", loop_language)]
            } else {
                stop(paste0("Column '", outcome[i], "' not found in 'data'"), call. = FALSE)
            }
        }

        # Add region names
        data$region <- factor(data$region, levels = 1:6, labels = rccShinyRegionNames(language = loop_language))

        # Add county names
        data <- data[, colnames(data) != "landsting"]
        data <- merge(data, rccShinyCounties(language = loop_language, lkf = geoUnitsPatient), by = "landstingCode", all.x = TRUE)

        # Check for hospital variable in data
        if (is.null(geoUnitsHospital))
            geoUnitsHospital <- "sjukhus"
        if (!(geoUnitsHospital %in% colnames(data)))
            stop(paste0("Column '", geoUnitsHospital, "' not found in 'data'"), call. = FALSE)
        data$sjukhus <- if (paste0(geoUnitsHospital, "_", loop_language) %in% colnames(data)) {
            data[, paste0(geoUnitsHospital, "_", loop_language)]
        } else {
            data[, geoUnitsHospital]
        }
        # Fix missing in hospital variable
        data$sjukhus[is.na(data$sjukhus) | data$sjukhus == ""] <- rccShinyTXT(language = GLOBAL_language)$missing

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
                  GLOBAL_varOther[[i]]$choices <- GLOBAL_varOther[[i]]$choices[[ifelse(length(GLOBAL_varOther[[i]]$choices) >= which_language, which_language,
                    1)]]
                }

                if (is.null(GLOBAL_varOther[[i]]$selected))
                  GLOBAL_varOther[[i]]$selected <- GLOBAL_varOther[[i]]$choices
                if (is.list(GLOBAL_varOther[[i]]$selected)) {
                  GLOBAL_varOther[[i]]$selected <- GLOBAL_varOther[[i]]$selected[[ifelse(length(GLOBAL_varOther[[i]]$selected) >= which_language, which_language,
                    1)]]
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

        GLOBAL_outcomeTitle <- if (is.list(outcomeTitle) & length(outcomeTitle) >= which_language) {
            outcomeTitle[[which_language]]
        } else {
            outcomeTitle
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

        GLOBAL_comment <- ifelse(length(comment) >= which_language, comment[which_language], comment[1])

        GLOBAL_description <- ifelse(length(description) >= which_language, description[which_language], description[1])

        GLOBAL_periodLabel <- ifelse(length(periodLabel) >= which_language, periodLabel[which_language], periodLabel[1])
        GLOBAL_periodStart <- min(data$period, na.rm = TRUE)
        GLOBAL_periodEnd <- max(data$period, na.rm = TRUE)

        GLOBAL_geoUnitsPatient <- geoUnitsPatient

        GLOBAL_regionSelection <- regionSelection
        GLOBAL_regionLabel <- ifelse(length(regionLabel) >= which_language, regionLabel[which_language], regionLabel[1])
        GLOBAL_regionChoices <- levels(factor(data$region))
        GLOBAL_regionSelected <- rccShinyTXT(language = GLOBAL_language)$all

        GLOBAL_targetValues <- targetValues
        GLOBAL_funnelplot <- funnelplot
        GLOBAL_sortDescending <- sortDescending

        GLOBAL_hideLessThan <- ifelse(hideLessThan < 5, 5, hideLessThan)

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

        save(GLOBAL_data, GLOBAL_outcome, GLOBAL_outcomeTitle, GLOBAL_outcomeClass, GLOBAL_textBeforeSubtitle, GLOBAL_textAfterSubtitle, GLOBAL_comment, GLOBAL_description,
            GLOBAL_periodLabel, GLOBAL_periodStart, GLOBAL_periodEnd, GLOBAL_geoUnitsPatient, GLOBAL_regionSelection, GLOBAL_regionLabel, GLOBAL_regionChoices, GLOBAL_regionSelected,
            GLOBAL_targetValues, GLOBAL_funnelplot, GLOBAL_sortDescending, GLOBAL_varOther, GLOBAL_hideLessThan, GLOBAL_language, GLOBAL_npcrGroupPrivateOthers,
            file = paste0(path,"/apps/", loop_language, "/", folder, "/data/data.RData"))

        # Output description to .html-file

        printRow <- function(row = "", file = globalOutFile, append = TRUE) {
            cat(paste0(row, "\n"), file = file, append = append)
        }

        globalOutFile <<- file(paste0(path,"/apps/", loop_language, "/", folder, "/docs/description.html"), "w", encoding = "UTF-8")
        printRow(paste0("<!DOCTYPE html>"), append = FALSE)
        printRow(paste0("<html>"))
        printRow(paste0("<body>"))
        for (i in 1:length(GLOBAL_outcome)) {
            printRow(paste0("<h4>", GLOBAL_outcomeTitle[i], "</h4>"))
        }
        if (!is.null(GLOBAL_description)) {
            printRow(paste0("<p>", rccShinyTXT(language = GLOBAL_language)$description, "</p>"))
            printRow(paste0("<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>"))
            printRow(paste0(GLOBAL_description))
            printRow(paste0("</div>"))
        }
        printRow(paste0("</body>"))
        printRow(paste0("</html>"))
        close(globalOutFile)

        tempLinks <- cbind(tempLinks, paste0("<li class='reportLi'><a data-toggle='pill' href='#reportDiv' class='reportLink' id='", folder, "'>", ifelse(length(folderLinkText) >=
            which_language, folderLinkText[which_language], folderLinkText[1]), "</a></li>"))

    }

    return(invisible(tempLinks))
}
