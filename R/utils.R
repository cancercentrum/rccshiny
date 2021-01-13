#' Sets encoding to utf8 for data
#' @description internal function used by rccShiny to "clean" data to utf8 encoding.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
fixEncoding <-
  function(data) {
    numCols <- ncol(data)
    for (col in 1:numCols) {
      if (class(data[,col]) == "character") {
        data[,col] <- enc2utf8(data[,col])
      } else if (class(data[,col]) == "factor") {
        levels(data[,col]) <- enc2utf8(levels(data[,col]))
      }
    }
    data
  }
#' Number of decimals
#' @description internal function used by rccShinyIndTable to give number of decimals.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyDecimals <-
  function() {
    return(1)
  }
#' County dataset
#' @description internal function that creates dataset with numeric codes (given by user) and corresponding names (printed in output) for county/county of residence.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyCounties <-
  function(
    language = "sv",
    lkf = FALSE
  ) {
    if (lkf) {
      data.frame(
        landstingCode = c(1, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 17, 18, 19, 20, 21, 22, 23, 24, 25, NA),
        landsting = c(
          "Stockholm",
          "Uppsala",
          "Södermanland",
          "Östergötland",
          "Jönköping",
          "Kronoberg",
          "Kalmar",
          "Gotland",
          "Blekinge",
          "Skåne",
          "Halland",
          "Västra Götaland",
          "Värmland",
          "Örebro",
          "Västmanland",
          "Dalarna",
          "Gävleborg",
          "Västernorrland",
          "Jämtland",
          "Västerbotten",
          "Norrbotten",
          if (language == "en") {
            "Missing"
          } else {
            "Uppgift saknas"
          }
        ),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        landstingCode = c(10, 11, 12, 13, 21, 22, 23, 24, 25, 26, 27, 28, 30, 41, 42, 50, 51, 52, 53, 54, 55, 56, 57, 61, 62, 63, 64, 65, 91, 92, 93, 94, 95, 96, NA),
        landsting = c(
          "Stockholm",
          "Stockholm",
          "Uppsala",
          "Södermanland",
          "Östergötland",
          "Jönköping",
          "Kronoberg",
          "Kalmar",
          "Kalmar",
          "Gotland",
          "Blekinge",
          "Skåne",
          "Skåne",
          "Skåne",
          "Halland",
          "Västra Götaland",
          "Västra Götaland",
          "Västra Götaland",
          "Västra Götaland",
          "Värmland",
          "Örebro",
          "Västmanland",
          "Dalarna",
          "Gävleborg",
          "Västernorrland",
          "Jämtland",
          "Västerbotten",
          "Norrbotten",
          paste0(
            if (language == "en") {
              "Others/private - "
            } else {
              "Övriga/privat - "
            },
            rccShinyRegionNames(language = language)[1:6]
          ),
          if (language == "en") {
            "Missing"
          } else {
            "Uppgift saknas"
          }
        ),
        stringsAsFactors = FALSE
      )
    }
  }
#' Region variable
#' @description internal function that creates variable from numeric codes (given by user) to corresponding names (printed in output) for region.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyRegionNumToText <-
  function(
    var = NULL,
    language = "sv"
  ) {
    var <-
      factor(
        var,
        levels = c(1:6, NA),
        labels = rccShinyRegionNames(language = language),
        exclude = NULL
      )
    as.character(var)
  }
#' Region names
#' @description internal function that gives names of regions used by rccShinyRegionNumToText.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyRegionNames <-
  function(
    all = FALSE,
    sort = FALSE,
    language = "sv"
  ) {
    if (language == "en") {
      if (all) {
        regions <- "All"
      } else {
        regions <- c(
          "Stockholm-Gotland",
          "Mid Sweden",
          "Southeast",
          "South",
          "West",
          "North",
          rccShinyTXT(language = "en")$missing
        )
      }
    } else {
      if (all) {
        regions <- "Alla"
      } else {
        regions <- c(
          "Stockholm-Gotland",
          "Mellansverige",
          "Sydöstra",
          "Södra",
          "Västra",
          "Norra",
          rccShinyTXT(language = "sv")$missing
        )
      }
    }
    if (sort) {
      regions <- sort(regions)
    }
    regions <- enc2utf8(regions)
    return(regions)
  }
#' Names for choices in Level of comparison
#' @description internal function used to supply text printed in Level of comparison widget.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyLevelNames <-
  function(
    level = "region",
    language = "sv"
  ) {
    if (level == "region") {
      levelName <- if (language == "en") {
        "Healthcare region"
      } else {
        "Sjukvårdsregion"
      }
    } else if (level == "county_lkf") {
      levelName <- if (language == "en") {
        "County of residence"
      } else {
        "Bostadslän"
      }
    } else if (level == "county") {
      levelName <- if (language == "en") {
        "Region"
      } else {
        "Region"
      }
    } else if (level == "hospital") {
      levelName <- if (language == "en") {
        "Hospital"
      } else {
        "Sjukhus"
      }
    } else {
      levelName <- if (language == "en") {
        "Unknown"
      } else {
        "Okänd"
      }
    }
    return(levelName)
  }
#' Group together geoUnits
#' @description internal function used by server.R to simplify code.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyGroupVariable <-
  function(
    label = "sjukhus",
    otherVariables = NULL,
    otherLabels = NULL
  ) {
    if (tolower(label) %in% c("sjukvårdsregion", "healthcare region")) {
      "region"
    } else if (tolower(label) %in% c("region", "county", "bostadslän", "county of residence")) {
      "landsting"
    } else if (tolower(label) %in% c("sjukhus", "hospital")) {
      "sjukhus"
    } else if (label %in% otherLabels) {
      otherVariables[which(otherLabels == label)]
    } else {
      "sjukhus"
    }
  }
#' Text for no observations
#' @description internal function with text to be printed if there is no data available.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyNoObservationsText <-
  function(language = "sv") {
    if (language == "en") {
      return("Not enough observations available for current selection criteria")
    } else {
      return("Ej tillräckligt med observationer för aktuellt urval")
    }
  }
#' Names for tabs
#' @description internal function used to supply names of the tabs.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyTabsNames <-
  function(language = "sv") {
    tab <-
      data.frame(
        tab_language = c("sv", "en"),
        fig_compare = c("Jämförelse", "Comparison"),
        fig_trend = c("Trend", "Trend"),
        tab_n = c("Tabell (antal)", "Table (number)"),
        tab_p = c("Tabell (andel)", "Table (proportion)"),
        tab = c("Tabell", "Table"),
        map = c("Karta", "Map"),
        list = c("Lista", "List"),
        description = c("Beskrivning", "Description"),
        stringsAsFactors = FALSE
      )
    subset(
      tab,
      tab_language == language
    )
  }
#' Miscellaneous text
#' @description internal function used to supply text for various output.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rccShinyTXT <-
  function(language = "sv") {
    tab <-
      data.frame(
        tab_language = c("sv", "en"),
        all = c("Alla", "All"),
        missing = c("Uppgift saknas", "Missing"),
        outcome = c("Variabel", "Variable"),
        presentation = c("Redovisning", "Presentation"),
        levelofcomparison = c("Jämförelsenivå", "Level of comparison"),
        hospitalinterest = c("Sjukhus av intresse", "Hospital of interest"),
        periodSplit1 = c("Redovisa varje", "Present each"),
        periodSplit2 = c("separat", "separate"),
        funnelplot = c("Funnel plot", "Funnel plot"),
        comment = c("Kommentar", "Comment"),
        description = c("Beskrivning", "Description"),
        numericchoices_prop = c("Andel inom ...", "Proportion within ..."),
        numeric_proportionwithin = c("andel inom ", "proportion within "),
        propWithinUnit = c("dagar", "days"),
        fewcases1 = c("Resultat från grupper med färre än", "Results from groups with fewer than"),
        fewcases2 = c("fall redovisas ej separat", "cases are not shown separately"),
        fewcases1cell = c("Om en enskild cell för en grupp innehåller färre än", "If a single cell for a group contains fewer than"),
        fewcases2cell = c("fall redovisas ej absoluta antal för gruppen", "cases, absolute numbers for that group are not displayed"),
        median = c("Median", "Median"),
        medianiqr = c("Median samt kvartilavstånd", "Median and interquartile range"),
        q1 = c("Första kvartil", "First quartile"),
        q3 = c("Tredje kvartil", "Third quartile"),
        iqr = c("kvartilavstånd", "interquartile range"),
        interquantilerange = c("kvantilavstånd", "interquantile range"),
        iqr_and = c("samt", "and"),
        percentile = c("percentil", "percentile"),
        percent = c("Procent", "Percent"),
        noofcases = c("Antal fall", "No. of cases"),
        noofcases_nOfN = c("av", "of"),
        message = c("Meddelande", "Message"),
        numerator = c("Täljare", "Numerator"),
        denominator = c("Nämnare", "Denominator"),
        total = c("Totalt", "Total"),
        period = c("Period", "Period"),
        periodTypeInputLabelQuarter = c("Kvartal", "Quarter"),
        periodTypeInputLabelYear = c("År", "Year"),
        RIKET = c("RIKET", "SWEDEN"),
        dxYear = c("Diagnosår", "Year of diagnosis"),
        limitRegion = c("Begränsa till sjukvårdsregion", "Limit to healthcare region"),
        descriptionAbout = c("Om indikatorn", "About"),
        descriptionInterpretation = c("Att tänka på vid tolkning", "Interpretation"),
        descriptionTechnical = c("Teknisk beskrivning", "Technical description"),
        targetValuesLabelIntermediate = c("Mellannivå av måluppfyllelse", "Intermediate level of performance"),
        targetValuesLabelHigh = c("Hög nivå av måluppfyllelse", "High level of performance"),
        yes = c("Ja", "Yes"),
        no = c("Nej", "No"),
        idOverviewLink = c("Översikt", "Overview"),
        grouphidelessthan = c("(otillräcklig data)", "(insufficient data)"),
        selectAll = c("Markera alla", "Select all"),
        deselectAll = c("Avmarkera alla", "Deselect all"),
        stringsAsFactors = FALSE
      )
    tab <- fixEncoding(tab)
    tempTab <- data.frame()
    for (i in language) {
      tempTab <-
        rbind(
          tempTab,
          if (i %in% tab$tab_language) {
            subset(tab, tab_language == i)
          } else {
            subset(tab, tab_language == "sv")
          }
        )
    }
    tempTab
  }

# # # # # # NPCR # # # # #
#' Used by NPCR
#' @description internal function.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
npcrPreparePeriodRegionCountyHospitalVariables <-
  function(
    prefix = "d0",
    data = NULL,
    language = c("sv", "en"),
    returnPrivateOthersNames = FALSE
  ) {
  if (returnPrivateOthersNames) {
    privateOthersNames <- list()
    for (i in 1:length(language)) {
      tempNames <- data.frame(landsting = if (language[i] == "sv") {
        "Övriga/privat"
      } else {
        "Others/private"
      }, sjukhus_privat = if (language[i] == "sv") {
        "Privat vårdgivare"
      } else {
        "Private practice"
      }, sjukhus_ovriga = if (language[i] == "sv") {
        "Övriga"
      } else {
        "Others"
      }, sjukhus_privatovriga = if (language[i] == "sv") {
        "Privat vårdgivare/övriga"
      } else {
        "Private practice/others"
      }, stringsAsFactors = FALSE)

      privateOthersNames[[i]] <- tempNames
      names(privateOthersNames)[i] <- language[i]
    }
    privateOthersNames
  } else {
    if (prefix == "rp0") {
      data$period <- data$rp0_year
    } else if (prefix == "s0") {
      data$period <- data$s0_year
    } else {
      data$period <- data$d0_year
    }
    data$region <- rccShinyRegionNumToText(var = data[, paste0(prefix, "_region")])
    data$landsting = substr(data[, paste0(prefix, "_landstingtxt")], 4, 100)
    data$sjukhus = data[, paste0(prefix, "_sjukhustxt")]

    for (loop_language in language[language != "sv"]) {
      data[, paste0("region_", loop_language)] <- rccShinyRegionNumToText(language = loop_language, var = data[, paste0(prefix, "_region")])

      data[, paste0("landsting_", loop_language)] <- data$landsting
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Stockholm-Gotland"] <- "Others/private - Stockholm-Gotland"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Mid Sweden"] <- "Others/private - Mid Sweden"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Sydöstra"] <- "Others/private - Southeast"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Södra"] <- "Others/private - South"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Västra"] <- "Others/private - West"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "Övriga/privat - Norra"] <- "Others/private - North"

      data[, paste0("sjukhus_", loop_language)] <- data$sjukhus
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Stockholm-Gotland"] <- "Private practice - Stockholm-Gotland"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Mellansverige"] <- "Private practice - Mellansverige"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Sydöstra"] <- "Private practice - Southeast"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Södra"] <- "Private practice - South"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Västra"] <- "Private practice - West"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat vårdgivare - Norra"] <- "Private practice - North"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Övriga"] <- "Others"
    }
    data
  }
}
