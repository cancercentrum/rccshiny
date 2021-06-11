#' Sets encoding to utf8 for data
#' @description internal function used by rccShiny to "clean" data to utf8 encoding.
#' @author Fredrik Sandin, RCC Mellansverige
#' @noRd
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
#' @noRd
rccShinyDecimals <-
  function() {
    return(1)
  }

#' County dataset
#' @description internal function that creates dataset with numeric codes (given by user) and corresponding names (printed in output) for county/county of residence.
#' @author Fredrik Sandin, RCC Mellansverige
#' @noRd
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
          "S\u00f6dermanland",
          "\u00d6sterg\u00f6tland",
          "J\u00f6nk\u00f6ping",
          "Kronoberg",
          "Kalmar",
          "Gotland",
          "Blekinge",
          "Sk\u00e5ne",
          "Halland",
          "V\u00e4stra G\u00f6taland",
          "V\u00e4rmland",
          "\u00d6rebro",
          "V\u00e4stmanland",
          "Dalarna",
          "G\u00e4vleborg",
          "V\u00e4sternorrland",
          "J\u00e4mtland",
          "V\u00e4sterbotten",
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
          "S\u00f6dermanland",
          "\u00d6sterg\u00f6tland",
          "J\u00f6nk\u00f6ping",
          "Kronoberg",
          "Kalmar",
          "Kalmar",
          "Gotland",
          "Blekinge",
          "Sk\u00e5ne",
          "Sk\u00e5ne",
          "Sk\u00e5ne",
          "Halland",
          "V\u00e4stra G\u00f6taland",
          "V\u00e4stra G\u00f6taland",
          "V\u00e4stra G\u00f6taland",
          "V\u00e4stra G\u00f6taland",
          "V\u00e4rmland",
          "\u00d6rebro",
          "V\u00e4stmanland",
          "Dalarna",
          "G\u00e4vleborg",
          "V\u00e4sternorrland",
          "J\u00e4mtland",
          "V\u00e4sterbotten",
          "Norrbotten",
          paste0(
            if (language == "en") {
              "Others/private - "
            } else {
              "\u00d6vriga/privat - "
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
#' @noRd
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
#' @noRd
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
          "Syd\u00f6stra",
          "S\u00f6dra",
          "V\u00e4stra",
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
#' @noRd
rccShinyLevelNames <-
  function(
    level = "region",
    language = "sv",
    optionalLabel = NULL
  ) {
    if (is.null(optionalLabel)){
      if (level == "region") {
        levelName <- if (language == "en") {
          "Healthcare region"
        } else {
          "Sjukv\u00e5rdsregion"
        }
      } else if (level == "county_lkf") {
        levelName <- if (language == "en") {
          "County of residence"
        } else {
          "Bostadsl\u00e4n"
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
          "Ok\u00e4nd"
        }
      }
    } else {
      levelName <- optionalLabel
    }
    return(levelName)
  }

#' Group together geoUnits
#' @description internal function used by server.R to simplify code.
#' @author Fredrik Sandin, RCC Mellansverige
#' @noRd
rccShinyGroupVariable <-
  function(
    label = "sjukhus",
    otherVariables = NULL,
    otherLabels = NULL,
    optionalHospitalLabel = NULL,
    optionalCountyLabel = NULL,
    optionalRegionLabel = NULL
  ) {
    if (tolower(label) %in% c("sjukv\u00e5rdsregion", "healthcare region", tolower(optionalRegionLabel))) {
      "region"
    } else if (tolower(label) %in% c("region", "county", "bostadsl\u00e4n", "county of residence", tolower(optionalCountyLabel))) {
      "landsting"
    } else if (tolower(label) %in% c("sjukhus", "hospital", tolower(optionalHospitalLabel))) {
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
#' @noRd
rccShinyNoObservationsText <-
  function(language = "sv") {
    if (language == "en") {
      return("Not enough observations available for current selection criteria")
    } else {
      return("Ej tillr\u00e4ckligt med observationer f\u00f6r aktuellt urval")
    }
  }

#' Names for tabs
#' @description internal function used to supply names of the tabs.
#' @author Fredrik Sandin, RCC Mellansverige
#' @noRd
rccShinyTabsNames <-
  function(language = "sv") {
    tab <-
      data.frame(
        tab_language = c("sv", "en"),
        fig_compare = c("J\u00e4mf\u00f6relse", "Comparison"),
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
#' @noRd
rccShinyTXT <-
  function(language = "sv") {
    tab <-
      data.frame(
        tab_language = c("sv", "en"),
        all = c("Alla", "All"),
        missing = c("Uppgift saknas", "Missing"),
        outcome = c("Variabel", "Variable"),
        presentation = c("Redovisning", "Presentation"),
        levelofcomparison = c("J\u00e4mf\u00f6relseniv\u00e5", "Level of comparison"),
        hospitalinterest = c("Sjukhus av intresse", "Hospital of interest"),
        periodSplit1 = c("Redovisa varje", "Present each"),
        periodSplit2 = c("separat", "separate"),
        funnelplot = c("Funnel plot", "Funnel plot"),
        comment = c("Kommentar", "Comment"),
        description = c("Beskrivning", "Description"),
        numericchoices_prop = c("Andel inom ...", "Proportion within ..."),
        numeric_proportionwithin = c("andel inom ", "proportion within "),
        propWithinUnit = c("dagar", "days"),
        fewcases1 = c("Resultat fr\u00e5n grupper med f\u00e4rre \u00e4n", "Results from groups with fewer than"),
        fewcases2 = c("fall redovisas ej separat", "cases are not shown separately"),
        fewcases1cell = c("Om en enskild cell f\u00f6r en grupp inneh\u00e5ller f\u00e4rre \u00e4n", "If a single cell for a group contains fewer than"),
        fewcases2cell = c("fall redovisas ej absoluta antal f\u00f6r gruppen", "cases, absolute numbers for that group are not displayed"),
        median = c("Median", "Median"),
        medianiqr = c("Median samt kvartilavst\u00e5nd", "Median and interquartile range"),
        q1 = c("F\u00f6rsta kvartil", "First quartile"),
        q3 = c("Tredje kvartil", "Third quartile"),
        iqr = c("kvartilavst\u00e5nd", "interquartile range"),
        interquantilerange = c("kvantilavst\u00e5nd", "interquantile range"),
        iqr_and = c("samt", "and"),
        percentile = c("percentil", "percentile"),
        percent = c("Procent", "Percent"),
        noofcases = c("Antal fall", "No. of cases"),
        noofcases_nOfN = c("av", "of"),
        message = c("Meddelande", "Message"),
        numerator = c("T\u00e4ljare", "Numerator"),
        denominator = c("N\u00e4mnare", "Denominator"),
        total = c("Totalt", "Total"),
        period = c("Period", "Period"),
        periodTypeInputLabelQuarter = c("Kvartal", "Quarter"),
        periodTypeInputLabelYear = c("\u00c5r", "Year"),
        RIKET = c("RIKET", "SWEDEN"),
        dxYear = c("Diagnos\u00e5r", "Year of diagnosis"),
        limitRegion = c("Begr\u00e4nsa till sjukv\u00e5rdsregion", "Limit to healthcare region"),
        descriptionAbout = c("Om indikatorn", "About"),
        descriptionInterpretation = c("Att t\u00e4nka p\u00e5 vid tolkning", "Interpretation"),
        descriptionTechnical = c("Teknisk beskrivning", "Technical description"),
        targetValuesLabelIntermediate = c("Mellanniv\u00e5 av m\u00e5luppfyllelse", "Intermediate level of performance"),
        targetValuesLabelHigh = c("H\u00f6g niv\u00e5 av m\u00e5luppfyllelse", "High level of performance"),
        yes = c("Ja", "Yes"),
        no = c("Nej", "No"),
        idOverviewLink = c("\u00d6versikt", "Overview"),
        grouphidelessthan = c("(otillr\u00e4cklig data)", "(insufficient data)"),
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
#' @noRd
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
        "\u00d6vriga/privat"
      } else {
        "Others/private"
      }, sjukhus_privat = if (language[i] == "sv") {
        "Privat v\u00e5rdgivare"
      } else {
        "Private practice"
      }, sjukhus_ovriga = if (language[i] == "sv") {
        "\u00d6vriga"
      } else {
        "Others"
      }, sjukhus_privatovriga = if (language[i] == "sv") {
        "Privat v\u00e5rdgivare/\u00f6vriga"
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
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - Stockholm-Gotland"] <- "Others/private - Stockholm-Gotland"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - Mid Sweden"] <- "Others/private - Mid Sweden"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - Syd\u00f6stra"] <- "Others/private - Southeast"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - S\u00f6dra"] <- "Others/private - South"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - V\u00e4stra"] <- "Others/private - West"
      data[, paste0("landsting_", loop_language)][data[, paste0("landsting_", loop_language)] == "\u00d6vriga/privat - Norra"] <- "Others/private - North"

      data[, paste0("sjukhus_", loop_language)] <- data$sjukhus
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - Stockholm-Gotland"] <- "Private practice - Stockholm-Gotland"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - Mellansverige"] <- "Private practice - Mellansverige"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - Syd\u00f6stra"] <- "Private practice - Southeast"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - S\u00f6dra"] <- "Private practice - South"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - V\u00e4stra"] <- "Private practice - West"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "Privat v\u00e5rdgivare - Norra"] <- "Private practice - North"
      data[, paste0("sjukhus_", loop_language)][data[, paste0("sjukhus_", loop_language)] == "\u00d6vriga"] <- "Others"
    }
    data
  }
}
