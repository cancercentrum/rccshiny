set.seed(567)
listLandstingL <- c(10,11,12,13,21,22,23,24,25,26,27,28,30,41,42,42,50,51,52,53,54,55,56,57,61,62,63,64,65,91,92,93,94,95,96)
listLandstingR <- c(1,1,2,2,3,3,3,3,3,1,4,4,4,4,4,5,5,5,5,5,2,2,2,2,2,6,6,6,6,1,2,3,4,5,6)
listSjukhus <-
  paste0(
    rep(
      paste0(
        "Sjukhus ",
        listLandstingL,
        "-",
        listLandstingR
      ),
      each = 3
    ),
    "-",
    rep(
      letters[1:3],
      each = 3
    )
  )

rccShinyData <-
  data.frame(
    sjukhus = sample(listSjukhus, 20000, replace = TRUE),
    stringsAsFactors = FALSE
  )
rccShinyData$landsting <- as.numeric(substr(rccShinyData$sjukhus, 9, 10))
rccShinyData$region <- as.numeric(substr(rccShinyData$sjukhus, 12, 12))

rccShinyData$period <-
  sample(
    2012:2016,
    nrow(rccShinyData),
    replace = TRUE
  )

rccShinyData$age <-
  sample(
    18:99,
    nrow(rccShinyData),
    replace = TRUE
  )

rccShinyData$outcome1 <- runif(nrow(rccShinyData)) > 0.7
rccShinyData$outcome2 <- round(rnorm(nrow(rccShinyData), 30, 30))
rccShinyData$outcome3 <- factor(1*(runif(nrow(rccShinyData)) > 0.2) + 1 *
                                  (runif(nrow(rccShinyData)) > 0.3) + 1 *
                                  (runif(nrow(rccShinyData)) > 0.5) + 1 *
                                  (runif(nrow(rccShinyData)) > 0.7),
                                levels = 0:4, labels = paste0("Variabel ", toupper(letters[1:5])))

rccShinyData$stage <-
  sample(
    factor(c("I","II","III","IV")),
    nrow(rccShinyData),
    replace = TRUE
  )

usethis::use_data_raw()
usethis::use_data(rccShinyData, overwrite = TRUE)
