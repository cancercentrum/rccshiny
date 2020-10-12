# Workaround to mimic an INCA-like situation

df <- rccShinyData
df$id <- 1:nrow(df)
df$sjukhuskod <- as.numeric(substring(df$sjukhus, 9, 10))
df$idAuthorisedToView <- df$sjukhuskod == 10

environmentVariables <- list(UserParentUnitCode = 10)
optionsList$incaUserHospital <- environmentVariables$UserParentUnitCode
