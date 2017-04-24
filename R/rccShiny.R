
rccShiny <- 
  function(
    data=NULL,
    outcome="outcome",
    outcomeTitle=outcome,
    folder="ind",
    folderLinkText=outcomeTitle,
    titleTextBeforeSubtitle=NULL,
    titleTextAfterSubtitle=NULL,
    comment="",
    description=c(
      "(beskrivning saknas)",
      "(description missing)"
    ),
    geoUnitsHospital="sjukhus",
    geoUnitsCounty="landsting",
    geoUnitsRegion="region",
    geoUnitsFromLKF=FALSE,
    regionSelection=TRUE,
    regionLabel=c(
      "Begränsa till region",
      "Limit to region"
    ),
    period="period",
    periodLabel=c(
      "Diagnosår",
      "Year of diagnosis"
    ),
    userInputList=NULL,
    targetValues=NULL,
    targetValuesSortDescending=NULL,
    hideLessThan=5,
    language=c("sv"),
    npcrGroupPrivateOthers=TRUE
  ) {

    # # # # # #
    # # # # # #
    # # # # # #
    # Lägg till felkontroller!
    # # # # # #
    # # # # # #
    # # # # # #
 
    if (is.null(data) | !is.data.frame(data)) stop("'data' has to be a data.frame",call.=FALSE)
    
    testVariableError <- 
      function(var) {
        if (is.null(get(var))) stop(paste0("'",var,"' is missing"),call.=FALSE)
        if (any(is.na(get(var)))) stop(paste0("'",var,"' is missing"),call.=FALSE)
        if (is.list(get(var))) {
          tempList <- get(var)
          for (i in 1:length(tempList)) {
            if (!is.character(tempList[[i]])) stop(paste0("'",var,"' has to be of type character"),call.=FALSE)
          }
        } else {
          if (!is.character(get(var))) stop(paste0("'",var,"' has to be of type character"),call.=FALSE)
        }
      }
    testVariableError("outcome")
    testVariableError("outcomeTitle")
    testVariableError("folder")
    testVariableError("folderLinkText")
    
    if (is.null(comment)) comment <- ""

    if (is.null(period)) period <- "period"
    if (is.null(outcome)) outcome <- "outcome"

    # Check for region variable in data
    if (is.null(geoUnitsRegion)) geoUnitsRegion <- "region"
    if (!(geoUnitsRegion%in%colnames(data))) stop(paste0("Column '",geoUnitsRegion,"' not found in 'data'"),call.=FALSE)
    data$regionCode <- suppressWarnings(as.numeric(as.character(data[,geoUnitsRegion])))
    if (any(is.na(data$regionCode)) | !(all(data$regionCode%in%1:6))) stop(paste0("'",geoUnitsRegion,"' contains missing or invalid values. '",geoUnitsRegion,"' should only contain the values (",paste(1:6,collapse=", "),")."),call.=FALSE)

    if (is.null(geoUnitsCounty)) geoUnitsCounty <- "landsting"
    if (!(geoUnitsCounty%in%colnames(data))) stop(paste0("Column '",geoUnitsCounty,"' not found in 'data'"),call.=FALSE)
    data$landstingCode <- suppressWarnings(as.numeric(as.character(data[,geoUnitsCounty])))
    if (any(is.na(data$landstingCode)) | !(all(data$landstingCode%in%rccShinyCounties(lkf=geoUnitsFromLKF)$landstingCode))) stop(paste0("'",geoUnitsCounty,"' contains missing or invalid values. When 'geoUnitsFromLKF'=",geoUnitsFromLKF,", '",geoUnitsCounty,"' should only contain the values (",paste(rccShinyCounties(lkf=geoUnitsFromLKF)$landstingCode,collapse=", "),")."),call.=FALSE)
 
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # # # # # #
    # # # # # #
    # # # # # #

    tempLinks <- vector()

    # Save folder for each language 
    
    for (loop_language in language) {
      
      which_language <- which(language==loop_language)

      GLOBAL_language <- loop_language
      
      # Check for period variable in data
      if (period%in%colnames(data)) {
        data$period <- data[,period]
      } else {
        stop(paste0("Column '",period,"' not found in 'data'"),call.=FALSE)
      }
      
      # Check for outcome variable(s) in data
      for (i in 1:length(outcome)) {
        if (period%in%colnames(data)) {
          if (paste0(outcome[i],"_",loop_language)%in%colnames(data)) data[,outcome[i]] <- data[,paste0(outcome[i],"_",loop_language)]
        } else {
          stop(paste0("Column '",outcome[i],"' not found in 'data'"),call.=FALSE)
        }
      }

      # Add region names
      data$region <- 
        factor(
          data$region,
          levels=1:6,
          labels=rccShinyRegionNames(language=loop_language)
        )
      
      # Add county names
      data <- data[,colnames(data)!="landsting"]
      data <- 
        merge(
          data,
          rccShinyCounties(
            language=loop_language,
            lkf=geoUnitsFromLKF
          ),
          by="landstingCode",
          all.x=TRUE
        )
      
      # Check for hospital variable in data
      if (is.null(geoUnitsHospital)) geoUnitsHospital <- "sjukhus"
      if (!(geoUnitsHospital%in%colnames(data))) stop(paste0("Column '",geoUnitsHospital,"' not found in 'data'"),call.=FALSE)
      data$sjukhus <- 
        if(paste0(geoUnitsHospital,"_",loop_language)%in%colnames(data)){
          data[,paste0(geoUnitsHospital,"_",loop_language)]
        } else {
          data[,geoUnitsHospital]
        }
      # Fix missing in hospital variable
      data$sjukhus[is.na(data$sjukhus) | data$sjukhus==""] <- rccShinyTXT(language=GLOBAL_language)$missing
      
      includeVariables <- c("period","region","landsting","sjukhus")
      
      # Check for user variable(s) in data
      GLOBAL_userInputList <- userInputList
      if (!is.null(GLOBAL_userInputList)) {
        userInputVariables <- vector()
        for (i in 1:length(GLOBAL_userInputList)) {
          if (is.null(GLOBAL_userInputList[[i]]$var)) stop(paste0("'var' is missing from userInputList[[",i,"]]"),call.=FALSE)
          temp_var <- GLOBAL_userInputList[[i]]$var
          if (!(temp_var%in%colnames(data))) stop(paste0("The variable '",temp_var,"' from userInputList[[",i,"]] is missing in 'data'"),call.=FALSE)
          
          if (paste0(temp_var,"_",loop_language)%in%colnames(data)) data[,temp_var] <- data[,paste0(temp_var,"_",loop_language)]
          userInputVariables <- c(userInputVariables,temp_var)
          
          if (is.null(GLOBAL_userInputList[[i]]$label)) GLOBAL_userInputList[[i]]$label <- temp_var
          GLOBAL_userInputList[[i]]$label <- ifelse(length(GLOBAL_userInputList[[i]]$label)>=which_language,GLOBAL_userInputList[[i]]$label[which_language],GLOBAL_userInputList[[i]]$label[1])
          
          GLOBAL_userInputList[[i]]$classNumeric <- class(data[,temp_var])%in%c("difftime","numeric","integer")
          
          if (is.null(GLOBAL_userInputList[[i]]$choices)) {
            if (GLOBAL_userInputList[[i]]$classNumeric) {
              GLOBAL_userInputList[[i]]$choices <- range(data[,temp_var],na.rm=TRUE)
            } else {
              GLOBAL_userInputList[[i]]$choices <- levels(factor(data[,temp_var]))
            }
          }
          if (is.list(GLOBAL_userInputList[[i]]$choices)) {
            GLOBAL_userInputList[[i]]$choices <- GLOBAL_userInputList[[i]]$choices[[ifelse(length(GLOBAL_userInputList[[i]]$choices)>=which_language,which_language,1)]]
          }
          
          if (is.null(GLOBAL_userInputList[[i]]$selected)) GLOBAL_userInputList[[i]]$selected <- GLOBAL_userInputList[[i]]$choices
          if (is.list(GLOBAL_userInputList[[i]]$selected)) {
            GLOBAL_userInputList[[i]]$selected <- GLOBAL_userInputList[[i]]$selected[[ifelse(length(GLOBAL_userInputList[[i]]$selected)>=which_language,which_language,1)]]
          }
          
          if (is.null(GLOBAL_userInputList[[i]]$multiple)) GLOBAL_userInputList[[i]]$multiple <- TRUE
          
          if (is.null(GLOBAL_userInputList[[i]]$showInTitle)) GLOBAL_userInputList[[i]]$showInTitle <- TRUE
        }
        includeVariables <- c(includeVariables,userInputVariables)
      }
      
      includeVariables <- c(includeVariables,outcome)
      GLOBAL_outcome <- outcome
      GLOBAL_outcomeClass <- vector()
      for (i in 1:length(outcome)) {
        GLOBAL_outcomeClass[i] <- class(data[,outcome[i]])
      }

      GLOBAL_data <- 
        subset(
          data,
          select=includeVariables
        )

      GLOBAL_outcomeTitle <- 
        if (is.list(outcomeTitle) & length(outcomeTitle)>=which_language) {
          outcomeTitle[[which_language]]
        } else {
          outcomeTitle
        }
      
      GLOBAL_titleTextBeforeSubtitle <- 
        if (length(titleTextBeforeSubtitle)>=which_language) {
          titleTextBeforeSubtitle[which_language]
        } else {
          titleTextBeforeSubtitle[1]
        }
      GLOBAL_titleTextAfterSubtitle <- 
        if (length(titleTextAfterSubtitle)>=which_language) {
          titleTextAfterSubtitle[which_language]
        } else {
          titleTextAfterSubtitle[1]
        }
      
      GLOBAL_comment <- ifelse(length(comment)>=which_language,comment[which_language],comment[1])
      
      GLOBAL_description <- ifelse(length(description)>=which_language,description[which_language],description[1])
 
      GLOBAL_periodLabel <- ifelse(length(periodLabel)>=which_language,periodLabel[which_language],periodLabel[1])
      GLOBAL_periodStart <- min(data$period,na.rm=TRUE)
      GLOBAL_periodEnd <- max(data$period,na.rm=TRUE)
      
      GLOBAL_geoUnitsFromLKF <- geoUnitsFromLKF
      
      GLOBAL_regionSelection <- regionSelection
      GLOBAL_regionLabel <- ifelse(length(regionLabel)>=which_language,regionLabel[which_language],regionLabel[1])
      GLOBAL_regionChoices <- levels(factor(data$region))
      GLOBAL_regionSelected <- rccShinyTXT(language=GLOBAL_language)$all

      GLOBAL_targetValues <- targetValues
      GLOBAL_targetValuesSortDescending <- targetValuesSortDescending
      
      GLOBAL_hideLessThan <- 
        ifelse(
          hideLessThan<5,
          5,
          hideLessThan
        )
      
      GLOBAL_npcrGroupPrivateOthers <- npcrGroupPrivateOthers

      if (!dir.exists(paste0("./apps/",loop_language))) {
        dir.create(paste0("./apps/",loop_language),showWarnings=FALSE)
      }
      
      dir.create(paste0("./apps/",loop_language,"/",folder),showWarnings=FALSE)
      dir.create(paste0("./apps/",loop_language,"/",folder,"/data"),showWarnings=FALSE)
      dir.create(paste0("./apps/",loop_language,"/",folder,"/docs"),showWarnings=FALSE)
      
      file.copy("./source/global.R",paste0("./apps/",loop_language,"/",folder,"/global.R"),overwrite=TRUE)
      file.copy("./source/server.R",paste0("./apps/",loop_language,"/",folder,"/server.R"),overwrite=TRUE)
      file.copy("./source/ui.R",paste0("./apps/",loop_language,"/",folder,"/ui.R"),overwrite=TRUE)
      
      save(
        GLOBAL_data,
        GLOBAL_outcome,
        GLOBAL_outcomeTitle,
        GLOBAL_outcomeClass,
        GLOBAL_titleTextBeforeSubtitle,
        GLOBAL_titleTextAfterSubtitle,
        GLOBAL_comment,
        GLOBAL_description,
        GLOBAL_periodLabel,
        GLOBAL_periodStart,
        GLOBAL_periodEnd,
        GLOBAL_geoUnitsFromLKF,
        GLOBAL_regionSelection,
        GLOBAL_regionLabel,
        GLOBAL_regionChoices,
        GLOBAL_regionSelected,
        GLOBAL_targetValues,
        GLOBAL_targetValuesSortDescending,
        GLOBAL_userInputList,
        GLOBAL_hideLessThan,
        GLOBAL_language,
        GLOBAL_npcrGroupPrivateOthers,
        file=paste0("./apps/",loop_language,"/",folder,"/data/data.RData")
      )
      
      # Output description to .html-file
      
      printRow <- 
        function(
          row="",
          file=globalOutFile,
          append=TRUE
        ) {
          cat(
            paste0(
              row,
              "\n"
            ),
            file=file,
            append=append
          )
        }
      
      globalOutFile <<- file(paste0("./apps/",loop_language,"/",folder,"/docs/description.html"),"w",encoding="UTF-8")
      printRow(paste0("<!DOCTYPE html>"),append=FALSE)
      printRow(paste0("<html>"))
      printRow(paste0("<body>"))
      for (i in 1:length(GLOBAL_outcome)) {
        printRow(paste0("<h4>",GLOBAL_outcomeTitle[i],"</h4>"))
      }
      if (!is.null(GLOBAL_description)) {
        printRow(paste0("<p>",rccShinyTXT(language=GLOBAL_language)$description,"</p>"))
        printRow(paste0("<div style='background-color:#f7f7f7;width:100%;border-radius:3px;padding:3px 5px;margin:10px 0px;'>"))
        printRow(paste0(GLOBAL_description))
        printRow(paste0("</div>"))
      }
      printRow(paste0("</body>"))
      printRow(paste0("</html>"))
      close(globalOutFile)
      
      tempLinks <- 
        cbind(
          tempLinks,
          paste0(
            "<li class='reportLi'><a data-toggle='pill' href='#reportDiv' class='reportLink' id='",folder,"'>",ifelse(length(folderLinkText)>=which_language,folderLinkText[which_language],folderLinkText[1]),"</a></li>")
        )
      
    }
    
    return(invisible(tempLinks))
  }
