#' "echte" Faktorvariablen auslassen und restliche in Strings umwandeln
#' @param x entsprechender Datensatz
#' @export
resetFactors <- function(x) {
  dframe = x
  for(i in 1:ncol(dframe)) {
    #print(i)
    nofL <- nlevels(dframe[,i])
    #print(nofL)
    #print(summary(testdaten[,i]))
    if(nofL >= 10) {
      dframe[,i] <- as.character(dframe[,i])
    }
  }
  dframe
}

#' Datum erkennen
#' @param x entsprechender Datensatz
#' @export
checkDate <- function(x) {
  x[x==""]<-NA
  dframe = x
  dateAll <- FALSE
  names <- names(dframe)
  for(i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      isDate <- TRUE
      for (j in dframe[,i]) {
        if(is.na(j)) {
          next
        }
        if((nchar(j)<8&&nchar(j)<10)||nchar(j)>10) {
          # print("ist falsch")
          # print(names[i])
          isDate <- FALSE
        } else if(nchar(j)==10 || nchar(j)==8) {
          regexausdruck <- '^(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$'
          dateReg1 <- grepl(regexausdruck, j)
          #print(dateReg1)
          dateReg2 <- grepl("^\\d{4}(\\/|-|\\.)(0?[1-9]|1[012])(\\/|-|\\.)(0?[1-9]|[12][0-9]|3[01])$", j)
          # dateReg3 <- grepl("^((0?[13578]|10|12)(-|\\/)(([1-9])|(0[1-9])|([12])([0-9]?)|(3[01]?))(-|\\/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1}))|(0?[2469]|11)(-|\\/)(([1-9])|(0[1-9])|([12])([0-9]?)|(3[0]?))(-|\\/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1})))$",j)
          print(j)
          dateReg3 <- grepl("(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\\d\\d",j)
          print(dateReg3)
          #print(dateReg)
          if(!dateReg1 && !dateReg2 && !dateReg3) {
            isDate <- FALSE
          }
        }
      }
      if(isDate) {
        dateAll <- TRUE
      }
    }
  }
  dateAll
}

#' Datum konvertieren
#' @param x entsprechender Datensatz
#' @export
convertDate <- function(x, dateformat) {
  x[x==""]<-NA
  dframe = x
  names <- names(dframe)
  for(i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      isDate <- TRUE
      for (j in dframe[,i]) {
        if(is.na(j)) {
          next
        }
        if((nchar(j)<8&&nchar(j)<10)||nchar(j)>10) {
          # print("ist falsch")
          # print(names[i])
          isDate <- FALSE
        } else if(nchar(j)==10 || nchar(j)==8) {
          regexausdruck <- '^(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$'
          dateReg1 <- grepl(regexausdruck, j)
          #print(dateReg1)
          dateReg2 <- grepl("^\\d{4}(\\/|-|\\.)(0?[1-9]|1[012])(\\/|-|\\.)(0?[1-9]|[12][0-9]|3[01])$", j)
          # dateReg3 <- grepl("^((0?[13578]|10|12)(-|\\/)(([1-9])|(0[1-9])|([12])([0-9]?)|(3[01]?))(-|\\/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1}))|(0?[2469]|11)(-|\\/)(([1-9])|(0[1-9])|([12])([0-9]?)|(3[0]?))(-|\\/)((19)([2-9])(\\d{1})|(20)([01])(\\d{1})|([8901])(\\d{1})))$",j)
          print(j)
          dateReg3 <- grepl("(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\\d\\d",j)
          print(dateReg3)
          #print(dateReg)
          if(!dateReg1 && !dateReg2 && !dateReg3) {
            isDate <- FALSE
          }
        }
      }
      #print(isDate)
      if (isDate) {
        # dframe[,i] <- as.POSIXct(dframe[,i], format="%d.%m.%Y")
        # Timezone is very important!!
        #print("hier")
        #print(names[i])
        if (dateReg1) {
          #print("hier2")
          dframe[,i] <- as.POSIXct(dframe[,i],
                                   tryFormats = c("%d.%m.%Y",
                                                  "%d-%m-%Y",
                                                  "%d/%m/%Y"),
                                   tz="Europe/Vienna")
        } else if(dateReg2) {
          dframe[,i] <- as.POSIXct(dframe[,i],
                                   tryFormats = c("%Y.%m.%d",
                                                  "%Y-%m-%d",
                                                  "%Y/%m/%d"),
                                   tz="Europe/Vienna")
        } else {
          dframe[,i] <- as.POSIXct(dframe[,i],
                                   tryFormats = c("%m.%d.%Y",
                                                  "%m-%d-%Y",
                                                  "%m/%d/%Y"),
                                   tz="Europe/Vienna")
        }
      } else {
        print(paste0(names[i], " ist kein Datum"))
      }
      print("--------------------------------")
    }
  }
  dframe
}

#' Uhrzeit konvertieren
#' @param x entsprechender Datensatz
#' @export
convertTime <- function(x) {
  dframe = x
  names <- names(dframe)
  for(i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      isTime <- TRUE
      for (j in dframe[,i]) {
        if(is.na(j)) {
          next
        }
        if(nchar(j)!=5&&nchar(j)>0) {
          isTime <- FALSE
        } else if(nchar(j)==10) {
          timeReg <- grepl("[0-9]{2}\\:[0-9]{2}", j)
          #print(dateReg)
          if(!timeReg) {
            isTime <- FALSE
          }
        }
      }
      #library('chron')
      library(hms)
      #time = paste0(as.character(dframe[,i]),":00",sep = '')
      if (isTime) {
        #dframe[,i] <- chron(time)
        dframe[,i] <- parse_hm(dframe[,i])
        print(typeof(dframe[,i]))
        print(paste0(names[i], " wurde in Uhrzeit umgewandelt"))
      } else {
        print(paste0(names[i], " ist keine Uhrzeit"))
      }
      print("--------------------------------")
    }
  }
  dframe
}

#' Datum + Uhrzeit konvertieren
#' @param x entsprechender Datensatz
#' @export
convertDateAndTime <- function(x) {
  dframe = x
  names <- names(dframe)
  for(i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      isDateTime <- TRUE
      for (j in dframe[,i]) {
        if(is.na(j)) {
          next
        }
        if(nchar(j)!=16&&nchar(j)>0) {
          isDateTime <- FALSE
        } else if(nchar(j)==16) {
          dateTimeReg <- grepl("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4} [0-9]{2}:[0-9]{2}", j)
          #print(dateReg)
          if(!dateTimeReg) {
            isDateTime <- FALSE
          }
        }
      }
      #print(isDate)
      if (isDateTime) {
        # Timezone is very important!!
        dframe[,i] <- as.POSIXct(dframe[,i], format="%d.%m.%Y %H:%M", tz="Europe/Vienna")
        print(paste0(names[i], " wurde in Datum und Uhrzeit umgewandelt"))
      } else {
        print(paste0(names[i], " ist kein Datum + Uhrzeit"))
      }
      print("--------------------------------")
    }
  }
  dframe
}

#' NAs auffüllen
#' @param x entsprechender Datensatz
#' @export
fillNAs <- function(x) {
  x[x==""]<-NA
  dframe = x
  #dframe = x
  #names <- names(dframe)
  #for (i in 1:ncol(dframe)) {
  #  if (typeof(dframe[,i])=="character") {
  #    for (j in 1:nrow(dframe)) {
  #      if(is.na(j)) {
  #        next
  #      }
  #      if(nchar(dframe[,i][j])==0) {
  #        #print("leeres String-Feld")
  #        dframe[,i][j] = NA
  #      }
  #    }
  #  }
  #}
  dframe
}

#' Nur NA-Spalten loeschen
#' @param x entsprechender Datensatz
#' @export
removeNAColumns <- function(x) {
  dframe = x
  drop <- c()

  names <- names(dframe)

  NAcols <- 0
  for (i in 1:ncol(dframe)) {
    NAs <- sapply(dframe[i], function(x) sum(is.na(x)))
    #print(NAs)
    if (NAs == nrow(dframe)) {
      NAcols <- NAcols + 1
      print(paste0(names[i], " hat nur NAs"))
      drop <- c(drop,names[i])
    }
  }

  print(NAcols)
  ## Entfernen der NA-Reihen
  dframe <- dframe[,!(names(dframe) %in% drop)]
  ncol(dframe)

  dframe
}

#' Spalten mit "echten" Strings aussortieren
#' @description Damit keine Notizen etc fälschlicherweise als String-Variablen aufgenommen werden (eig sind Strings nur als Factors brauchbar)
#' @param x entsprechender Datensatz
#' @export
filterStrings <- function(x) {
  dframe = x
  print(ncol(dframe))
  elements <- c()
  drop <- c()
  # Vektor, damit alle Variablen nur ein mal vorkommen

  names <- names(dframe)
  for (i in 1:ncol(dframe)) {
    elements <- c()
    if (typeof(dframe[,i])=="character") {
      for (j in 1:nrow(dframe)) {
        if(! dframe[,i][j] %in% elements) {
          elements <- c(elements,dframe[,i][j])
        }
      }
      NAs <- sapply(dframe[i], function(x) sum(is.na(x)))
      actualFiels <- nrow(dframe)-NAs
      # print(elements)
      elements <- elements[!is.na(elements)]
      elements <- elements[elements!=""]
      differentValues <- length(elements[!is.na(elements)])
      print(actualFiels)
      print(paste0(names[i], " - Verschiedene Werte: ", differentValues))
      if (differentValues > 10 || differentValues < 2) {
        print(i)
        drop <- c(drop,names[i])
      }
      # Hier eine Unterscheidung verwenden oder nicht?? -> eventuell wichtige Variablen
      #print(elements)
    }
  }

  dframe <- dframe[,!(names(dframe) %in% drop)]
  dframe <- as.data.frame(unclass(dframe))
  print(ncol(dframe))

  dframe
}
