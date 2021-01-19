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

#' Datum konvertieren
#' @param x entsprechender Datensatz
#' @export
convertDate <- function(x) {
  dframe = x
  names <- names(dframe)
  for(i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      isDate <- TRUE
      for (j in dframe[,i]) {
        if(nchar(j)!=10&&nchar(j)>0) {
          isDate <- FALSE
        } else if(nchar(j)==10) {
          dateReg <- grepl("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", j)
          #print(dateReg)
          if(!dateReg) {
            isDate <- FALSE
          }
        }
      }
      #print(isDate)
      if (isDate) {
        dframe[,i] <- as.POSIXct(dframe[,i], format="%d.%m.%Y")
        print(paste0(names[i], " wurde in Datum umgewandelt"))
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
        dframe[,i] <- as.POSIXct(dframe[,i], format="%d.%m.%Y %H:%M")
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
  dframe = x
  names <- names(dframe)
  for (i in 1:ncol(dframe)) {
    if (typeof(dframe[,i])=="character") {
      for (j in 1:nrow(dframe)) {
        if(nchar(dframe[,i][j])==0) {
          #print("leeres String-Feld")
          dframe[,i][j] = NA
        }
      }
    }
  }
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
#' Damit keine Notizen etc fälschlicherweise als String-Variablen aufgenommen werden (eig sind Strings nur als Factors brauchbar)
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
      differentValues <- length(elements)
      print(actualFiels)
      print(paste0(names[i], " - Verschiedene Werte: ", differentValues))
      drop <- c(drop,names[i])
      # Hier eine Unterscheidung verwenden oder nicht?? -> eventuell wichtige Variablen
      #print(elements)
    }
  }

  dframe <- dframe[,!(names(dframe) %in% drop)]
  print(ncol(dframe))

  dframe
}
