# Assignment 3 - Part 2

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses ="character")  
  suppressWarnings(data[,11] <- as.numeric(data[,11])) #heart attack
  suppressWarnings(data[,17] <- as.numeric(data[,17])) #heart failure
  suppressWarnings(data[,23] <- as.numeric(data[,23])) #pneumonia

  listOfStates <- unique(data$State)  
  listOfOutcomes <- c("heart attack", "heart failure", "pneumonia")
  colIndex <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  hospitalName = ""
  if (state %in% listOfStates) {
    if ( outcome %in% listOfOutcomes) {
      
      countryDf <- data[data$State == state & !is.na(data[,colIndex[[outcome]]]),]
      orderSettings <- order(countryDf[,colIndex[[outcome]]], countryDf[, 2])
      countryDfOrdered <- countryDf[orderSettings,]
      
      numCalculated = 1
      if ( !is.numeric(num) ) {
        if ( num == "worst") {
          numCalculated = nrow(countryDfOrdered)
        }
      } else {
        numCalculated = num
      }
      if ( numCalculated > nrow(countryDfOrdered)) {
        hospitalName = "NA"
      } else {
        countryDfOrdered$RankCalculated <- seq(1:nrow(countryDfOrdered))
        hospitalName = countryDfOrdered$Hospital.Name[countryDfOrdered$RankCalculated == numCalculated]
      }
      hospitalName
    } else {
      stop("invalid outcome")
    }
  } else {
    stop("invalid state")
  }
}

