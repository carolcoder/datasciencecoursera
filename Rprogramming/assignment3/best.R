# Assignment 3 - Part 1

best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses ="character")  
  suppressWarnings(data[,11] <- as.numeric(data[,11])) #heart attack
  suppressWarnings(data[,17] <- as.numeric(data[,17])) #heart failure
  suppressWarnings(data[,23] <- as.numeric(data[,23])) #pneumonia
  
  countsByState <- tapply(rep(1,nrow(data)), data$State, sum)
  listOfStates <- names(countsByState)  
  listOfOutcomes <- c("heart attack", "heart failure", "pneumonia")
  colIndex <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  hospitalNames = list()
  if (state %in% listOfStates) {
    if ( outcome %in% listOfOutcomes) {
      minGrade <- min(data[,colIndex[[outcome]]][data$State==state], na.rm=TRUE)
      hospitalNames <- data$Hospital.Name[data$State==state & data[,colIndex[[outcome]]] == minGrade]
      hospitalNames[!is.na(hospitalNames)]
    } else {
      stop("invalid outcome")
    }
  } else {
    stop("invalid state")
  }
}

