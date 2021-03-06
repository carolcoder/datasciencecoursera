# Assignment 3 - Part 3

rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses ="character")  
  suppressWarnings(data[,11] <- as.numeric(data[,11])) #heart attack
  suppressWarnings(data[,17] <- as.numeric(data[,17])) #heart failure
  suppressWarnings(data[,23] <- as.numeric(data[,23])) #pneumonia

  listOfStates <- unique(data$State)  
  listOfOutcomes <- c("heart attack", "heart failure", "pneumonia")
  colIndex <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

  if ( outcome %in% listOfOutcomes) {
    
    noNA <- data[!is.na(data[,colIndex[[outcome]]]),]
    orderSettings <- order(noNA$State, noNA[,colIndex[[outcome]]], noNA[,2])
    invertedOrderSettings <- order(noNA$State, -noNA[,colIndex[[outcome]]], noNA[,2])
    outcomeOrdered <- noNA[orderSettings,]
    outcomeInvertedOrder <- noNA[invertedOrderSettings,]
    outcomeOrdered$RankCalculated <- ave( outcomeOrdered[,colIndex[[outcome]]], outcomeOrdered$State, FUN=seq_along )
    outcomeInvertedOrder$RankCalculated <- ave( outcomeInvertedOrder[,colIndex[[outcome]]], outcomeInvertedOrder$State, FUN=seq_along )
    
    
    searchingDf = data.frame()
    numCalculated = 1
    if ( !is.numeric(num) ) {
      if ( num == "worst") {
        searchingDf = outcomeInvertedOrder
      } else {
        searchingDf = outcomeOrdered
        
      }
    } else {
      searchingDf = outcomeOrdered
      numCalculated = num
    }    
    
    ranked <- searchingDf[searchingDf$RankCalculated == numCalculated,] 
    hn = ranked$Hospital.Name
    he = ranked$State
    lapply(listOfStates, function(s) {
      if (! ( s %in% ranked$State ) )  {
        hn[length(hn)+1] <<- NA
        he[length(he)+1] <<- s
      }
    })
    
    resultDf = data.frame( hospital=hn, state=he )    
    
    resultDf[order(resultDf$state), ]
  } else {
    stop("invalid outcome")
  }
}
