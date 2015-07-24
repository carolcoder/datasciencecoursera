# Assignment 3

outcome <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
listOfStates <- unique(outcome$State)
outcome[,11] <- as.numeric(outcome[,11])
outcome[,17] <- as.numeric(outcome[,17])
noNA <- outcome[!is.na(outcome[,11]),]
orderHA <- order(noNA$State, noNA[,11], noNA[,2])
invertedOrder <- order(noNA$State, -noNA[,11], noNA[,2])
outcomeOrdered <- noNA[orderHA,]
outcomeInvertedOrder <- noNA[invertedOrder,]

outcomeOrdered$RankCalculated <- ave( outcomeOrdered[,11], outcomeOrdered$State, FUN=seq_along )
outcomeInvertedOrder$RankCalculated <- ave( outcomeInvertedOrder[,11], outcomeInvertedOrder$State, FUN=seq_along )


rank20 <- outcomeOrdered[outcomeOrdered$RankCalculated == 20,] 
resultDf = data.frame( Hospital=rank20$Hospital.Name, State=rank20$State )
lapply(listOfStates, function(s) {
  if (! ( s %in% resultDf$State ) )  {
    resultDf <<- rbind(resultDf, c(Hospital=NA), c(State="AK"))
  }
})

hn = rank20$Hospital.Name
he = rank20$State
lapply(listOfStates, function(s) {
  if (! ( s %in% rank20$State ) )  {
    hn[length(hn)+1] <<- NA
    he[length(he)+1] <<- s
   }
})

resultDf = data.frame( Hospital=hn, State=he )

resultDf[order(resultDf$State),]

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
    
    resultDf = data.frame( Hospital=hn, State=he )    
    
    resultDf[order(resultDf$State), ]
  } else {
    stop("invalid outcome")
  }
}

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)
