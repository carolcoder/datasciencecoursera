# complete.R - Assignment 2
# Set wd to C:/Users/sao-carolinamo/Documents/Carolina/R/coursera/Rprogramming/assignment1

complete <- function(directory, id=1:332) {
  
  files <- list.files(directory, pattern="*.csv")
  
  fullFilePaths = paste(getwd(), "/", directory, "/" , files, sep = "")
  
  dfInFiles = lapply(fullFilePaths, read.csv)
  
  aDf = data.frame()
  
  for(df in dfInFiles) {
    good <- complete.cases(df)
    completeData <- df[good,]
    aDf <- rbind(aDf, completeData)
  } 
  
  dataInId = aDf[aDf$ID %in% id,]
  countCompleteNbs = lapply(id, function(x) { nrow(dataInId[dataInId$ID == x,]) } )
  dataFrameSummary = data.frame( id, nobs=unlist(countCompleteNbs)) #unlist converts list to vector
}


