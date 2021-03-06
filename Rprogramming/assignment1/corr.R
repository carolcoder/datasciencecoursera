# corr.R - Assignment 3
# Set wd to C:/Users/sao-carolinamo/Documents/Carolina/R/coursera/Rprogramming/assignment1

corr <- function(directory, threashold=0) {
  
  completeDf <- complete(directory)
  
  aboveThreashold <- completeDf[completeDf$nobs > threashold, ]
  
  files <- list.files(directory, pattern="*.csv")
  
  fullFilePaths = paste(getwd(), "/", directory, "/" , files, sep = "")
  
  dfInFiles = lapply(fullFilePaths, read.csv)
  
  aDf = data.frame()
  
  for(df in dfInFiles) {
    good <- complete.cases(df)
    completeData <- df[good,]
    aDf <- rbind(aDf, completeData)
  }   
  
  dataInThreashold <- aDf[aDf$ID %in% aboveThreashold$id, ]
  
  correlations <- lapply(aboveThreashold$id, function(x) {
    
    cor(dataInThreashold$nitrate[dataInThreashold$ID == x], dataInThreashold$sulfate[dataInThreashold$ID == x], use = "pairwise.complete.obs")  
    
  })  
  unlist(correlations)

}
