#Assignment 1
# Set wd to C:/Users/sao-carolinamo/Documents/Carolina/R/coursera/Rprogramming/assignment1

pollutantmean <- function(directory, pollutant, id=1:332) {

  files <- list.files(directory, pattern="*.csv")
  
  fullFilePaths = paste(getwd(), "/", directory, "/" , files, sep = "")
  
  dfInFiles = lapply(fullFilePaths, read.csv)
  
  aDf = data.frame()


  returnValue = 0
  
  if ( pollutant == "sulfate") {
    
    for(df in dfInFiles) {
      isNa = is.na(df$sulfate)
      goodDf <- df[!isNa,]
      aDf <- rbind(aDf, goodDf)
    }    
    
    returnValue = mean( aDf$sulfate[aDf$ID %in% id] )
  } else if ( pollutant == "nitrate") {
    
    for(df in dfInFiles) {
      isNa = is.na(df$nitrate)
      goodDf <- df[!isNa,]
      aDf <- rbind(aDf, goodDf)
    }
    
    returnValue = mean( aDf$nitrate[aDf$ID %in% id] )
  } 
  
  returnValue
}

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)

