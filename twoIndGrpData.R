twoIndGrpData <- function(grpNames = c("a", "b"), 
                          grpMeans = c(40, 50), 
                          grpSD = 10, 
                          daLen = 42,
                          decimal_places=2,
                          fileName = "dataFile.csv") {
  
  # import needed packages
  library(tidyverse)  # for write_csv()
  
  # will call grpData() for just two groups
  source("grpData.R")
  
  df <- grpData(grpNames,  grpMeans, grpSD, daLen, decimal_places, 
                  writeFile = FALSE, fileName)
  
  write_csv(x = df, file = fileName)
  
  return(df)
  
}