grpData <- function(grpNames = c("a", "b", "c"), 
                    grpMeans = c(40, 50, 60), 
                    grpSDs = c(10, 10, 10), 
                    daLen = 30,
                    fileName = "dataFile.csv") {
  # make a grouped data set
  # the length of the first 2 arguments determines the number of groups
  # (these lengths must be the same)
  # entering a single number for gprSDs uses that SD for all groups
  # example call:
  # myData <- grpData(c("alcohol", "thc", "cocaine", "control"), 
  #                   c(50, 40, 70, 60), 5, 42, "MyGrpData.csv")
  
  # note, must have the tidyverse loaded: library(tidyverse)
  
  nGrps <- length(grpNames)
  
  # fill out SD vector if necessary
  if (length(grpSDs) == 1){
    grpSDs <- rep(grpSDs, 4)
  }
  
  # generate the data 
  dataMat <- matrix(data = NA, daLen, nGrps)
  for (i in 1:nGrps){
    dataMat[ ,i] <-  rnorm(daLen, grpMeans[i], grpSDs[i])
  }
  
  # convert to a data frame
  df <- data.frame(dataMat)
  colnames(df) <- grpNames
  
  #----- plot in order to check that these are the data we want -----
  daPlot <- ggplot(stack(df), aes(x = ind, y = values)) +
    geom_boxplot(notch = TRUE) +
    labs(x = "Group")
  print(daPlot)
 
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
