grpData <- function(grpNames = c("a", "b", "c"), 
                    grpMeans = c(40, 50, 60), 
                    grpSDs = c(10, 10, 10), 
                    daLen = 30,
                    decimal_places=2,
                    constraint.L = NA,
                    constraint.U = NA,
                    makeplot = FALSE,
                    writeFile = TRUE,
                    fileName = "dataFile.csv") {
  # make a grouped data set
  # the length of the first 2 arguments determines the number of groups
  # (these lengths must be the same)
  # entering a single number for gprSDs uses that SD for all groups
  # example call:
  # myData <- grpData(c("alcohol", "thc", "cocaine", "control"), 
  #                   c(50, 40, 70, 60), 5, 42, 0, makeplot = TRUE
  #                   writeFile = FALSE, "MyGrpData.csv")
  
  # import needed packages
  library(tidyverse)
  
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
  
  # round the data
  dataMat = round(dataMat, decimal_places)
  
  # convert to a data frame
  df <- data.frame(dataMat)
  colnames(df) <- grpNames
  
  # could've done this before plotting but I didn't
  df = df %>% pivot_longer(colnames(df), names_to = "group", values_to = "value")
  
  # Is there a constraint on the data?
  # Lower constraint: add to anything < constraint 
  # 2*delta, where delta = constraint - value
  if(is.na(constraint.L)==FALSE){
    L.prblms <- which(df[,2]<constraint.L)
    df[L.prblms,2] <- 
      df[L.prblms,2] + 2*(constraint.L - df[L.prblms,2])
  }
  
  #Same deal for upper constraint, but reverse
  if(is.na(constraint.U)==FALSE){
    U.prblms <- which(df[,2]>constraint.U)
    df[U.prblms,2] <- 
      df[U.prblms,2] + 2*(constraint.U - df[U.prblms,2])
  }
  
  if (makeplot) {
    #----- plot in order to check that these are the data we want -----
    # stack() returns ind (factor) and values columns
    daPlot <- ggplot(df, aes(x = group, y = value)) +
      geom_boxplot(notch = TRUE) +
      labs(x = "Group")
    print(daPlot)
  }
  
  # write data to file in current directory 
  if (writeFile){
    write_csv(x = df, file = fileName)
  }
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
