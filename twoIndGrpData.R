twoIndGrpData <- function(grpNames = c("a", "b"), 
                          grpMeans = c(40, 50), 
                          grpSD = 10, 
                          daLen = 42,
                          decimal_places=2,
                          constraint_L = NA,
                          constraint_U = NA,
                          make_plot = TRUE,
                          write_file = TRUE,
                          fileName = "dataFile.csv") {
  # make two independent groups data ("t test data")
  # This is just a wrapper for grpData to make generating two-group
  # data a little easier. 
  # Example call:
  # my_df <-  twoIndGrpData(grpNames = c("female", "male"), grpMeans = c(110, 100),
  #                          grpSD = 15, daLen = 50, fileName = "sexDiffs.csv")
  
  # import needed packages
  library(tidyverse)  # for write_csv()
  
  # will call grpData() for just two groups
  source("grpData.R")
  
  # call grpData. 
  #makeplot is always false in the call as plotting is done locally
  df <- grpData(grpNames,  grpMeans, grpSD, daLen, decimal_places, 
                constraint.L = constraint_L, constraint.U = constraint_U,
                makeplot = FALSE, writeFile = write_file, fileName)
  
  if (make_plot) {
    #----- plot in order to check that these are the data we want -----
    daPlot <- ggplot(df, aes(x = group, y = value)) +
      geom_boxplot(notch = TRUE) +
      labs(x = "Group")
    print(daPlot)
  }
  
  # write data to file in current directory 
  if (write_file){
    write_csv(x = df, file = fileName)
  }
  
  return(df)
  
}