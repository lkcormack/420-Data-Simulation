twoIndGrpData <- function(grpNames = c("a", "b"), 
                          grpMeans = c(40, 50), 
                          grpSD = 10, 
                          daLen = 42,
                          decimal_places=2,
                          make_plot = TRUE,
                          write_file = TRUE,
                          fileName = "dataFile.csv") {
  
  # import needed packages
  library(tidyverse)  # for write_csv()
  
  # will call grpData() for just two groups
  source("grpData.R")
  
  # call grpData. 
  #makeplot is always false in the call as plotting is done locally
  df <- grpData(grpNames,  grpMeans, grpSD, daLen, decimal_places, 
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