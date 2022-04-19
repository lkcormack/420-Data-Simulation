regDataSeqX <- function(xname = "x", yname = "y", 
                    xRng = c(0, 100), params = c(0, 1), 
                    daLen = 30, yErr = 1,
                    yPerX = 5, withinS = FALSE,
                    x_dec_plcs = 2, y_dec_plcs = 2,
                    fileName = "dataFile.csv") {
  # related data set with a given slope and y int with a sequential x axis
  # example call:
  # myData <- regDataSeqX("myx", "myy", 
  #                       c(-50, 50), c(0, 0.5), 
  #                       8, 25, 
  #                       10, withinS = TRUE, 
  #                       fileName = "myDataFile.csv")
  #
  # Similar to regData() except it makes a regularly spaced x-axis with 
  # yPerX observations at each value of x.
  # The "withinS = TRUE" is currently just for cosmetic purposes as the "subjects"
  # behave identically except for noise
  
  # import needed packages
  library(tidyverse)
  
  # generate the unique x values
  xVals = seq(-1, 1, length = daLen)
  
  # make instances of each x value for every y value so we have a "tidy" data frame
  # in which each row corresponds to a single observation (i.e. an x,y pair).
  yPerXVec = rep(yPerX, daLen) # how many y's we want at each unique x
  x = rep(xVals, yPerXVec)
  
  # re-scale x to desired range
  mx <- xRng[2]
  mn <- xRng[1]
  x = x - min(x);
  x = x/max(x);
  x = x * (mx-mn)+mn;

  # compute y
  y = params[1] + params[2]*x + yErr*rnorm(daLen)
  
  # round the data
  x = round(x,  x_dec_plcs)
  y = round(y,  y_dec_plcs)
  
  # if within-subjects, make a factor of subject IDs
  if (withinS) {
    # generate the factor
    prefix <- "s"
    suffix <- seq(1:yPerX)
    uniqNames <- paste(prefix, suffix, sep = "")
    sIds <- rep(uniqNames, daLen)
  }
  
  # make the data frame
  if (withinS) {
    df <- data.frame(x, y, sIds)
    idName = "IDs"
    colnames(df) <- c(xname, yname, idName)
  } else {
    df <- data.frame(x, y)
    colnames(df) <- c(xname, yname)
  }
   
  
  # ----- plot to check the data -----
  if (withinS) {
    daPlot <- ggplot(df, aes_string(x = xname, y = yname, color = idName)) +
      geom_point() + 
      stat_smooth(method = "lm") +
      labs(x = xname, y = yname)
  } else {
    daPlot <- ggplot(df, aes_string(x = xname, y = yname)) +
      geom_point() + 
      stat_smooth(method = "lm") +
      labs(x = xname, y = yname)
  }
  print(daPlot)
  
  # look at the correlation for grins
  print(cor(df[,1], df[,2]))
   
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
