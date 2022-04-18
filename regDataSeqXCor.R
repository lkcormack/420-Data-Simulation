regDataSeqXCor <- function(xname = "x", yname = "y", 
                    xRng = c(0, 100), yRng = c(0, 100), 
                    daLen = 10, yPerX = 5, r = 0.7,
                    withinS = TRUE,
                    x_dec_plcs = 2, y_dec_plcs = 2,
                    fileName = "dataFile.csv") {
  # make a correlated data set
  # example call:
  # myData <- regDataSeqXCor("myx", "myy", c(-50, 50), c(0, 50), 8, 5, 0.7, 
  # withinS = TRUE, "myDataFile.csv")
  # Similar to regDataCor() except it makes a regularly spaced x-axis with 
  # yPerX observations at each value of x.
  # The "withinS = TRUE" when there is any grouping variable (e.g. rat strain)
  
  # import needed packages
  library(tidyverse)
  
  # generate the unique x values
  xVals = seq(-1, 1, length = daLen)
  
  # make instances of each x value for every y value so we have a "tidy" data frame
  # in which each row corresponds to a single observation (i.e. an x,y pair).
  yPerXVec = rep(yPerX, daLen) # how many y's we want at each unique x
  x = rep(xVals, yPerXVec)
  
  # now make the correlated y-values
  y = (r)*x + (sqrt(1-r^2))*rnorm(length(x))   # correlated y
  
  # rescale both x and y to desired range 
  # rescale x
  mx <- xRng[2]
  mn <- xRng[1]
  x = x - min(x);
  x = x/max(x);
  x = x * (mx-mn)+mn;
  
  # rescale y
  mx <- yRng[2]
  mn <- yRng[1]
  y = y - min(y);
  y = y/max(y);
  y = y * (mx-mn)+mn;
  
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
  
  # check the correlation (it will NOT match the input correlation exactly)
  print(cor(df[,1], df[,2]))
   
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
