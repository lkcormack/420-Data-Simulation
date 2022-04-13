regData <- function(xname = "x", yname = "y", 
                    xRng = c(0, 100), params = c(0, 1), 
                    daLen = 30, yErr = 1, 
                    fileName = "dataFile.csv") {
  # make data set with a given slope, y intercept (params) 
  # and a y noise level (yErr) over a range on x
  # example call:
  # myData <- regData("myx", "myy", c(-50, 50), c(1, 3), 50, 5, "myData.csv")
  
  # note: must have the tidyverse loaded: library(tidyverse)
  
  # generate the un-normalized data
  x = rnorm(daLen)                         # random x

  # re-scale x to desired range 
  mx <- xRng[2]
  mn <- xRng[1]
  x = x - min(x);
  x = x/max(x);
  x = x * (mx-mn)+mn;
  
  # compute y
  y = params[1] + params[2]*x + yErr*rnorm(daLen)
  
  # make the data frame
  df <- data.frame(x, y)
  colnames(df) <- c(xname, yname)
  
  # ----- plot to check the data -----
  daPlot <- ggplot(df, aes_string(x = xname, y = yname)) +
    geom_point() + 
    stat_smooth(method = "lm") +
    labs(x = xname, y = yname)
  print(daPlot)
  
  # look at the correlation for grins
  print(cor(df))
  
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
