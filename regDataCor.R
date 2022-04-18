regDataCor <- function(xname = "x", yname = "y", 
                    xRng = c(0, 100), yRng = c(0, 100), 
                    daLen = 30, r = 0.5, 
                    x_dec_plcs = 2, y_dec_plcs = 2,
                    fileName = "dataFile.csv") {
  # make a correlated data set
  # example call:
  # myData <- regDataCor("myx", "myy", c(-50, 50), c(0, 50), 50, 0.4, "myData.csv")
  
  # import needed packages
  library(tidyverse)
  
  # generate the un-normalized data
  x = rnorm(daLen)                         # random x
  y = (r)*x + (sqrt(1-r^2))*rnorm(daLen)   # correlated y
  
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
  
  # make the data frame
  df <- data.frame(x, y)
  colnames(df) <- c(xname, yname)
  
  # ----- plot to check the data -----
  daPlot <- ggplot(df, aes_string(x = xname, y = yname)) +
    geom_point() + 
    stat_smooth(method = "lm") +
    labs(x = xname, y = yname)
  print(daPlot)
 
  # check the correlation (it will NOT match the input correlation exactly)
  print(cor(df))
  
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}
