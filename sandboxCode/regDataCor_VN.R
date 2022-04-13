

library(tidyverse)
library(reshape2) #melt function
library(forestmangr)#round function

regDataCor <- function(xname = "Closeness_Score", yname = "Loneliness_Score", 
                    xRng = c(0, 7), yRng = c(0, 80), 
                    daLen = 100, r = -0.66,
                    decimal_places=2,
                    fileName = "Example.csv") {
  # make a correlated data set
  # example call:
  # myData <- regDataCor("myx", "myy", c(-50, 50), c(0, 50), 50, 0.4, "myData.csv")
  
  # note: must have the tidyverse loaded: library(tidyverse)
  
  
  #decimal_places does what it says 0 --> whole numbers (useful for likert data)
  
  
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
  
  # make the data frame
  df_1 <- data.frame(x, y)
  colnames(df_1) <- c(xname, yname)
  
  #rounding 
  df<-round_df(df_1, digits = decimal_places)
  
  # ----- plot to check the data -----
  daPlot <- ggplot(df, aes_string(x = xname, y = yname)) +
    geom_point() + 
    stat_smooth(method = "lm") +
    labs(x = xname, y = yname)
  print(daPlot)
 
  # check the correlation (it will NOT match the input correlation exactly)
  print(cor(df))
  
  #add variable 
  #delete '#' in front of line to add additional column useful for factorial ANOVA designs 
  factor <- "Male"
  #df$new_col_name <- c(rep(factor,daLen))
  
  
  
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}


regDataCor()


#merge 2 data frames
# this part is set up to merege 2 data frames that have already been written to csv
# useful for factorial ANOVA designs where you want to make sure that there is an effect of both factors 

df1<- read.csv("Meyer_nonLGBT.csv")
df2<- read.csv("Meyer_LGBT.csv")

df3 <- rbind(df1, df2)
colnames(df3)[colnames(df3) == "gender"] <- "Identity"

write_csv(x = df3, file = "Meyer.csv")


