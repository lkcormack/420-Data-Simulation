


library(tidyverse)
library(reshape2) #melt function
library(forestmangr)#round function


regData <- function(xname = "Closeness_Score", yname = "Loneliness_Score", 
                    xRng = c(0, 7), params = c(55, -0.71), 
                    daLen = 100, yErr = 2.5, 
                    decimal_places=2,
                    fileName = "Example.csv") {
  # make data set with a given slope, y intercept (params) 
  # and a y noise level (yErr) over a range on x
  # example call:
  # myData <- regData("myx", "myy", c(-50, 50), c(1, 3), 50, 5, "myData.csv")
  
  # note: must have the tidyverse loaded: library(tidyverse)
  
  #decimal_places does what it says 0 --> whole numbers (useful for likert data)
  
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
  
  # look at the correlation for grins
  print(cor(df))
  
  # write data to file in current directory 
  write_csv(x = df, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
}


#add variable 
#delete '#' in front of line to add additional column useful for factorial ANOVA designs
factor <- "Male"
#df$new_col_name <- c(rep(factor,daLen))

# regData()


#merge 2 data frames
# this part is set up to merege 2 data frames that have already been written to csv
# useful for factorial ANOVA designs

df1<- read.csv("Zapalac_Twin.csv")
df2<- read.csv("Zapalac_Sibling.csv")

df3 <- rbind(df1, df2)

colnames(df3)[colnames(df3) == "gender"] <- "Identity"

write_csv(x = df3, file = "Zapalac.csv")
