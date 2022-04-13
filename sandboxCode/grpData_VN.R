

library(tidyverse)
library(reshape2) #melt function
library(forestmangr)#round function

#library(tidyr) forget what function this was for 


grpData <- function(grpNames = c("Religious", "non-Religious"), 
                    long_colnames= c("Upbringing", "Shame_Score"),
                    grpMeans = c(4.2, 5.3), 
                    grpSDs = c(1.1, 0.9), 
                    daLen = 10,
                    decimal_places=2,
                    repeated_measures= TRUE,
                    fileName = "example.csv") {
  # make a grouped data set
  # the length of the first 2 arguments determines the number of groups
  # (these lengths must be the same)
  # entering a single number for gprSDs uses that SD for all groups
  # example call:
  # myData <- grpData(c("alcohol", "thc", "cocaine", "control"), 
  #                   c(50, 40, 70, 60), 5, 42, "MyGrpData.csv")
  
  # note, must have the tidyverse loaded: library(tidyverse)
  
  #VN Additions: 
  #long_columns= (IV, DV) group names from previous argument are melted together into a 
  # single column (needed for means and CIs plots)
  #decimal_places does what it says 0 --> whole numbers (useful for likert data)
  #
  
  
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
  df_1 <- data.frame(dataMat)
  colnames(df_1) <- grpNames
  
  #rounding 
  df<-round_df(df_1, digits = decimal_places)
 
  
  #----- plot in order to check that these are the data we want -----
  daPlot <- ggplot(stack(df), aes(x = ind, y = values)) +
    geom_boxplot(notch = TRUE) +
    labs(x = "Group")
  print(daPlot)
  
  
  #convert to long form (means plot)
  df_long<-melt(df)
  colnames(df_long)<- long_colnames
  
  #add subject ID 
  if (repeated_measures == TRUE) {
    df_long$subject <- c(rep(1:daLen,nGrps))
  }
  if (repeated_measures == FALSE) {
    df_long$subject <- c(rep(1:(daLen*nGrps)))
  }
  
  
  
  #add variable 
  #delete '#' in front of line to add additional column useful for factorial ANOVA designs 

    factor <- "Female"
    df_long$new_col_name <- c(rep(factor,daLen*2)) 
    
    
 
  # write data to file in current directory 
  write_csv(x = df_long, file = fileName)
  
  # return the data frame just in case you want to do anything else with it
  return(df_long)
  
}

grpData()






#merge 2 data frames
# this part is set up to merege 2 data frames that have already been written to csv
# useful for factorial ANOVA designs

df1<- read.csv("example_Male.csv")
df2<- read.csv("example_Female.csv")

df3 <- rbind(df1, df2)

write_csv(x = df3, file = "Example.csv")












#generating data based on distribution shape 

Control <- ceiling( runif(1000 ,min = 0, max = 10)  )
Depressed <- ceiling( rnorm(n=1000 ,mean=5, sd=1.7)  )

df_short <- data.frame(Control, Depressed)

df <- melt(df_short)

colnames(df)<- c("Group", "Favorite_Genre")

ggplot()+geom_freqpoly(data=df, binwidth=1, aes(Favorite_Genre, color=Group))


df$Favorite_Genre<-factor(df$Favorite_Genre, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
       labels = c("pop", "Indie_pop", "Indie_Rock", "Metal", 
                  "Goth", "Punk", "Experimental", "Rap", "Psychedelic", "Classical"))



write_csv(x = df, file = "Example.csv")
