##Paired t-test function
pairedT <- function(grpNames = c("a", "b"), 
                    grpMeans = c(40, 50), 
                    grpSDs = c(10, 10), 
                    daLen = 30,
                    r = .5,
                    decimal_places=2,
                    constraint.L = NA,
                    constraint.U = NA,
                    make_plot = TRUE,
                    write_file = TRUE,
                    fileName = "dataFile.csv"){
  
  # fill out SD vector if necessary
  if (length(grpSDs) == 1){
    grpSDs <- rep(grpSDs, 2)
  }
  
  
  # create covariance matrix 
  Sigma <- matrix(c(grpSDs[1]^2, r * prod(grpSDs),
                    r * prod(grpSDs), grpSDs[2]^2), 
                  nrow=2)
  
  # generate some correlated data
  df <- as.data.frame(
    mvrnorm(daLen, grpMeans, Sigma)
                      )
  
  # round the data
  df = round(df, decimal_places)
  
  # Add col names
  colnames(df) <- grpNames
  
  # Is there a constraint on the data?
  
  # Lower constraint: add to anything < constraint 
  # 1.2*delta, where delta = constraint - value
  if(is.na(constraint.L)==F){
    L.prblms <- list()
    L.prblms[[1]] <- which(df[,1]<constraint.L)
    L.prblms[[2]] <- which(df[,2]<constraint.L)
    df[L.prblms[[1]],1] <- 
      df[L.prblms[[1]],1] + 1.2*(constraint.L - df[L.prblms[[1]],1])
    df[L.prblms[[2]],2] <- 
      df[L.prblms[[2]],2] + 1.2*(constraint.L - df[L.prblms[[2]],2])
  }
  
  #Same deal for upper constraint, but reverse
  if(is.na(constraint.U)==F){
    U.prblms <- list()
    U.prblms[[1]] <- which(df[,1]>constraint.U)
    U.prblms[[2]] <- which(df[,2]>constraint.U)
    df[U.prblms[[1]],1] <- 
      df[U.prblms[[1]],1] + 1.2*(constraint.U - df[U.prblms[[1]],1])
    df[U.prblms[[2]],2] <- 
      df[U.prblms[[2]],2] + 1.2*(constraint.U - df[U.prblms[[2]],2])
  }
  
  
  # write data to file in current directory 
  if (write_file){
    write_csv(x = df, file = fileName)
  }
  
  # return the data frame just in case you want to do anything else with it
  return(df)
  
  
}