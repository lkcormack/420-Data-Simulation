## function for combining data frames separated by factors

combine <- function(dfs = list(), 
                    factors=c("male","female"), 
                    new.factor="sex",
                    write_file = TRUE,
                    fileName = "dataFile.csv"){
  #stack the dfs
  N <- length(factors) #number of data frames to stack
  df <- dfs[[1]] #start with the first one
  for(i in 2:N){
    df <- rbind(df,dfs[[i]]) #rbind each one sequentially
  }
  colnames(df) <- colnames(dfs[[1]])
  
  #Make a vector of factors for each dataset
  D <- ncol(dfs[[1]])
  facts <- list() #set up a list
  for(i in 1:N){ #fill in list with rep'd factors for each grou
    facts[[i]] <- rep(factors[i],length(dfs[[i]][,1])) 
  }
  facts <- unlist(facts) #convert to a big vector 
  
  #make a new column and name it
  df <- cbind(df,facts)
  colnames(df)[D+1] <- new.factor
  
  # write data to file in current directory 
  if (write_file){
    write_csv(x = df, file = fileName)
  }
  
  return(df)
  
}