pollutantmean <- function(directory, pollutant, id = 1:332) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  setwd(directory)
  file.list <- dir()
  pollution.data <- data.frame()
  for (i in id) 
  { 
    temp <- read.csv(file=file.list[i])
    pollution.data <- rbind(pollution.data, temp)
  }
  if(pollutant=="sulfate") {
    pollutant.mean <- mean(pollution.data$sulfate, na.rm=TRUE)
  } else if(pollutant=="nitrate")
  {
    pollutant.mean <- mean(pollution.data$nitrate, na.rm=TRUE)
  } else {print("incorrect input")}  
  print(pollutant.mean)
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  setwd(paste(getwd(), "/", directory, sep=""))
  file.list <- dir()
  pollution.data <- data.frame()
  
  for (i in id)
  {
    temp <- read.csv(file = file.list[i])
    pollution.data <- rbind(pollution.data, temp)
    DF.1 <- na.omit(pollution.data)
    print(dim(DF.1))
  }
  DF.2 <- table(DF.1$ID)
  DF.3 <- as.data.frame(DF.2)
  names(DF.3) <- c("id","nobs")
  print(DF.3)
  setwd("~/Code/DataScienceCoursera")
}
