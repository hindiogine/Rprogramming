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