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
  setwd("/Users/hindiogine/Code/DataScienceCoursera")
  setwd(paste(getwd(), "/", directory, sep=""))
  file.list <- dir()
  pollution.data <- data.frame()
  
  for (i in id)
  {
    temp <- read.csv(file = file.list[i])
    pollution.data <- rbind(pollution.data, temp)
    DF.1 <- na.omit(pollution.data)
  }
  DF.2 <- table(DF.1$ID)
  DF.3 <- as.data.frame(DF.2)
  names(DF.3) <- c("id","nobs")
  setwd("/Users/hindiogine/Code/DataScienceCoursera")
  DF.3
}