corr <- function(directory, threshold = 0)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  setwd("/Users/hindiogine/Code/DataScienceCoursera")
  setwd(paste(getwd(), "/", directory, sep=""))
  file.list <- dir()
  
  comp.v <- vector(mode="numeric")
  corr.v <- vector(mode="numeric")
  
  for (i in 1:length(file.list)) 
  { 
    temp.1 <- read.csv(file=file.list[i])
    temp.2 <- na.omit(temp.1)
    comp.v[i] <- dim(temp.2)[1]
    if(comp.v[i] > threshold)
    {
      temp.3 <- cor(temp.2$sulfate, temp.2$nitrate, method="pearson")
      corr.v <- append(corr.v, temp.3)
    }
  }
  setwd("/Users/hindiogine/Code/DataScienceCoursera")
  threshold <- 0
  corr.v
}






