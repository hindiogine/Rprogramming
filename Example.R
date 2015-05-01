data <- read.csv("cities.csv")

## orderdata: output data.frame with the ordered rows
## order(): sort by default in decreasing order the values. In case of numbers
## from the smallest to the biggest, in case of characters from A to Z. This 
## function returns a vector of indexes with the ordered rows. 
## > order(data[,1])
## [1] 2 4 5 6 3 1
## data[order(),] subsets the data frame using the indexes above
sort_by_column <- function (data, column)
{
  orderdata <- data[order(data[,column]), ]
  return(orderdata)
}

sort_by_columns <- function (data, col1, col2)
{
  orderdata <- data[order(data[,col1],data[,col2]),]
  return(orderdata)
}

data <- read.csv("citiesNA.csv")

## Subsetting data by column, we get a factor:
## > class(data[,2])
## [1] "factor"
## One way to extract a vector from the factor is by subsetting it by its levels.
## levels(data[,2]) returns a vector of the levels:
## [1] "China" "UK"    "USA"
## levels(data[,2])[data[,2]] returns a vector with the content of [data[,2]]
## [1] "China" "China" "USA"   "USA"   "UK"    "UK"  
## data[,2] would return a factor, that for our purposes is harder to handle
## [1] China China USA   USA   UK    UK   
## Levels: China UK USA
## SuppressWarnings() stops the warning alerts from R. When we coerce a mixed list of numeric and character
## into a numeric vector, text becomes automatically NA, but it's a forced coercion and R sends a warning.
## This is the case of our column 3 and 4, where "Unknown" becomes NA.
## complete.cases() returns the indexes of the rows that don't have any NA. By subsetting the matrix by these
## indexes we get a data frame with only complete cases.
sort_by_column_NA <- function(data,column)
{
  for (i in 3:4)
  {
    data[,i] <- suppressWarnings(as.numeric(levels(data[,i])[data[,i]]))
  }
  orderdata <- data[order(data[,column]),]
  orderdata <- orderdata[complete.cases(orderdata),] 
  return(orderdata)
}

## grep function finds the character vector (e.g. "China") in the
## data$countries factor, and returns a vector of indexes. 
## > data$countries
## [1] China China USA   USA   UK    UK   
## Levels: China UK USA
## > grep("China",data$countries)
## [1] 1 2
## We then subset the main data frame, data, by these indexes
## > data [grep("China",data$countries),]
##     cities countries areakm2 populationk
## 1 Shanghai     China    2643       21766
## 2  Beijing     China    1368       21500
sort_country <- function (data, country, column)
{
  countrydata <- data[grep(country,data$countries),]
  orderdata <- countrydata[order(countrydata[,column]),]
  return (orderdata)
}

## argument decreasing = TRUE inverts the direction of the order. Numbers from biggest to smallest and 
## characters from Z to A. This is helpful when we consider rank#1 the biggest city.
## as.character () will return the vector with the name of the city. If we just return orderdata[rank,1] 
## we get a factor instead.
find_city_rank <- function(data,column,rank)
{
  orderdata <- data[order(decreasing = TRUE, data[ ,column]),]
  return(as.character(orderdata[rank,1]))
}

## nrow() returns the number of rows of a data frame. We use this function to determine the index of the last item.
find_last_city <- function(data,column)
{
  orderdata <- data[order(decreasing = TRUE, data[,column]),]
  return(as.character(orderdata[nrow(orderdata),1]))
}

rank_by_country <- function(data, column, rank)
{
  ## We save the levels of column 2, the countries' names, in the countries vector
  countries <- levels(data[ , 2])
  
  ## We generate an empty vector that we will fill later, row by row, to generate our final output
  output <- vector()
  
  ## For loop to get the right data on each city. length(countries) is the number of different countries in our
  ## database. In our case we have 3 countries: China, UK, USA.
  for (i in 1:length(countries))
  {
    ## countrydata subsets data by the considered country
    countrydata <- data[grep(countries[i], data$countries), ]
    orderdata   <- countrydata[order(decreasing = TRUE, countrydata[,column]), ]
    
    ## append() adds elements at the end of a vector. We want to add the name of the city [rank,1],
    ## the areakm2 [rank,2] and the populationk [rank,3]. We don't add the name of the countries, because it
    ## will be the label of the rows.
    output <- append(output, as.character(orderdata[rank, 1]))
    for (l in 3:4)
    {
      output <- append(output, as.character(orderdata[rank, l]))
    }
  }
  
  ## Just because it's simpler to generate a matrix rather than a data frame, I generate it first and convert it
  ## to data frame immediatly after. 
  output <- as.data.frame(matrix(output,length(countries), 3, byrow = TRUE))
  
  ## Name of the columns will be "cities", "areakm2" and "populationk". Name of the rows are the countries.
  colnames(output) <- c("cities","areakm2","populationk")
  rownames(output) <- countries
  return(output)
}
