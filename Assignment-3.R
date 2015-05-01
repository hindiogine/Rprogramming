# Assignment no. 3 - R Programming

best <- function(state, outcome)
{
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  hosp.data <- na.omit(temp[,c(2,7,11,17,23)])
  names(hosp.data) <- c("Name","State","Heart.attack","Heart.failure","Pneumonia")
  hosp.data$Heart.attack  <- as.numeric(hosp.data$Heart.attack)
  hosp.data$Heart.failure <- as.numeric(hosp.data$Heart.failure)
  hosp.data$Pneumonia     <- as.numeric(hosp.data$Pneumonia)
  states <- unique(hosp.data$State)
  
  ## Check that state and outcome are valid
  temp <- grepl(state, states)
  if(sum(temp)!=1) stop("invalid state")
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  temp <- grepl(outcome, valid.outcomes)
  if(sum(temp)!=1) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  if(outcome == "heart attack") outcome.column <- 3
  if(outcome == "heart failure") outcome.column <- 4
  if(outcome == "pneumonia") outcome.column <- 5
  
  ## rate of "heart attack", "heart failure", "pneumonia".
  hosp.data <- hosp.data[hosp.data$State == state, c(1, outcome.column)]
  best.hosp.rows <- which(hosp.data[,2] == min(hosp.data[,2]))
  best.hosp <- hosp.data[best.hosp.rows, 1]
  best.hosp <- best.hosp[order(best.hosp$Name), ]
  best.hosp[1, 1]
}

rankhospital <- function(state, outcome, num = "best")
{
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  hosp.data <- na.omit(temp[,c(2,7,11,17,23)])
  names(hosp.data) <- c("Name","State","Heart.attack","Heart.failure","Pneumonia")
  hosp.data$Heart.attack  <- as.numeric(hosp.data$Heart.attack)
  hosp.data$Heart.failure <- as.numeric(hosp.data$Heart.failure)
  hosp.data$Pneumonia     <- as.numeric(hosp.data$Pneumonia)
  states <- unique(hosp.data$State)
  
  ## Check that state and outcome are valid
  temp <- grepl(state, states)
  if(sum(temp)!=1) stop("invalid state")
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  temp <- grepl(outcome, valid.outcomes)
  if(sum(temp)!=1) stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(outcome == "heart attack") outcome.column <- 3
  if(outcome == "heart failure") outcome.column <- 4
  if(outcome == "pneumonia") outcome.column <- 5
  
  hosp.data <- hosp.data[hosp.data$State == state, c(1, outcome.column)]  
  hosp.data <- hosp.data[order(hosp.data[,2]), ]  
  
  rank <- num
  if(num == "best") { rank <- 1 }
  if(num == "worst") { rank <- nrow(hosp.data) }
  ranked.hosp <- hosp.data[order(hosp.data[,1]), ]
  ranked.hosp[rank, 1]
}

rankall <- function(outcome, num = "best")
{
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  hosp.data <- na.omit(temp[,c(2,7,11,17,23)])
  names(hosp.data) <- c("Name","State","Heart.attack","Heart.failure","Pneumonia")
  hosp.data$State         <- factor(hosp.data$State)
  hosp.data$Heart.attack  <- as.numeric(hosp.data$Heart.attack)
  hosp.data$Heart.failure <- as.numeric(hosp.data$Heart.failure)
  hosp.data$Pneumonia     <- as.numeric(hosp.data$Pneumonia)
  states <- levels(hosp.data$State)
  
  ## Check that state and outcome are valid
  # temp <- grepl(state, states)
  # if(sum(temp) != 1) stop("invalid state")
  valid.outcomes <- c("heart attack","heart failure","pneumonia")
  temp <- grepl(outcome, valid.outcomes)
  if(sum(temp) != 1) stop("invalid outcome")  
  
  if(outcome == "heart attack") outcome.column <- 3
  if(outcome == "heart failure") outcome.column <- 4
  if(outcome == "pneumonia") outcome.column <- 5  

  hosp.data <- hosp.data[ , c(1,2, outcome.column)]
  names(hosp.data) <- c("Name","State","Outcome")
  hosp.data <- hosp.data[order(hosp.data$State, hosp.data$Outcome), ]
  num.states <- length(states)

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name.
  
  ## We generate an empty vector that we will fill later, row by row, to generate our final output
  output <- data.frame(Name=NA, State=NA)
  
  ## For loop to get the right data on each city. length(countries) is the number of different countries in our
  ## database. In our case we have 3 countries: China, UK, USA.
  for (i in 1:num.states)
  {
    ## countrydata subsets data by the considered country
    hospital.data <- hosp.data[grep(states[i], hosp.data$State), ]
    # orderdata   <- countrydata[order(decreasing = TRUE, countrydata[,column]), ]
    
    ## append() adds elements at the end of a vector. We want to add the name of the city [rank,1],
    ## the areakm2 [rank,2] and the populationk [rank,3]. We don't add the name of the countries, because it
    ## will be the label of the rows.
    output <- rbind(output, hospital.data[num, c(1,2)])
    # for (j in 3:4)
    # {
    #   output <- append(output, as.character(orderdata[rank, j]))
    # }
  }
  
  ## Just because it's simpler to generate a matrix rather than a data frame, I generate it first and convert it
  ## to data frame immediatly after.
  
  output <- na.omit(output)
  return(output)
  # output.df <- as.data.frame(matrix(output, num.states, 2, byrow = TRUE))
  
  ## Name of the columns will be "cities", "areakm2" and "populationk". Name of the rows are the countries.
  # colnames(output.df) <- c("Name","State","Outcome")
  # rownames(output.df) <- states
  # return(output.df)
}