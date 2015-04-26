# Assignment no. 3 - R Programming

best <- function(state, outcome)
{
  ## Read outcome data
  temp <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  hosp.data <- temp[,c(2,7,11,17,23)]
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
  min(hosp.data[,2], na.rm = TRUE)
  
  best.hosp.row <- which(hosp.data[,2] == min(hosp.data[,2], na.rm = TRUE))
  
  hosp.data[best.hosp.row, 1]
}

rankhospital <- function(state, outcome, num = "best")
{
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}

rankall <- function(outcome, num = "best")
{
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}