best <- function(state, outcome){
  ##read outcome data
  careMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #create vector of unique existing state names
  uniqueST <- unique(careMeasures[,7])
  uniqCheck <- as.numeric(0)
  #create vector of valid outcomes
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  validCheck <- as.numeric(0)
  
  #check for valid state
  for (i in seq_along(uniqueST)){
    if(uniqueST[i] == state){uniqCheck <- uniqCheck+1}
  }
  if (uniqCheck == 0) stop(print("invalid state"))
  
  ##check outcome is valid
  validCheck <- sum(outcome == validOutcome)
  if (validCheck == 0) stop(print("invalid outcome"))

  #return the hospital name in that state with lowest 30-day death rate
  
  ##first, create subset for state
  state_subset <- careMeasures[,7] == state
  ##create subset to drop all NAs
  if(outcome=="heart attack"){na_subset <- !is.na(as.numeric(careMeasures[,11]))}
  if(outcome=="heart failure"){na_subset <- !is.na(as.numeric(careMeasures[,17]))}
  if(outcome=="pneumonia"){na_subset <- !is.na(as.numeric(careMeasures[,23]))}
  #combine subsets 
  master_subset <- state_subset*na_subset
  #create mortality rate object
  if(outcome=="heart attack"){mortRate <- (as.numeric(careMeasures[,11]))}
  if(outcome=="heart failure"){mortRate <- (as.numeric(careMeasures[,17]))}
  if(outcome=="pneumonia"){mortRate <- (as.numeric(careMeasures[,23]))}
  #filter mortality rate object
  filtered_mortRate <- master_subset*mortRate
  #get rid of zero values
  filtered_mortRate[filtered_mortRate == 0] <- NA
  #get minimum value
  minMR <- min(filtered_mortRate, na.rm = TRUE)
  hospital_name <- subset(careMeasures[,2], filtered_mortRate <= minMR)
  
  
}
