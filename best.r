best <- function(state, outcome){
  ##read outcome data
  careMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##check state is valid
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
 else {print("continue")}
  ##return the hospital name in that state with lowest 30-day death rate
  
}
