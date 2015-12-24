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

  #return the hospital name in that state with lowest 30-day death rate
  
  ##first, create subset for state
  state_subset <- careMeasures[,7] == state
  ##create subset to drop all NAs
  if(outcome=="heart attack"){na_subset <- is.na(as.numeric(careMeasures[,11]))}
  if(outcome=="heart failure"){na_subset <- is.na(as.numeric(careMeasures[,17]))}
  if(outcome=="pneumonia"){na_subset <- is.na(as.numeric(careMeasures[,23]))}
  #combine subsets and apply to dataframe
  master_subset <- state_subset*!na_subset
  subCareM <- careM[master_subset]
  ##get location of lowest value
  if(outcome=="heart attack"){min_location <- which.min(subCareM[,11])}
  if(outcome=="heart failure"){min_location <- which.min(subCareM[,17])}
  if(outcome=="pneumonia"){min_location <- which.min(subCareM[,23])}
  hospName <- subCareM[,2]
  hospName[min_location]
}
