best <- function(state, outcome){
  ##read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##check state is valid
    #create list of unique existing state names
    uniqueST <- unique(outcome[,7])
    uniqCheck <- as.numeric(0)
    #check against this list with loop. This is where I left off
    for (i in seq_along(uniqueST)){
      if(uniqueST[i] == state){uniqCheck <- uniqCheck+1}
    }
    if (uniqCheck == 0) print("The state abbreviation is invalid")
  ##check outcome is valid
  if (outcome != "heart attack" || "heart failure" || "pneumonia"){
    print("The outcome is invalid")
  }
  ##return the hospital name in that state with lowest 30-day death rate
    
}
