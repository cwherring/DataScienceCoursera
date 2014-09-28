best <- function(state, myoutcome) {
    source("prog3-common.R")
    
    ## Read outcome data
    outcomes <<- getOutcomeData("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    if (!validState(state)) stop("invalid state")
    if (!validOutcome(myoutcome)) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death ## rate
    if (myoutcome == "heart attack") {
        bestHA(state)
    }else if (myoutcome == "heart failure") {
        bestHF(state)
    }else if (myoutcome == "pneumonia") {
        bestPN(state)
    }else stop("invalid outcome")
}

